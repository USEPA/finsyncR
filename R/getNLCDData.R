#' Generate NLCD Land-use/Land-cover data for each NAWQA and EPA site
#'
#' @param data Input dataset which needs to include columns for
#'  \code{"SiteNumber"} and \code{"CollectionYear"}.
#' @param scale Scale for LULC data to be generated. Must be \code{"Cat"} for
#'  catchment or \code{"Ws"} for watershed. See \code{Details} below for more
#'  information.
#' @param group logical. Should the land-use/land-cover data be grouped into broad
#'  categories? \code{TRUE} or \code{FALSE}. See \code{Details} below for more
#'  information.
#'
#' @return Function returns a site by year dataframe of NLCD land-use/land-cover
#'   data for either the catchment or watershed scale.
#'
#' @details Land-use/land-cover data is extracted from the USGS National Land Cover
#'   Database (NLCD). These LULC data are available in the following years:
#'   2001, 2004, 2006, 2008, 2011, 2013, 2016, and 2019. For instances of years in the
#'   input data that are not exact year matches from this list, years are
#'   temporally matched to the closest year with LULC data (e.g. a site sampled
#'   in 1995 will have LULC data from 2001). Note that for samples that fall at
#'   the midpoint of two NLCD years (e.g. 2005), the function defaults to the
#'   earlier year (e.g. a site sampled in 2005 will have LULC data from 2004).
#'
#'   Data can be extracted at the catchment (\code{"Cat"}) or watershed (\code{"Ws"})
#'   scales. Watershed is larger than catchment, and includes land that drains
#'   downstream of the sampling location. Catchment only includes land that is
#'   upstream of the sampling location (watershed).
#'
#'   LULC data can be grouped (\code{group = TRUE}) into five broad categories.
#'   These categories include "water" (grouping of all wetland and open water
#'   land covers), "urban" (grouping of all urban land covers), "forest" (grouping
#'   of deciduous, coniferous, and mixed forests), "open" (grouping of shrub,
#'   barren land, grassland, and hay/pasture), and "crop" (no grouping, just the
#'   crop landuse).
#'
#'   Function pings StreamCat API.
#'
#' @author Michael Mahon, Ryan Hill, Samantha Rumschlag
#'
#' @examples
#' \dontrun{
#' ## Code to generate the percent land-use/land-cover in the catchment for
#' ## site number "USGS-05276005" from 2007.
#'
#'   dat = data.frame(SiteNumber = "USGS-05276005",
#'                    CollectionYear = 2007)
#'
#'   getNLCDData(data=dat,
#'   scale = "Cat",
#'   group = FALSE
#'   }
#'
#' @export

getNLCDData <- function(data, scale = "Cat", group = FALSE){

  if(!(scale %in% c("Cat","Ws"))){
    stop('scale must be either "Cat" or "Ws".')
  }

  if(httr::POST("https://java.epa.gov/StreamCAT/metrics?")$status_code != 200){
    stop('Connection to StreamCat API cannot be established')
  }


  ##set up streamcat call for NLCD data
  nlcd <- c("PctMxFst", "PctOw", "PctShrb", "PctUrbHi", "PctUrbLo",
            "PctUrbMd", "PctUrbOp", "PctWdWet", "PctBl", "PctConif",
            "PctCrop", "PctDecid", "PctGrs", "PctHay", "PctHbWet",
            "PctIce")
  years <- c("2001", "2004", "2006", "2008", "2011",
            "2013", "2016", "2019")
  nlcd_mets = paste(paste(rep(nlcd, each = length(year)), year, sep = ""), collapse = ",")

  ##attach COMIDs to site-numbers
  data = dplyr::left_join(data, (StreamData:::.allsitesCOMID[,-3] %>% dplyr::filter(SiteNumber %in% data$SiteNumber)))

  comid = paste(data$COMID, collapse = ",")

  post_body = ""
  post_body <- paste0(post_body, "name=", nlcd_mets)
  post_body <- paste0(post_body, "&areaOfInterest=",
                      ifelse(scale == "Cat",
                             'catchment',
                             'watershed'))
  post_body <- paste0(post_body, "&comid=", comid)

  streamcat <- httr::content(httr::POST("https://java.epa.gov/StreamCAT/metrics?",
                                        body = post_body),
                             type = "text/csv", encoding = "UTF-8",
                             show_col_types = FALSE)


  ##Naming scheme
  #ItemYearScale

  ##Focus on only those that end in Cat (catchment) or Ws (watershed)
  cat_ws_cols <- colnames(streamcat)[stringr::str_sub(colnames(streamcat), -3) == "CAT" |
                                       stringr::str_sub(colnames(streamcat), -2) == "WS"]

  streamcat2 = streamcat %>%
    ##Select columns that are pertinent: site info, size of Ws/Cat, and above cols
    dplyr::select(COMID, CATAREASQKM, WSAREASQKM, tidyselect::all_of(cat_ws_cols)) %>%
    ##Pivot longer, so that non-site info columns are in "Info" and the values are
    ##in "value"; this will help with extraction of year and scale information
    ##In turn, this process will make it easier to join StreamCat data w/ biodata
    tidyr::pivot_longer(cols = tidyselect::ends_with("CAT") |
                   tidyselect::ends_with("WS") |
                   tidyselect::starts_with("CAT") |
                   tidyselect::starts_with("WS"),
                 names_to = "Info"
    ) %>%
    ##Get info on whether it is for the Ws or Cat, whether the column has specific
    ##values for each year (NLCD);
    ##Extract what the data is (Info2): i.e. PctUrb, PctCrop, etc.
    ##From this, provide broad groupings for NLCD data:
    ## Urb, Crop, and Hay are HumanDominated; everything else is "Natural"
    dplyr::mutate(Scale = ifelse(grepl("CAT", Info),
                          "Cat",
                          "Ws"),
           Y_spec = ifelse(grepl("20", Info),
                           "Y",
                           "N"),
           Year = ifelse(Scale == "Cat" & Y_spec == "Y",
                         stringr::str_sub(Info, -7, -4),
                         ifelse(Scale == "Ws" & Y_spec == "Y",
                                stringr::str_sub(Info, -6, -3),
                                NA
                         )),
           Info2 = ifelse(is.na(Year),
                          stringr::str_remove(Info, toupper(scale)),
                          stringr::str_remove(stringr::str_remove(Info, toupper(scale)), Year))
    ) %>%
    dplyr::filter(Scale %in% scale) %>%
    dplyr::filter(Info2 %in% toupper(c("PctBl", "PctConif", "PctCrop", "PctDecid", "PctGrs",
                        "PctHay", "PctHbWet", "PctMxFst", "PctOw", "PctShrb",
                        "PctUrbHi", "PctUrbLo", "PctUrbMd", "PctUrbOp",
                        "PctWdWet"))) %>%
    ##Remove columns we no longer need
    dplyr::select(-Y_spec, -Info)

  if(isTRUE(group)){
    streamcat2 <- streamcat2 %>%
      dplyr::mutate(Info2 = ifelse(grepl(toupper("PctUrb"),
                                   Info2),
                                   "PctUrb",
                             ifelse(grepl(paste(toupper(c("PctDecid","PctConif",
                                                  "PctMxFst")),
                                                collapse = "|"),
                                          Info2),
                                    "PctFst",
                                    ifelse(grepl(paste(toupper(c("PctGrs", "PctShrb",
                                                         "PctHay", "PctBl")),
                                                       collapse = "|"),
                                                 Info2),
                                           "PctOpn",
                                           ifelse(grepl(paste(toupper(c("PctOw", "PctWdWet",
                                                                "PctHbWet")),
                                                              collapse = "|"),
                                                        Info2),
                                                  "PctWater",
                                                  "PctCrop"))))
      ) %>%
      dplyr::group_by(COMID, SiteNumber, Scale, Year, Info2) %>%
      dplyr::summarize(value = sum(value)) %>%
      dplyr::ungroup()
  }

  ##Join Area of Ws and Cat with LULC data
  USGS_streamcat <- streamcat2 %>%
    tidyr::unite(InfoBroadScale, c("Info2", "Scale"), sep = "_", remove = T) %>%
    tidyr::pivot_wider(id_cols = c("COMID", "Year"),
                names_from = "InfoBroadScale",
                values_from = "value") %>%
    dplyr::mutate(Year = as.numeric(Year))

  ##Years are the NLCD years
  Years = c(2001,2004,2006,2008,2011,2013,2016,2019)

  ##Read in datasets

  ##Create a holder column to be used to match the datasets tmeporally
  data$ClosestYear = 0

  ##Generate closest LULC year
  for(i in 1:nrow(data)){
    data$ClosestYear[i] = as.numeric(Years[which.min(abs(Years-data$CollectionYear[i]))])
  }

  ##Join the datasets
  data = data %>%
    dplyr::left_join(USGS_streamcat,
              by = join_by("COMID" == "COMID",
                     "ClosestYear" == "Year")) %>%
    dplyr::select(-ClosestYear, -COMID)

  return(data)
}

