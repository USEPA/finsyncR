#' Generate NLCD Land-use/Land-cover data for each NAWQA site
#'
#' @param data Input dataset which needs to include columns for
#'  \code{"SiteNumber"} and \code{"CollectionYear"}.
#'  \code{"SiteNumber"} must have \code{"USGS-"} prefix.
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
#' @details Currently, this function only works for USGS NAWQA sites, but implementation
#'   for EPA NRSA sites will be completed in the near future. Land-use/land-cover
#'   data is extracted from the USGS National Land Cover Database (NLCD). These
#'   LULC data are available in the following years: 2001, 2004, 2006, 2008,
#'   2011, 2013, and 2016. For instances of years in the input data that are not
#'   exact year matches from this list, years are temporally matched to the closest
#'   year with LULC data (e.g. a site sampled in 1995 will have LULC data from 2001).
#'   Note that for samples that fall at the midpoint of two NLCD years (e.g. 2005),
#'   the function defaults to the earlier year (e.g. a site sampled in 2005 will
#'   have LULC data from 2004).
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
#' @examples
#' \dontrun{
#' ## Code to generate the percent land-use/land-cover in the catchment for
#' ## site number "05276005" from 2007.
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

  ##Read in NLCD data from streamcat dataset from Ryan Hill
  streamcat <- read.csv(base::system.file("extdata",
                                          "streamcat-usgs-nawqa-join.csv",
                                          package = "StreamData"))

  ##Naming scheme
  #ItemYearScale

  ##Focus on only those that end in Cat (catchment) or Ws (watershed)
  cat_ws_cols <- colnames(streamcat)[stringr::str_sub(colnames(streamcat), -3) == "Cat" |
                                       stringr::str_sub(colnames(streamcat), -2) == "Ws"]

  streamcat2 = streamcat %>%
    ##Select columns that are pertinent: site info, size of Ws/Cat, and above cols
    dplyr::select(COMID, SiteNumber, CatAreaSqKm, WsAreaSqKm, all_of(cat_ws_cols)) %>%
    ##Pivot longer, so that non-site info columns are in "Info" and the values are
    ##in "value"; this will help with extraction of year and scale information
    ##In turn, this process will make it easier to join StreamCat data w/ biodata
    dplyr::pivot_longer(cols = tidyselect::ends_with("Cat") |
                   tidyselect::ends_with("Ws") |
                   tidyselect::starts_with("Cat") |
                   tidyselect::starts_with("Ws"),
                 names_to = "Info"
    ) %>%
    ##Get info on whether it is for the Ws or Cat, whether the column has specific
    ##values for each year (NLCD);
    ##Extract what the data is (Info2): i.e. PctUrb, PctCrop, etc.
    ##From this, provide broad groupings for NLCD data:
    ## Urb, Crop, and Hay are HumanDominated; everything else is "Natural"
    dplyr::mutate(Scale = ifelse(grepl("Cat", Info),
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
                          stringr::str_remove(Info, scale),
                          stringr::str_remove(stringr::str_remove(Info, scale), Year)),
    ) %>%
    dplyr::filter(Scale %in% scale) %>%
    dplyr::filter(Info2 %in% c("PctBl", "PctConif", "PctCrop", "PctDecid", "PctGrs",
                        "PctHay", "PctHbWet", "PctMxFst", "PctOw", "PctShrb",
                        "PctUrbHi", "PctUrbLo", "PctUrbMd", "PctUrbOp",
                        "PctWdWet")) %>%
    ##Remove columns we no longer need
    dplyr::select(-Y_spec, -Info)

  if(isTRUE(group)){
    streamcat2 <- streamcat2 %>%
      dplyr::mutate(Info2 <- ifelse(grepl("PctUrb",
                                   Info2),
                             "PctUrb",
                             ifelse(grepl(paste(c("PctDec","PctCon",
                                                  "PctMx"),
                                                collapse = "|"),
                                          Info2),
                                    "PctFst",
                                    ifelse(grepl(paste(c("PctGrs", "PctShrb",
                                                         "PctHay", "PctBl"),
                                                       collapse = "|"),
                                                 Info2),
                                           "PctOpn",
                                           ifelse(grepl(paste(c("PctOw", "PctWdWet",
                                                                "PctHbWet"),
                                                              collapse = "|"),
                                                        Info2),
                                                  "PctOpn",
                                                  "PctCrop"))))
      )
  }

  ##Join Area of Ws and Cat with LULC data
  USGS_streamcat <- streamcat2 %>%
    tidyr::unite(InfoBroadScale, c("Info2", "Scale"), sep = "_", remove = T) %>%
    dplyr::pivot_wider(id_cols = c("COMID", "SiteNumber", "Year"),
                names_from = "InfoBroadScale",
                values_from = "value") %>%
    dplyr::mutate(Year = as.numeric(Year))

  ##Years are the NLCD years
  Years = c(2001,2004,2006,2008,2011,2013,2016)

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
              by = c("SiteNumber" = "SiteNumber",
                     "ClosestYear" = "Year")) %>%
    dplyr::select(-ClosestYear)

  return(data)
}
