#' Generate NLCD Land-use/Land-cover data for each NAWQA site
#'
#' @param data Input dataset which needs to include columns for
#'  \code{"SiteNumber"} and \code{"CollectionYear"}.
#'  \code{"SiteNumber"} must have \code{"USGS-"} prefix.
#' @param scale Scale for LULC data to be generated. Must be \code{"cat"} for
#'  catchment or \code{"ws"} for watershed. See \code{Details} below for more
#'  information.
#' @param group logical. Should the land-use/land-cover data be grouped into broad
#'  categories? \code{TRUE} or \code{FALSE}. See \code{Details} below for more
#'  information.
#'
#' @return Function returns a site by year dataframe of NLCD land-use/land-cover
#'   data for either the catchment or watershed scale.
#'
#' @details To be completed.
#'
#' @examples
#' \dontrun{
#' ## Code to generate the sum of the county-level use estimates (high estimate)
#' ## from site number "05276005" from 1997-1999 (2 year lag) for "insecticide",
#' ## "herbicide", and "synthetic_fungicide".
#'
#'   dat = data.frame(SiteNumber = "USGS-05276005",
#'                    CollectionYear = 2007)
#'
#'   getNLCDData(data=dat,
#'   scale = "cat",
#'   group = FALSE
#'   }
#'
#' @export

getNLCDData <- function(data, scale = "cat", group = FALSE){

  ##Read in NLCD data from streamcat dataset from Ryan Hill
  streamcat <- read.csv("C:/Users/mikem/Documents/Research/USGS Stream Macros/MahonRumschlagPowell/streamcat-usgs-nawqa-join.csv")

  ##Naming scheme
  #ItemYearScale

  ##Focus on only those that end in Cat (catchment) or Ws (watershed)
  cat_ws_cols <- colnames(streamcat)[str_sub(colnames(streamcat), -3) == "Cat" |
                                       str_sub(colnames(streamcat), -2) == "Ws"]

  streamcat2 = streamcat %>%
    ##Select columns that are pertinent: site info, size of Ws/Cat, and above cols
    dplyr::select(COMID, SiteNumber, CatAreaSqKm, WsAreaSqKm, all_of(cat_ws_cols)) %>%
    ##Pivot longer, so that non-site info columns are in "Info" and the values are
    ##in "value"; this will help with extraction of year and scale information
    ##In turn, this process will make it easier to join StreamCat data w/ biodata
    pivot_longer(cols = ends_with("Cat") |
                   ends_with("Ws") |
                   starts_with("Cat") |
                   starts_with("Ws"),
                 names_to = "Info"
    ) %>%
    ##Get info on whether it is for the Ws or Cat, whether the column has specific
    ##values for each year (NLCD);
    ##Extract what the data is (Info2): i.e. PctUrb, PctCrop, etc.
    ##From this, provide broad groupings for NLCD data:
    ## Urb, Crop, and Hay are HumanDominated; everything else is "Natural"
    mutate(Scale = ifelse(grepl("Cat", Info),
                          "Cat",
                          "Ws"),
           Y_spec = ifelse(grepl("20", Info),
                           "Y",
                           "N"),
           Year = ifelse(Scale == "Cat" & Y_spec == "Y",
                         str_sub(Info, -7, -4),
                         ifelse(Scale == "Ws" & Y_spec == "Y",
                                str_sub(Info, -6, -3),
                                NA
                         )),
           Info2 = ifelse(is.na(Year),
                          str_remove(Info, Scale),
                          str_remove(str_remove(Info, Scale), Year)),
    ) %>%
    filter(Scale %in% scale) %>%
    filter(Info2 %in% c("PctBl", "PctConif", "PctCrop", "PctDecid", "PctGrs",
                        "PctHay", "PctHbWet", "PctMxFst", "PctOw", "PctShrb",
                        "PctUrbHi", "PctUrbLo", "PctUrbMd", "PctUrbOp",
                        "PctWdWet")) %>%
    ##Remove columns we no longer need
    select(-Y_spec, -Info)

  ##Join Area of Ws and Cat with LULC data
  USGS_streamcat2 <- USGS_streamcat %>%
    unite(InfoBroadScale, c("InfoBroad", "Scale"), sep = "_", remove = T) %>%
    pivot_wider(id_cols = c("COMID", "SiteNumber", "Year"),
                names_from = "InfoBroadScale",
                values_from = "Value")

  ##Years are the NLCD years
  Years = c(2001,2004,2006,2008,2011,2013,2016)

  ##Read in datasets

  ##Create a holder column to be used to match the datasets tmeporally
  data$ClosestYear = 0

  ##Generate closest LULC year
  for(i in 1:nrow(data)){
    data$ClosestYear[i] = Years[which.min(abs(Years-data$CollectionYear[i]))]
  }

  #CHECK THIS MIKE; REMOVE IT MAYBE?
  ##Add USGS- to site numbers to match the LULC data
  data$SiteNumber = paste("USGS-", data$SiteNumber, sep = "")

  ##Join the datasets
  data = data %>%
    left_join(USGS_streamcat2,
              by = c("SiteNumber" = "SiteNumber",
                     "ClosestYear" = "Year")) %>%
    select(-ClosestYear)

  return(data)
}
