#' Access clean USGS County Level Pesticide Estimates Dataset
#'
#' @param data Input dataset which needs to include columns for
#'  \code{"SiteNumber"} and \code{"CollectionYear"}.
#'  \code{"SiteNumber"} must have \code{"USGS-"} prefix.
#' @param ePest Pesticide use estimate type. Must be \code{"low"} or
#'  \code{"high"}.
#' @param lagTime numeric. The number of years over which the pesticide use
#'  estimates should be summarized. 0 returns pesticide use estimates in
#'  the given \code{"CollectionYear"}.
#' @param lagType Specifies whether to take a \code{"sum"} or a \code{"mean"}
#'  over years. Can only be used if \code{lagTime} is greater than 0.
#' @param pestLevel Specifies whether to summarize pesticide use estimates
#'  according to \code{"type"}, \code{"class"}, or \code{"compound"}.
#' @param pestLevelName Specifies the names of the pesticide types, classes, or
#'  individual compounds. See details for usage.
#'
#' @return Function returns a site by year dataframe of county-level pesticides use estimates
#'   in kilograms.
#'
#' @details Note: Pesticide classes and types taken from PAN Pesticide Database.
#'   There are 19 pesticides out of 483 that do not have a specific
#'   class or type assigned because they were not present in PAN Database.
#'   Some types were reassigned from PAN designations: plant growth regulators
#'   were designated as herbicides, and insect growth regulators were designated as
#'   insecticides. Fungicides were split into antibiotic fungicides, biologic fungicides,
#'   copper fungicides, mineral fungicides, and synthetic fungicides. To access the original PAN
#'   type designations see \code{Type1} in \code{StreamData:::.pest.info}.
#'
#'   Not every county has an estimate for each of the 483 pesticides in every
#'   year. We have set \code{na.rm = TRUE} when means and sums are calculated,
#'   so we inherently assume use is 0 when not provided. Seed coats including
#'   many neonicotinoids are not included in pesticide use estimates. Pesticide use
#'   estimates are based on agriculture alone.
#'
#'   See the following internal dataset for pesticide compounds, classes, and types
#'   \code{StreamData:::.pest.info}.
#'
#' @author Samantha Rumschlag, Michael Mahon
#'
#' @examples
#' \dontrun{
#' ## Code to generate the sum of the county-level use estimates (high estimate)
#' ## from site number "05276005" from 1997-1999 (2 year lag) for "insecticide",
#' ## "herbicide", and "synthetic_fungicide".
#'
#'   dat = data.frame(SiteNumber = "USGS-05276005",
#'                    CollectionYear = 1999)
#'
#'   getCountyPest(data=dat,
#'   ePest = "high",
#'   lagType = "sum",
#'   lagTime = 2,
#'   pestLevel = "type",
#'   pestLevelName = c("insecticide", "herbicide", "synthetic_fungicide"))
#'   }
#'
#' @export

getCountyPest <- function(data, ePest = "low", lagTime = 0, lagType,
                          pestLevel, pestLevelName){
  if (lagTime != 0 & !exists("lagType")){stop("Provide lagType as 'sum' (sum across years) or 'mean' (average across years) ")}

  pest.dat <- utils::read.table(base::unz(base::system.file("extdata",
                                                            "pestCountyEstYrs.zip",
                                                            package = "StreamData"),
                                          "pestCountyEstYrs.txt"),
                                sep = "\t", header= T,
                                colClasses = c("STATE_FIPS_CODE" = "character",
                                               "COUNTY_FIPS_CODE" = "character"))  %>%
    mutate(compound = stringr::str_to_lower(COMPOUND))

  ##Remove the unzipped file from the system
  if(file.exists(system.file("extdata",
                             "pestCountyEstYrs.txt",
                             package = "StreamData"))){
    unlink(system.file("extdata",
                       "pestCountyEstYrs.txt",
                       package = "StreamData"))}

  if(ePest == "low"){
    ePest = "EPEST_LOW_KG"
    dropC = "EPEST_HIGH_KG"
  }else{
    ePest = "EPEST_HIGH_KG"
    dropC = "EPEST_LOW_KG"
  }

  ##Make the data a dataframe, ran into some issues when it was a tibble and
  ##maintained grouping variables
  data = data.frame(data)

  #takes input data and links cnty and state FIPS
  site.dat <- data %>% dplyr::left_join(StreamData:::.site.info, by = "SiteNumber")

  if(lagTime !=0){
    site.dat.nrw = nrow(site.dat)
    site.dat = site.dat[rep(row.names(site.dat), each = lagTime + 1),]
    site.dat$LagYear = site.dat$CollectionYear
    site.dat$CollectionYear = site.dat$CollectionYear - rep(0:lagTime,
                                                            times = site.dat.nrw)
  }

  #filter pest.dat by...
  dat <- pest.dat %>%
    # COUNTY FIPS, STATE FIPS
    dplyr::filter(STATE_FIPS_CODE %in% site.dat$StateFIPSCode,
                  COUNTY_FIPS_CODE %in% site.dat$CountyFIPSCode,
                  # TIME (could include a LAG)
                  YEAR %in% site.dat$CollectionYear) %>%
    # LOW estimates
    dplyr::select(-tidyselect::any_of(dropC)) %>%
    # GROUP by pesticide types (e.g. insecticides, herbicides, fungicides)
    dplyr::left_join(StreamData:::.pest.info, by = c("compound" = "Name")) %>%
    dplyr::filter_at(vars(any_of(pestLevel)), any_vars(. %in% pestLevelName)) %>%
    # SUM (or average) by "insecticides", "herbicides", "fungicides" in each
    # County-Yr-GROUP combo
    dplyr::group_by(!!dplyr::sym(pestLevel), STATE_FIPS_CODE, COUNTY_FIPS_CODE, YEAR) %>%
    dplyr::summarize(sumPest = sum(!!dplyr::sym(ePest), na.rm = TRUE)) %>%
    dplyr::ungroup()

  dat.wide <- dat %>%
    tidyr::pivot_wider(names_from = !!dplyr::sym(pestLevel),
                       values_from = sumPest)

  site.dat.j <- site.dat %>%
    dplyr::left_join(dat.wide, by = c("CollectionYear" = "YEAR",
                                      "StateFIPSCode" = "STATE_FIPS_CODE",
                                      "CountyFIPSCode" = "COUNTY_FIPS_CODE"))

  if(lagTime !=0){
    if(lagType == "sum"){
      dat.end <- site.dat.j %>%
        dplyr::select(-CollectionYear, -StateFIPSCode, -CountyFIPSCode) %>%
        dplyr::group_by(SiteNumber, LagYear) %>%
        dplyr::summarize_all(list(sum), na.rm = TRUE) %>%
        dplyr::ungroup()
    }else{
      if(lagType == "mean"){
        dat.end <- site.dat.j %>%
          dplyr::select(-CollectionYear, -StateFIPSCode, -CountyFIPSCode) %>%
          dplyr::group_by(SiteNumber, LagYear) %>%
          dplyr::summarize_all(list(mean), na.rm = TRUE) %>%
          dplyr::ungroup()
      } else { if(!(lagType %in% c("sum", "mean"))){
        stop("Provide lagType as 'sum' (sum across years) or 'mean' (average across years) ")
      } }
    }
  } else {
    dat.end <- site.dat.j %>%
      dplyr::select(-StateFIPSCode, -CountyFIPSCode)
  }

  colnames(dat.end)[which(colnames(dat.end) == "LagYear")] = "CollectionYear"

  return(data.frame(dat.end))
}
