#' Access clean USGS Fish Dataset
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"Superclass"}, \code{"Class"}, \code{"Subclass"}, \code{"Superorder"},
#'   \code{"Order"}, \code{"Superfamily"}, \code{"Family"}, \code{"Subfamily"},
#'   \code{"Genus"}, \code{"Species"}, or \code{"Subspecies"}.
#' @param program The program name(s) that should be included in the output
#'   dataset. See \code{Details} below for more information.
#' @param standardize logical. Should abundance be standardized by unit effort (see
#'   details)? \code{"TRUE"} or \code{"FALSE"}.
#'
#' @return A species by sample data frame with site, stream reach, and
#'   sample information.
#'
#' @details Note: To standardize fish abundance data, abundances were divided by
#'   the seconds shocked, number of seine hauls, etc. Then were divided by the
#'   stream length sampled. ~35% of the data have been removed,
#'   as these samples lacked the required standardization information.
#'   Therefore, if you are interested in occurrence (pres/abs) data only, then
#'   set \code{standardize = FALSE} and \code{dataType = "occur"}. Thus,
#'   be aware that setting \code{standardize = FALSE} will result in a larger
#'   dataset with additional samples/sites than when \code{standardize = TRUE}.
#'
#'   \code{program} refers to the Local, regional, or national program project
#'   for which data were originally collected. Because the National Water
#'   Quality Assessment (NAWQA) contains the most standardized sampling methods,
#'   we recommend using only the NAWQA dataset (set as default) for density and
#'   abundance measures. If you want to use all datasets, set
#'   \code{program = "ALL"}. Otherwise, \code{program} can be set to
#'   \code{"National Water Quality Assessment"},
#'   \code{"Cooperative Water Program"},
#'   \code{"Collection of Basic Records"},
#'   \code{"Other Federal Agencies"}, or a combination of these using \code{c()}
#'   (for example, \code{program = c("National Water Quality Assessment",
#'   "Cooperative Water Program")} for both NAWQA and Cooperative Water
#'   Programs).
#'
#'   The \code{getFishData()} function only outputs a community (taxa x site)
#'   matrix. We will include an extension of this function to return the
#'   individual-level information (length, width, deformities, etc.).
#'
#' @examples
#' \dontrun{
#' Fish <- getFishData(taxonLevel = "Family")
#'
#' }
#'
#'
#' @export

getFishData <- function(dataType = "abun",
                        taxonLevel = "Species",
                        program = "National Water Quality Assessment",
                        standardize = TRUE) {

  if(!(dataType %in% c("abun", "occur"))) {
    stop('dataType must be either "abun" or "occur".')}

  if(!isTRUE(standardize) && dataType == "abun"){
    message(paste('It is highly recommended that you use standardized',
                  'abundances, rather than raw abundances.'))
  }

  if(standardize != TRUE && standardize != FALSE){
    stop('standardize must be set to either TRUE or FALSE.')
  }

  if(!(taxonLevel %in% StreamData:::.TaxLevCols_Fish$Superclass$taxcols)){
    stop(paste('taxonLevel must be set between ranks "Superclass" and',
               '"Subspecies"; see "Details" in ?getFishData.'))
  }

  fish <- utils::read.csv(unzip(system.file("extdata",
                                            "FishResults.zip",
                                            package = "StreamData")),
                   colClasses = c("SiteNumber" = "character"))
  if(colnames(fish)[1] != "SIDNO"){
    colnames(fish)[1] = "SIDNO"
  }
  ##Remove the unzipped file from the system
  if(file.exists(system.file("extdata",
                             "20201217.0745.FishResults.csv",
                             package = "StreamData"))){
    unlink(system.file("extdata",
                       "20201217.0745.FishResults.csv",
                       package = "StreamData"))
  }
  Project <- utils::read.csv(system.file("extdata",
                                  "20201217.0745.Project.csv",
                                  package = "StreamData"),
                      comment.char="#")
  if(program == "ALL") {
    database <- c("National Water Quality Assessment",
                  "Cooperative Water Program",
                  "Collection of Basic Records",
                  "Other Federal Agencies")
  } else {database <- program }

  fishup = fish %>%
    dplyr::filter(ProjectLabel %in% (Project %>%
                                       dplyr::filter(Program %in% database) %>%
                                       dplyr::distinct(ProjectLabel,
                                                       .keep_all = FALSE))[ , "ProjectLabel"]) %>%
    dplyr::select(SIDNO, ProjectLabel, SiteNumber, CollectionDate, StartTime,
                  SiteName, StudyReachName, TimeDatum, CollectionYear,
                  CollectionMonth, CollectionDayOfYear, NAWQA.SMCOD,
                  ProvisionalData, ProjectAssignedSampleLabel,
                  NAWQAStudyUnitCode, MethodCode, Abundance,
                  PublishedTaxonNameLevel, PublishedTaxonName, Superclass, Class,
                  Subclass, Superorder, Order, Superfamily, Family, Subfamily,
                  Genus, Species, Subspecies) %>%
    tidyr::unite(SIDNO_MethodCode, c("SIDNO", "MethodCode"), remove = FALSE) %>%
    tidyr::unite(SampleID, c("SIDNO",
                      "SiteNumber",
                      "CollectionDate",
                      "PublishedTaxonName"), remove = FALSE) %>%
    dplyr::mutate(CollectionDate = as.Date(CollectionDate)) %>%
    dplyr::group_by(SampleID) %>%
    dplyr::mutate(SumAbundance = sum(Abundance, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-SampleID, -Abundance)

  ##Need to get Lat, Long, HUC, Drainage Area
  site <- utils::read.csv(system.file("extdata",
                               "20201217.0745.SiteInfo.csv",
                               package = "StreamData"),
                   colClasses = c("SiteNumber" = "character")) %>%
    dplyr::select(SiteNumber, Latitude_dd, Longitude_dd,
           CoordinateDatum,
           HUCCode, DrainageArea_mi2,
           SiteTypeName,
           CountyFIPSCode,
           StateFIPSCode)

  ##
  sample <- utils::read.csv(system.file("extdata",
                                 "20201217.0745.FishSamp.csv",
                                 package = "StreamData"),
                     colClasses = c("SiteNumber" = "character"))
  if(colnames(sample)[1] != "SIDNO"){
    colnames(sample)[1] = "SIDNO"
  }

  sample = sample %>%
    dplyr::select(SIDNO, ReachLengthFished_m)

  samplemethod = utils::read.csv(system.file("extdata",
                                      "20201217.0745.FishMethodAndSubreachInfo.csv",
                                      package = "StreamData"),
                          colClasses = c("SiteNumber" = "character"))

  if(colnames(samplemethod)[1] != "SIDNO"){
    colnames(samplemethod)[1] = "SIDNO"
  }
  samplemethod = samplemethod %>%
    tidyr::unite(SIDNO_MethodCode, c("SIDNO", "MethodCode"), remove = FALSE) %>%
    dplyr::select(SIDNO_MethodCode,
           NumberSeineHauls, NumberStationarySetsKicks, NumberSnorkelingTransects,
           SecondsShockTime)

  ##Join the datasets
  fish_info = dplyr::left_join(dplyr::left_join(dplyr::left_join(fishup,
                                            site,
                                            by = "SiteNumber"),
                                  sample,
                                  by = "SIDNO"), samplemethod, by = "SIDNO_MethodCode")

  mycols = StreamData:::.TaxLevCols_Fish[[which(names(StreamData:::.TaxLevCols_Fish) == taxonLevel)]]$mycols
  taxcols = StreamData:::.TaxLevCols_Fish[[which(names(StreamData:::.TaxLevCols_Fish) == taxonLevel)]]$taxcols


  fish_comm = fish_info %>%
    dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
    dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel)), dplyr::any_vars(. != "")) %>%
    tidyr::unite(UNIQUE, c(SIDNO, MethodCode, all_of(taxonLevel)), remove = FALSE) %>%
    dplyr::group_by(UNIQUE) %>%
    dplyr::mutate(SumAbundance = sum(SumAbundance)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-UNIQUE, -SIDNO_MethodCode, -PublishedTaxonName,
           -PublishedTaxonNameLevel) %>%
    dplyr::select(-tidyselect::any_of(mycols)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(taxonLevel),
                names_prefix = "tax_",
                values_from = SumAbundance,
                values_fill = 0)

  ##Need to figure out here IF this results in multiple SIDNO-MethodBasic lines

  if(isTRUE(standardize)){

    ####ISSUE HERE
    ##For some reason there are some zeros for standardize method

    fish_comm2 <- fish_comm %>%
      dplyr::filter(!is.na(NumberSeineHauls) | !is.na(SecondsShockTime) |
               !is.na(NumberStationarySetsKicks) |
               !is.na(NumberSnorkelingTransects)) %>%
      dplyr::filter(!is.na(ReachLengthFished_m))%>%
      dplyr::mutate(MethodBasic = ifelse(grepl("Seine", MethodCode, fixed = TRUE),
                                  "Seine",
                                  ifelse(grepl("Snork", MethodCode, fixed = TRUE),
                                         "Snorkel",
                                         "Shocking"))) %>%
      dplyr::mutate(MinutesShockTime = SecondsShockTime / 60) %>%
      dplyr::group_by(SIDNO, MethodBasic) %>%
      dplyr::mutate(across(contains("tax_"),
                    sum)) %>%
      dplyr::mutate(MinutesShockTime = sum(MinutesShockTime, na.rm = TRUE)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(StandardMethod = ifelse(MethodBasic == "Seine",
                                     rowSums(across(c("NumberSeineHauls",
                                                      "NumberStationarySetsKicks")),
                                             na.rm = TRUE),
                                     ifelse(MethodBasic == "Shocking",
                                            MinutesShockTime,
                                            NumberSnorkelingTransects))) %>%
      dplyr::relocate(tidyselect::contains("tax_"),
               .after = tidyselect::last_col()) %>%
      dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                    ~. / StandardMethod / ReachLengthFished_m ))

  } else {
    fish_comm2 = fish_comm %>%
      dplyr::mutate(MethodBasic = ifelse(grepl("Seine", MethodCode, fixed = TRUE),
                                  "Seine",
                                  ifelse(grepl("Snork", MethodCode, fixed = TRUE),
                                         "Snorkel",
                                         "Shocking"))) %>%
      dplyr::mutate(MinutesShockTime = SecondsShockTime / 60) %>%
      dplyr::group_by(SIDNO, MethodBasic) %>%
      dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                    sum)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(StandardMethod = ifelse(MethodBasic == "Seine",
                                     rowSums(across(c("NumberSeineHauls",
                                                      "NumberStationarySetsKicks")),
                                             na.rm = TRUE),
                                     ifelse(MethodBasic == "Shocking",
                                            MinutesShockTime,
                                            NumberSnorkelingTransects))) %>%
      dplyr::relocate(tidyselect::contains("tax_"),
               .after = tidyselect::last_col())
  }

  if(dataType == "occur") {
    fish_comm2 = fish_comm2 %>%
      dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                    ~replace(., . > 0, 1)))
  }

  colnames(fish_comm2) = sub("tax_", "", colnames(fish_comm2))

  fish_comm2 <- fish_comm2 %>%
    dplyr::select(-SecondsShockTime,
                  -SIDNO) %>%
    dplyr::relocate(tidyselect::any_of(StreamData:::.ReorderUSGSBioDataColNames))

  return(data.frame(fish_comm2))

}

##~11% of observations do not have area sampled associated with the sample
##Likely will need to drop those observations if we look at density/abundance
##But, not an issue if just doing pres/abs

##


##Individual fish info
## To do



##To come later!
#2) Individual traits (remove all blanks in the datasets)
##Individual-level information
## Length (mm)
## Mass (g)
### DELT  (see below) - fairly few, but COULD be something of interest.
## Deformities
## Eroded Fins
## Lesions
## Tumors
