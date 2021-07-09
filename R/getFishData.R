#' Access clean USGS Fish Dataset
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"Superclass"}, \code{"Class"}, \code{"Subclass"}, \code{"Superorder"},
#'   \code{"Order"}, \code{"Superfamily"}, \code{"Family"}, \code{"Subfamily"},
#'   \code{"Genus"}, \code{"Species"}, or \code{"Subspecies"}.
#' @param program The program name(s) that should be included in the output
#'   dataset. See \code{Details} below for more information.
#' @param standardize Standardization method to use for calculating fish abundance matrices.
#'   Default is \code{standardize = "none"}, which returns raw fish abundance values. Other options include
#'   \code{standardize = "CPUE"}, which returns standardized abundances in Catch per Unit Effort.
#'   An alternative standardization method is \code{standardize = "MGMS"}, which calculates
#'   Multigear Mean Standardization (MGMS) values to account for catchability differences between
#'   fish sampling methods (see 'details' or Gibson-Reinemer et al. (2014) for more info on MGMS).
#'
#' @return A species by sample data frame with site, stream reach, and
#'   sample information.
#'
#' @details Note: To standardize fish abundance data, abundances were divided by
#'   the seconds shocked, number of seine hauls, etc. Then were divided by the
#'   stream length sampled. ~35% of the data have been removed,
#'   as these samples lacked the required standardization information.
#'   Therefore, if you are interested in occurrence (pres/abs) data only, then
#'   set \code{dataType = "occur"}. Thus, be aware that setting \code{dataType = "occur"}
#'   will result in a larger dataset with additional samples/sites than when \code{dataType = "abun"}.
#'
#'   To account for differences in efficacy between shocking, seine netting, and
#'   snorkeling, we included multigear mean standardization (\code{standardize = "MGMS"}) as a
#'   standardization method as an alternative to catch per unit effort (\code{standardize = "CPUE"}).
#'   To calculate MGMS, we first calculated the mean total CPUE (summation of CPUEs for all
#'   taxa at each site for each collection date) for each sampling method. Then we divided
#'   each CPUE value by the mean total CPUE for their respective sampling methods. See
#'   the supplement of Gibson-Reinemer et al. (2014) for an example of MGMS computation.
#'
#'   Keep in mind that when setting \code{standardize = "CPUE"} will result in a larger dataset
#'   (more rows) than \code{standardize = "MGMS"} because CPUE data will have a row for each sampling
#'   method used at each time-location, whereas MGMS will have only one row for each time-location
#'   because the fuction is standardizing by sampling method in addition to standardizing by
#'   time and area sampled.
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
#' @references Gibson-Reinemer DK, Ickes BS, Chick JH, 2014. Development and assessment of a new method for combining
#' catch per unit effort data from different fish sampling gears: multigear mean standardization (MGMS).
#' Can. J. Fish. Aquat. Sci. 74:8-14.
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
                        standardize = "none") {

  if(!(dataType %in% c("abun", "occur"))) {
    stop('dataType must be either "abun" or "occur".')}

  if(standardize == "none" && dataType == "abun"){
    message(paste('It is highly recommended that you use standardized',
                  'abundances, rather than raw abundances.
                  (i.e. `standardize = "CPUE"` or `standardize = "MGMS"`)'))
  }

  if(!(standardize %in% c("none","CPUE","MGMS")) && dataType == "abun"){
    stop('standardize must be "none", "CPUE", or "MGMS"')
  }

  if(!(taxonLevel %in% StreamData:::.TaxLevCols_Fish$Superclass$taxcols)){
    stop(paste('taxonLevel must be set between ranks "Superclass" and',
               '"Subspecies"; see "Details" in ?getFishData.'))
  }

  fish <- utils::read.csv(base::unz(base::system.file("extdata",
                                                      "20201217.0745.FishResults.zip",
                                                      package = "StreamData"),
                                    "20201217.0745.FishResults.csv"),
                          colClasses = c("SiteNumber" = "character"),
                          stringsAsFactors = FALSE)
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

  if(dataType == "abun"){

  # calculate abundance matrices via raw abundance ("none"), CPUE standardization, or MGMS standardization
  if(standardize == "none"){
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
    ####ISSUE HERE
    ##For some reason there are some zeros for standardize method
  if (standardize == "CPUE"){
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

    # sum CPUE for replicate methods (e.g. "Seine x2" or "Shock x2")
    siteInfo = fish_comm2 %>%
      dplyr::select(-tidyselect::contains("tax_")) %>%
      dplyr::group_by(CollectionDate, SiteNumber)

    condensedCPUEdata = fish_comm2 %>%
      dplyr::select(tidyselect::contains("tax_"), CollectionDate, SiteNumber, MethodBasic) %>%
      dplyr::group_by(CollectionDate, SiteNumber, MethodBasic) %>%
      dplyr::summarise_all(.funs = sum)

    fish_comm2 = siteInfo %>%
      dplyr::left_join(condensedCPUEdata) %>%
      dplyr::group_by(CollectionDate,SiteNumber,MethodBasic) %>%
      dplyr::slice(1)

    }

    # alternative standardization method, multigear mean standardization (Gibson-Reinemer et al. 2014)
  if (standardize == "MGMS"){
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

      # sum CPUE for replicate methods (e.g. "Seine x2" or "Shock x2")
      siteInfo = fish_comm2 %>%
        dplyr::select(-tidyselect::contains("tax_")) %>%
        dplyr::group_by(CollectionDate, SiteNumber)

      condensedCPUEdata = fish_comm2 %>%
        dplyr::select(tidyselect::contains("tax_"), CollectionDate, SiteNumber, MethodBasic) %>%
        dplyr::group_by(CollectionDate, SiteNumber, MethodBasic) %>%
        dplyr::summarise_all(.funs = sum)

      fish_comm2 = siteInfo %>%
        dplyr::left_join(condensedCPUEdata) %>%
        dplyr::group_by(CollectionDate,SiteNumber,MethodBasic) %>%
        dplyr::slice(1)

      TotalCPUE.df = fish_comm2 %>%
        dplyr::mutate(TotalCPUE = rowSums(dplyr::across(tidyselect::contains("tax_")))) # sum CPUE for all taxa

      # Remove TotalCPUE NAs to avoid producing NaN in Mean Total CPUE calculations
      # (this only seems to be a problem for ~10 Shocking data points)

      MeanTotalCPUE = TotalCPUE.df[!is.na(TotalCPUE.df$TotalCPUE),] %>%
        dplyr::group_by(MethodBasic) %>%
        dplyr::summarise(MeanTotalCPUE = mean(TotalCPUE)) # calculate avg CPUE for each sampling method

      fish_comm2 = dplyr::left_join(fish_comm2, MeanTotalCPUE, by = "MethodBasic")

      fish_comm2 = fish_comm2 %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                                    .fns = ~./MeanTotalCPUE)) %>% # divide each row by the mean CPUE for its respective sampling method
                                                                  # (this is now in units of MGMS)
        dplyr::ungroup()

      siteInfo = fish_comm2 %>%
        dplyr::select(-tidyselect::contains("tax_")) %>%
        dplyr::group_by(CollectionDate, SiteNumber) %>%
        dplyr::mutate(Methods = paste(MethodBasic, collapse = ", ")) %>%
        # include only unique site-date combinations
        dplyr::slice(1) %>%
        dplyr::ungroup()

      # sum MGMS values for rows with same collection date and site number (combining methods since they are now standardized)
      condensedMGMSdata = fish_comm2 %>%
        dplyr::select(tidyselect::contains("tax_"), CollectionDate, SiteNumber) %>%
        dplyr::group_by(CollectionDate, SiteNumber) %>%
        dplyr::summarise_all(.funs = mean) %>%
        dplyr::ungroup()

      fish_comm2 = siteInfo %>%
        dplyr::left_join(condensedMGMSdata)

  }
}

  if(dataType == "occur") {
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
