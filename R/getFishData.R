#' Access harmonized USGS and EPA Fish datasets
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"Class"}, \code{"Subclass"}, \code{"Order"}, \code{"Family"},
#'   \code{"Genus"}, or \code{"Species"}.
#' @param agency The agency name(s) (e.g., "USGS" and "EPA") that should be
#'   included in the output dataset. See \code{Details} below for more information.
#' @param standardize Standardization method to use for calculating fish abundance matrices.
#'   Default is \code{standardize = "none"}, which returns raw fish abundance values. Other options include
#'   \code{standardize = "CPUE"}, which returns standardized abundances in Catch per Unit Effort.
#'   An alternative standardization method is \code{standardize = "MGMS"}, which calculates
#'   Multigear Mean Standardization (MGMS) values to account for catchability differences between
#'   fish sampling methods (see 'details' or Gibson-Reinemer et al. (2014) for more info on MGMS).
#' @param hybrid logical. Should hybrid individuals be included in the output dataset?
#' \code{TRUE} or \code{FALSE}.
#' @param boatableStreams logical. Should EPA boatable streams be included in the
#'   output dataset? \code{TRUE} or \code{FALSE}. Note: all USGS streams are wadable;
#'   so \code{boatableStreams} should only be set to \code{TRUE} when gathering
#'   EPA data only.
#'
#' @return A taxa by sample data frame with site, stream reach, and
#'   sample information.
#'
#' @details
#'   Note: To standardize fish abundance data, abundances were divided by
#'   the minutes shocked, number of seine hauls, etc. Then were divided by the
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
#'   because the function is standardizing by sampling method in addition to standardizing by
#'   time and area sampled.
#'
#'   \code{agency} refers to the federal agency that collected the fish samples. If
#'   you want to use data from both agencies, set \code{agency = c("USGS", "EPA")}.
#'   For the "USGS" dataset, this includes all programs that collected fish data
#'   within the larger USGS database. For the "EPA"  dataset, samples from the
#'   National Stream and River Assessment programs (2018-2019, 2013-2014,
#'   2008-2009) will be included. Note that by default, only moving waters
#'   classified as "wadeable" are included and only samples that are "reach-wide"
#'   are included. Some information included in the USGS dataset are not included
#'   in the EPA datasets, and vice-versa, and thus will appear as "NA". NOTE:
#'   As of now, taxonLevel must be set to "Species".
#'
#' @author Michael Mahon, Ethan Brown, Samantha Rumschlag
#'
#' @references Gibson-Reinemer DK, Ickes BS, Chick JH, 2014. Development and assessment of a new method for combining
#' catch per unit effort data from different fish sampling gears: multigear mean standardization (MGMS).
#' Can. J. Fish. Aquat. Sci. 74:8-14.
#'
#' @examples
#' \dontrun{
#' Fish <- getFishData(taxonLevel = "Species")
#'
#' }
#'
#'
#' @export

getFishData <- function(dataType = "occur",
                        taxonLevel = "Species",
                        agency = c("USGS","EPA"),
                        standardize = "none",
                        hybrids = FALSE,
                        boatableStreams = FALSE) {

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

  if(taxonLevel != "Species"){
    stop(paste('as of now, taxonLevel must be set to "Species"'))
  }


  if(any(grepl("USGS", agency))){
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
    Project <- data.table::fread(system.file("extdata",
                                           "20201217.0745.Project.csv",
                                           package = "StreamData"),
                                 data.table = F)

    database <- c("National Water Quality Assessment",
                  "Cooperative Water Program",
                  "Collection of Basic Records",
                  "Other Federal Agencies")

    ##NEED TO FIGURE OUT HOW TO HANDLE HYBRIDS HERE; I THINK THERE IS A HYBRID TAG
    ##COLUMN; SHOULD BE ABLE TO RIP STRAIGHT FROM THERE: BioDataTaxonName

    fishH <- data.frame(Hybrid = unique((fish %>%
                                           filter(HybridFlag == "Y"))$BioDataTaxonName)) %>%
      filter(!grepl("hybrid", Hybrid)) %>%
      mutate(Spp1 = sub("\\ x .*", "", Hybrid),
             Spp2 = sub(".*\\ x ", "", Hybrid),
             Genus1 = sub("\\ .*" ,
                          "",Spp1),
             Species1 = sub(".*\\ " ,
                            "",Spp1),
             Genus2 = sub("\\ .*" ,
                          "",Spp2),
             Species2 = sub(".*\\ " ,
                            "",Spp2),
             GENUS = ifelse(Genus1 == Genus2,
                            Genus1,
                            paste(Genus1, tolower(Genus2), sep = " x ")),
             SPECIES = paste(Species1, Species2, sep = " x "),
             SCIENTIFIC = paste(GENUS, SPECIES, sep = " ")
      ) %>%
      filter(Genus2 != Species2)

    fish$Genus <- ifelse(fish$BioDataTaxonName %in% fishH$Hybrid,
                         fishH$GENUS[match(fish$BioDataTaxonName,
                                           fishH$Hybrid)],
                         fish$Genus)

    fish$Species <- ifelse(fish$BioDataTaxonName %in% fishH$Hybrid,
                           fishH$SCIENTIFIC[match(fish$BioDataTaxonName,
                                                  fishH$Hybrid)],
                           fish$Species)

    fishup = fish %>%
      dplyr::filter(ProjectLabel %in% (Project %>%
                                         dplyr::filter(Program %in% database) %>%
                                         dplyr::distinct(ProjectLabel,
                                                         .keep_all = FALSE))[ , "ProjectLabel"]) %>%
      dplyr::select(SIDNO, ProjectLabel, SiteNumber, CollectionDate, StartTime,
                    SiteName, StudyReachName, TimeDatum, CollectionYear,
                    CollectionMonth, CollectionDayOfYear, NAWQA.SMCOD,
                    SiteVisitSampleNumber, ProjectAssignedSampleLabel,
                    NAWQAStudyUnitCode, MethodCode, Abundance,
                    PublishedTaxonNameLevel, PublishedTaxonName, BioDataTaxonName,
                    Superclass, Class,
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
    site <- data.table::fread(system.file("extdata",
                                        "20201217.0745.SiteInfo.csv",
                                        package = "StreamData"),
                            colClasses = c("SiteNumber" = "character"),
                            data.table = F) %>%
      dplyr::select(SiteNumber, Latitude_dd, Longitude_dd,
                    CoordinateDatum,
                    HUCCode, DrainageArea_mi2,
                    SiteTypeName,
                    CountyFIPSCode,
                    StateFIPSCode)

    ##
    sample <- data.table::fread(system.file("extdata",
                                          "20201217.0745.FishSamp.csv",
                                          package = "StreamData"),
                              colClasses = c("SiteNumber" = "character"),
                              data.table = F)
    if(colnames(sample)[1] != "SIDNO"){
      colnames(sample)[1] = "SIDNO"
    }

    sample = sample %>%
      dplyr::select(SIDNO, ReachLengthFished_m)

    samplemethod = data.table::fread(system.file("extdata",
                                               "20201217.0745.FishMethodAndSubreachInfo.csv",
                                               package = "StreamData"),
                                   colClasses = c("SiteNumber" = "character"),
                                   data.table = F)

    if(colnames(samplemethod)[1] != "SIDNO"){
      colnames(samplemethod)[1] = "SIDNO"
    }
    samplemethod = samplemethod %>%
      tidyr::unite(SIDNO_MethodCode, c("SIDNO", "MethodCode"), remove = FALSE) %>%
      dplyr::select(SIDNO_MethodCode,
                    NumberSeineHauls, NumberStationarySetsKicks, NumberSnorkelingTransects,
                    SecondsShockTime)
    samplemethod <- samplemethod %>%
      group_by(SIDNO_MethodCode) %>%
      summarise(across(NumberSeineHauls:SecondsShockTime, ~sum(.x, na.rm = T)))
    ##Join the datasets
    fish_info = dplyr::left_join(dplyr::left_join(dplyr::left_join(fishup,
                                                                   site,
                                                                   by = "SiteNumber"),
                                                  sample,
                                                  by = "SIDNO"), samplemethod, by = "SIDNO_MethodCode")

    mycols = StreamData:::.TaxLevCols_Fish[[which(names(StreamData:::.TaxLevCols_Fish) == taxonLevel)]]$mycols
    taxcols = StreamData:::.TaxLevCols_Fish[[which(names(StreamData:::.TaxLevCols_Fish) == taxonLevel)]]$taxcols

    fish_info = fish_info %>%
      mutate(FISH_PROTOCOL = ifelse(grepl("Boat", MethodCode),
                                    "BOATABLE",
                                    ifelse(ReachLengthFished_m / 20 < 12.5,
                                           "SM_WADEABLE",
                                           ifelse(ReachLengthFished_m / 20 < 25.1,
                                                  "LG_WADEABLE",
                                                  "BOATABLE"))))


    fish_comm = fish_info %>%
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols |
                      grepl(" x ", BioDataTaxonName)) %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel)), dplyr::any_vars(. != "")) %>%
      tidyr::unite(UNIQUE, c(SIDNO, MethodCode, all_of(taxonLevel)), remove = FALSE) %>%
      dplyr::group_by(UNIQUE) %>%
      # multi-counting below; remove!
      # dplyr::mutate(SumAbundance = sum(SumAbundance)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-UNIQUE, -SIDNO_MethodCode, -PublishedTaxonName,
                    -PublishedTaxonNameLevel, -BioDataTaxonName) %>%
      dplyr::select(-tidyselect::any_of(mycols)) %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(taxonLevel),
                         names_prefix = "tax_",
                         values_from = SumAbundance,
                         values_fill = 0)



    if(dataType == "abun"){

      # calculate abundance matrices via raw abundance ("none"), CPUE standardization, or MGMS standardization
      if(standardize == "none"){
        fish_comm2 = fish_comm %>%
          dplyr::mutate(MethodBasic = ifelse(grepl("Seine|Net", MethodCode, fixed = TRUE),
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
          dplyr::mutate(NumberSeineHauls = ifelse(NumberSeineHauls == 0,
                                                  1,
                                                  NumberSeineHauls)) %>%
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

        suppressMessages({fish_comm2 = siteInfo %>%
          dplyr::left_join(condensedCPUEdata) %>%
          dplyr::group_by(CollectionDate,SiteNumber,MethodBasic) %>%
          dplyr::slice(1)})

      }

      # alternative standardization method, multigear mean standardization (Gibson-Reinemer et al. 2017)
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
          dplyr::mutate(NumberSeineHauls = ifelse(NumberSeineHauls == 0,
                                                  1,
                                                  NumberSeineHauls)) %>%
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

        suppressMessages({fish_comm2 = siteInfo %>%
          dplyr::left_join(condensedCPUEdata) %>%
          dplyr::group_by(CollectionDate,SiteNumber,MethodBasic) %>%
          dplyr::slice(1)})

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

        suppressMessages({fish_comm2 = siteInfo %>%
          dplyr::left_join(condensedMGMSdata)})

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
        dplyr::mutate(Methods = paste(MethodBasic, collapse = ", ")) %>%
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
                        .after = tidyselect::last_col()) %>%
        dplyr::select(-SecondsShockTime,
                      -SIDNO) %>%
        dplyr::relocate(tidyselect::any_of(StreamData:::.ReorderUSGSBioDataColNames))
    }
  }

  if(any(grepl("EPA", agency))){
    ##EPA ONLY WORKS WITH SPECIES FOR NOW; NOT GOING TO TAKE THE TIME TO MAKE IT WORK OTHERWISE
    UA <- paste('Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:98.0)',
                'Gecko/20100101 Firefox/98.0')
    #READ IN STUFF
    ##Read in datasets directly from EPA website - may want a more stable source
    ##in the future (github repo?)
    NRSA_1819_fishcnt = data.frame(data.table::fread(httr::content(httr::GET("https://www.epa.gov/system/files/other-files/2022-03/nrsa-1819-fish-count-data.csv",
                                                                                 httr::add_headers(`User-Agent` = UA)),
                                                                       encoding = "UTF-8", as = "text"),
                                                         colClasses = c("UID" = "character"),
                                                         stringsAsFactors = FALSE))

    NRSA_1819_sites = data.frame(data.table::fread(httr::content(httr::GET("https://www.epa.gov/system/files/other-files/2022-01/nrsa-1819-site-information-data-updated.csv",
                                                                           httr::add_headers(`User-Agent` = UA)),
                                                                 encoding = "UTF-8", as = "text"),
                                                   colClasses = c("UID" = "character"),
                                                   stringsAsFactors = FALSE))

    ##FIX 1819 COUNT UIDs
    NRSA_1819_fishcnt$UID <- NRSA_1819_sites$UID[match(paste(NRSA_1819_fishcnt$SITE_ID,
                                                             NRSA_1819_fishcnt$DATE_COL, sep = "_"),
                                                       paste(NRSA_1819_sites$SITE_ID,
                                                             NRSA_1819_sites$DATE_COL, sep = "_"))]


    NRSA_1314_fishcnt = data.frame(data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_fishcts_04232019.csv",
                                                                             httr::add_headers(`User-Agent` = UA)),
                                                                   encoding = "UTF-8", as = "text"),
                                                     colClasses = c("UID" = "character"),
                                                     stringsAsFactors = FALSE))

    NRSA_1314_sites = suppressMessages(data.frame(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2019-04/nrsa1314_siteinformation_wide_04292019.csv",
                                                                          httr::add_headers(`User-Agent` = UA)),
                                                                encoding = "UTF-8",
                                                                show_col_types = FALSE)) %>%
                                         dplyr::mutate(UID = as.character(UID)))

    NRSA_0809_fishcnts = data.frame(data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2015-09/fishcts.csv",
                                                                              httr::add_headers(`User-Agent` = UA)),
                                                                    encoding = "UTF-8", as = "text"),
                                                      colClasses = c("UID" = "character"),
                                                      stringsAsFactors = FALSE))

    NRSA_0809_sites = data.frame(data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                                                                           httr::add_headers(`User-Agent` = UA)),
                                                                 encoding = "UTF-8", as = "text"),
                                                   colClasses = c("UID" = "character"),
                                                   stringsAsFactors = FALSE))

    NRSA_1314_fishtax <- data.table::fread(system.file("extdata",
                                                     "updateNRSAfishtax.csv",
                                                     package = "StreamData"))

    NRSA_fish_sampleinfo <- data.table::fread(system.file("extdata",
                                                        "NRSA_Fish_SamplingInfo.csv",
                                                        package = "StreamData"),
                                            colClasses = c("UID" = "character"),
                                            stringsAsFactors = FALSE)

    #############
    ##2008/2009
    suppressMessages({NRSA_0809 <- NRSA_0809_fishcnts %>%
      dplyr::left_join(NRSA_1314_fishtax %>%
                         dplyr::filter(FINAL_NAME %in%
                                         unique(NRSA_0809_fishcnts$FINAL_NAME)) %>%
                         dplyr::mutate(across(c(FAMILY, GENUS),
                                              ~ stringr::str_to_sentence(.x))) %>%
                         dplyr::mutate(SPECIES = stringr::str_to_lower(SPECIES)) %>%
                         tidyr::unite("SCIENTIFIC",
                                      c(GENUS, SPECIES),
                                      remove = F,
                                      sep = " ") %>%
                         dplyr::select(FAMILY, GENUS, SPECIES,
                                       SCIENTIFIC, FINAL_NAME)
      ) %>%
      filter(!FAMILY %in% c("Ranidae", "Dicamptodontidae", "Salamandridae",
                            "Pipidae", "Bufonidae", "Hylidae", "Rhyacotritonidae",
                            "Ambystomatidae", "Leiopelmatidae")) %>%
      filter(IS_DISTINCT == 1) %>%
      filter(NOTFISH == "") %>%
      select(UID:VISIT_NO, DATE_COL, ELECTROFISH:METHOD, FISH_PROTOCOL, FAMILY:SCIENTIFIC, FINAL_NAME, FINAL_CT) %>%
      mutate(DATE_COL = as.Date(DATE_COL, "%d-%B-%y"))})



    ##2013/2014
    suppressMessages({NRSA_1314 <- NRSA_1314_fishcnt %>%
      dplyr::left_join(NRSA_1314_fishtax %>%
                         dplyr::filter(FINAL_NAME %in%
                                         unique(NRSA_1314_fishcnt$FINAL_NAME)) %>%
                         dplyr::mutate(across(c(FAMILY, GENUS),
                                              ~ stringr::str_to_sentence(.x))) %>%
                         dplyr::mutate(SPECIES = stringr::str_to_lower(SPECIES)) %>%
                         tidyr::unite("SCIENTIFIC",
                                      c(GENUS, SPECIES),
                                      remove = F,
                                      sep = " ") %>%
                         dplyr::select(FAMILY, GENUS, SPECIES,
                                       SCIENTIFIC, FINAL_NAME)
      ) %>%
      filter(!FAMILY %in% c("Ranidae", "Dicamptodontidae", "Salamandridae",
                            "Pipidae", "Bufonidae", "Hylidae", "Rhyacotritonidae",
                            "Ambystomatidae", "Leiopelmatidae")) %>%
      filter(IS_DISTINCT == 1) %>%
      dplyr::select(UID, SITE_ID:LON_DD83, FAMILY:SCIENTIFIC, FINAL_NAME, TOTAL)%>%
      mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))})


    ##2018/2019
    suppressMessages({NRSA_1819 <- NRSA_1819_fishcnt %>%
      dplyr::left_join(NRSA_1314_fishtax %>%
                         dplyr::filter(FINAL_NAME %in%
                                         unique(NRSA_1819_fishcnt$FINAL_NAME)) %>%
                         dplyr::mutate(across(c(FAMILY, GENUS),
                                              ~ stringr::str_to_sentence(.x))) %>%
                         dplyr::mutate(SPECIES = stringr::str_to_lower(SPECIES)) %>%
                         tidyr::unite("SCIENTIFIC",
                                      c(GENUS, SPECIES),
                                      remove = F,
                                      sep = " ") %>%
                         dplyr::select(FAMILY, GENUS, SPECIES,
                                       SCIENTIFIC, FINAL_NAME)
      ) %>%
      filter(!FAMILY %in% c("Ranidae", "Dicamptodontidae", "Salamandridae",
                            "Pipidae", "Bufonidae", "Hylidae", "Rhyacotritonidae",
                            "Ambystomatidae", "Leiopelmatidae")) %>%
      filter(IS_DISTINCT == 1) %>%
      dplyr::select(UID, SITE_ID:VISIT_NO, STATE, FAMILY:SCIENTIFIC, FINAL_NAME, TOTAL)%>%
      mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))})


    NRSA_FISH <- dplyr::bind_rows(NRSA_0809, NRSA_1314, NRSA_1819)

    NRSA_FISH <- NRSA_FISH %>%
      mutate(TOTAL = ifelse(is.na(TOTAL),
                            FINAL_CT,
                            TOTAL)) %>%
      dplyr::select(-FINAL_CT,-STATE,-SITE_ID, -PSTL_CODE, -LAT_DD83, -LON_DD83)


    ###Need to link the site-level information with the taxa-level info;
    #start w/ 08/09

    ##Need UID, SITE_ID, DATE_COL, VISIT_NO, STATE, LOC_NAME, LAT_DD, LON_DD, MASTER_SITEID

    NRSA_0809_s <- NRSA_0809_sites %>%
      dplyr::select(UID, SITE_ID, MASTER_SITEID, DATE_COL, VISIT_NO, STATE, LOC_NAME, LAT_DD83, LON_DD83) %>%
      mutate(DATE_COL = as.Date(DATE_COL, "%d-%B-%y"))

    NRSA_1314_s <- NRSA_1314_sites %>%
      dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, BOAT_WADE, NARS_NAME, LAT_DD83, LON_DD83) %>%
      mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))

    NRSA_1819_s <- NRSA_1819_sites %>%
      dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, UNIQUE_ID, NARS_NAME, LAT_DD83, LON_DD83) %>%
      mutate(VISIT_NO = as.numeric(ifelse(VISIT_NO == "R",
                                          2,
                                          VISIT_NO)))%>%
      mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))

    NRSA_fish_sites <- bind_rows(NRSA_0809_s, NRSA_1314_s, NRSA_1819_s) %>%
      relocate(UNIQUE_ID, .after = SITE_ID) %>%
      mutate(SITENAME = ifelse(is.na(LOC_NAME),
                               NARS_NAME,
                               LOC_NAME)) %>%
      dplyr::select(-c(LOC_NAME, NARS_NAME)) %>%
      relocate(SITENAME, .after = VISIT_NO)

    ##MASTER ID LIST
    NRSA_fish_sites$UNIQUE_ID <- ifelse(NRSA_fish_sites$SITE_ID %in% StreamData:::.NRSA_siteIDs$SITE_ID,
                                        StreamData:::.NRSA_siteIDs$UNIQUE_ID[match(NRSA_fish_sites$SITE_ID,
                                                                                   StreamData:::.NRSA_siteIDs$SITE_ID)],
                                        NA)

    ##if site number in nrsa_comms1 is in the MASTER_SITEID in the master crosswalk list,
    ##match the numbers and pull the corresponding unique id, which is the crosswalked site id,
    ##else give the current UNIQUE ID
    NRSA_fish_sites$UNIQUE_ID <- ifelse(NRSA_fish_sites$SITE_ID %in% StreamData:::.NRSA_siteIDs$MASTER_SITEID,
                                        StreamData:::.NRSA_siteIDs$UNIQUE_ID[match(NRSA_fish_sites$SITE_ID,
                                                                                   StreamData:::.NRSA_siteIDs$MASTER_SITEID)],
                                        NRSA_fish_sites$UNIQUE_ID)

    ##if there are any NA values in UNIQUE ID, replace these with the SiteNumber
    NRSA_fish_sites$SITE_ID = ifelse(is.na(NRSA_fish_sites$UNIQUE_ID),
                                     NRSA_fish_sites$SITE_ID,
                                     NRSA_fish_sites$UNIQUE_ID)

    ##remove the UNIQUEID column, as it is no longer needed
    NRSA_fish_sites <- NRSA_fish_sites %>%
      select(-UNIQUE_ID, -MASTER_SITEID)

    ##Join fish_sites w/ sampling information prior to joining with fish assemblage data
    ##Error here; difference in codes. UID should be character, no?

    NRSA_fish_info <- NRSA_fish_sites %>%
      dplyr::left_join(NRSA_fish_sampleinfo %>%
                         filter(UID %in% NRSA_fish_sites$UID) %>%
                         dplyr::select(-PRIM_FSHTIME), by = "UID")

    ##Join NRSA_fish_sites with actual data - may need to do the reverse order here
    ##Could be that some sites aren't in fish dataset, but are in site dataset
    ##because no fish could be sampled...; rbind these back into the dataset after the join

    ##Need to reorganize the wadeable/nonwadeable thing
    ##small, large, boatable
    ##Specifically need size classes <12, 12-25, >25; include this as a covariate

    NRSA_FISH_wSite <- NRSA_FISH %>%
      dplyr::select(-FISH_PROTOCOL) %>%
      left_join(NRSA_fish_info %>%
                  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
                  slice(1) %>%
                  ungroup() %>%
                  dplyr::select(-DATE_COL, -VISIT_NO),
                by = "UID") %>%
      mutate(FISH_PROTOCOL = ifelse(MethodBasic == "Shocking" & FISH_PROTOCOL == "WADEABLE",
                                    ifelse(is.na(ELECTROFISH) | ELECTROFISH == "",
                                           ifelse(RCH_LENGTH > 499,
                                                  "LG_WADEABLE",
                                                  "SM_WADEABLE"),
                                           ifelse(ELECTROFISH %in% c("BANK/TOW", "BOAT", "RAFT"),
                                                  "LG_WADEABLE",
                                                  "SM_WADEABLE")),
                                    ifelse(MethodBasic == "Seine" & FISH_PROTOCOL == "WADEABLE",
                                           ifelse(RCH_LENGTH > 499,
                                                  "LG_WADEABLE",
                                                  "SM_WADEABLE"),
                                           FISH_PROTOCOL)),
             StandardMethod = ifelse(MethodBasic == "Seine",
                                     NumberSeineHauls,
                                     MinutesShockTime)
      ) %>%
      dplyr::select(-BOAT_WADE)


    ##Add no fish collected rows
    NRSA_FISH_wSite <-  bind_rows(NRSA_FISH_wSite,
                                  NRSA_fish_info %>%
                                    filter(SAMPLED_FISH == "YES-NO FISH INFERRED") %>%
                                    mutate(RCH_LENGTH = 150,
                                           FISH_PROTOCOL = "SM_WADEABLE",
                                           MethodBasic = "Shocking",
                                           MinutesShockTime = 10,
                                           StandardMethod = MinutesShockTime,
                                           GENUS = "No fish",
                                           SPECIES = "No fish",
                                           SCIENTIFIC = "No fish collected") %>%
                                    dplyr::select(-BOAT_WADE)
    )



    NRSA_FISH_comm <- NRSA_FISH_wSite %>%
      filter(!is.na(GENUS) ) %>%
      filter(!is.na(SPECIES) & SPECIES != "") %>%
      filter(!grepl(" or ", SCIENTIFIC)) %>%
      dplyr::select(-FAMILY, -GENUS, -SPECIES, -FINAL_NAME) %>%
      pivot_wider(names_from = SCIENTIFIC,
                  names_prefix = "tax_",
                  values_from = TOTAL,
                  values_fn = sum,
                  values_fill = 0)
    ####Need to add if group here for just "abundance"

    if(dataType == "abun"){
      if(standardize == "none"){

      }
      if(standardize %in% c("CPUE", "MGMS")){
        NRSA_FISH_comm2 <- NRSA_FISH_comm %>%
          dplyr::filter(!is.na(NumberSeineHauls) | !is.na(MinutesShockTime) |
                          !is.na(RCH_LENGTH)) %>%
          dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                                      ~. / StandardMethod / RCH_LENGTH ))
        if(standardize == "MGMS"){

          NRSATotalCPUE.df = NRSA_FISH_comm2 %>%
            dplyr::mutate(TotalCPUE = rowSums(dplyr::across(tidyselect::contains("tax_")))) # sum CPUE for all taxa

          NRSAMeanTotalCPUE = NRSATotalCPUE.df[!is.na(NRSATotalCPUE.df$TotalCPUE),] %>%
            dplyr::group_by(MethodBasic) %>%
            dplyr::summarise(MeanTotalCPUE = mean(TotalCPUE)) # calculate avg CPUE for each sampling method

          NRSA_FISH_comm2 = dplyr::left_join(NRSA_FISH_comm2, NRSAMeanTotalCPUE, by = "MethodBasic")

          NRSA_FISH_comm2 = NRSA_FISH_comm2 %>%
            dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                                        .fns = ~./MeanTotalCPUE)) %>%# divide each row by the mean CPUE for its respective sampling method
            # (this is now in units of MGMS)
            dplyr::relocate(tidyselect::contains("tax_"),
                            .after = tidyselect::last_col())

        }
        NRSA_FISH_comm <- NRSA_FISH_comm2
      }
    }


    NRSA_FISH_comm <- NRSA_FISH_comm %>%
      ##Last step is to remove those sites wihtout sufficient sampling
      filter(!(grepl("NO-|NOT|LOST",SAMPLED_FISH))) %>%
      mutate(ProjectAssignedSampleLabel = paste(SITE_ID, UID, sep = "_"),
             CollectionYear = lubridate::year(DATE_COL),
             CollectionMonth = lubridate::month(DATE_COL),
             CollectionDayOfYear = lubridate::yday(DATE_COL),
             ProjectLabel = ifelse(CollectionYear %in% c(2008,2009),
                                   "NRSA0809",
                                   ifelse(CollectionYear %in% c(2013,2014),
                                          "NRSA1314",
                                          "NRSA1819")),
             CoordinateDatum = "NAD83") %>%
      rename(NAWQA.SMCOD = UID,
             SiteNumber = SITE_ID,
             SiteName = SITENAME,
             CollectionDate = DATE_COL,
             Latitude_dd = LAT_DD83,
             Longitude_dd = LON_DD83,
             MethodBasic = MethodBasic,
             SiteVisitSampleNumber = VISIT_NO,
             ReachLengthFished_m = RCH_LENGTH) %>%
      dplyr::select(-STATE, -ELECTROFISH, -METHOD,-FISHED)

    ##Need to add the following columns to the NRSA dataset:
    ##ProjectLabel (Which bout)
    ##ProjectAssignedSample Label (Site_ID;UID)
    ##NAWQA.SMCOD (UID)
    ##MAKE SURE COLLECTION DATE IS as.Date in both
    ##Add CollectionYear, CollectionMonth, CollectionDayOfYear
    #SiteNumber in USGS should be USGS-... (Site_ID)
    ##SiteName (SITENAME)
    ##SiteTypeName (FISH_PROTOCOL)
    #Remove FIPS and STAET from both
    ##Latitude_dd and Longitude_dd (LAT and LON)
    ##CoordinateDatum (NAD83)
    #MethodCode (Electrofish)
    ##MethodBasic (Method - Siene or Shocking)
    ##StandardMethod (FISHED)

  }


  if(all(grepl("EPA", agency))) {

    full_fish <- NRSA_FISH_comm %>%
      mutate(Agency = "EPA") %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))

  } else if(all(grepl("USGS", agency))){
    full_fish = fish_comm2 %>%
      mutate(SiteNumber = paste("USGS-",SiteNumber,sep = ""),
             # StandardMethod = as.character(StandardMethod),
             Agency = "USGS")%>%
      relocate(Agency, .after = SiteNumber) %>%
      dplyr::select(-StateFIPSCode, -CountyFIPSCode)  %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))

  } else if(any(grepl("EPA", agency)) & any(grepl("USGS", agency))) {

    full_fish <- bind_rows(fish_comm2 %>%
                             mutate(SiteNumber = paste("USGS-",SiteNumber,sep = ""),
                                    # StandardMethod = as.character(StandardMethod),
                                    Agency = "USGS") %>%
                             relocate(Agency, .after = SiteNumber) %>%
                             dplyr::select(-StateFIPSCode, -CountyFIPSCode, -MethodCode),
                           NRSA_FISH_comm %>% mutate(Agency = "EPA")) %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))
  } else {}


  if(dataType == "occur") {
    full_fish = full_fish %>%
      dplyr::mutate(dplyr::across(tidyselect::starts_with("tax_"),
                                  ~replace(., . > 0, 1)))
  }

  ##Remove those observations with 0s in their StandardMethod
  full_fish <- full_fish %>%
    filter(StandardMethod != 0) %>%
    dplyr::select(-tidyselect::any_of(c("NumberSeineHauls",
                                        "NumberStationarySetsKicks", "NumberSnorkelingTransects",
                                        "MinutesShockTime", "SecondsShockTime")))

  full_fish <- full_fish %>%
    left_join(StreamData:::.allsitesCOMID, by = dplyr::join_by(SiteNumber))

  full_fish <- full_fish  %>%
    dplyr::relocate(tidyselect::contains("tax_"), .after = last_col())

  colnames(full_fish) = sub("tax_", "", colnames(full_fish))

  if(!isTRUE(boatableStreams)){
    full_fish <- full_fish %>%
      ##remove boatable sites
      filter(FISH_PROTOCOL != "BOATABLE")
  }

  ##Fix some odd taxonomy issues here - mostly getting rid of subspecies
  full_fish <- full_fish %>%
    mutate(`Cottus bairdii` = ifelse(all(c("Cottus bairdii", "Cottus bairdi") %in% names(.)),
                                      `Cottus bairdii` + `Cottus bairdi`,
                                     c("Cottus bairdii", "Cottus bairdi")[(c("Cottus bairdii", "Cottus bairdi") %in% names(.))]),
           `Macrhybopsis aestivalis` = ifelse(all(c("Macrhybopsis aestivalis",
                                                    "Macrhybopsis cf. aestivalis") %in% names(.)),
                                              `Macrhybopsis aestivalis` + `Macrhybopsis cf. aestivalis`,
                                              c("Macrhybopsis aestivalis",
                                                "Macrhybopsis cf. aestivalis")[(c("Macrhybopsis aestivalis",
                                                                                  "Macrhybopsis cf. aestivalis") %in% names(.))]),
           `Notropis spectrunculus` = ifelse(all(c("Notropis spectrunculus",
                                                    "Notropis cf. spectrunculuss") %in% names(.)),
                                             `Notropis spectrunculus` + `Notropis cf. spectrunculus`,
                                              c("Notropis spectrunculus",
                                                "Notropis cf. spectrunculuss")[(c("Notropis spectrunculus",
                                                                                  "Notropis cf. spectrunculuss") %in% names(.))]),
           `Catostomus latipinnis` = ifelse(all(c("Catostomus latipinnis",
                                                   "Catostomus cf. latipinnis") %in% names(.)),
                                             `Catostomus latipinnis` + `Catostomus cf. latipinnis`,
                                            c("Catostomus latipinnis",
                                              "Catostomus cf. latipinnis")[(c("Catostomus latipinnis",
                                                                              "Catostomus cf. latipinnis") %in% names(.))]),
           `Cyprinella zanema` = ifelse(all(c("Cyprinella zanema",
                                                  "Cyprinella cf. zanema") %in% names(.)),
                                            `Cyprinella zanema` + `Cyprinella cf. zanema`,
                                        c("Cyprinella zanema",
                                          "Cyprinella cf. zanema")[(c("Cyprinella zanema",
                                                                      "Cyprinella cf. zanema") %in% names(.))]),
           `Noturus leptacanthus` = ifelse(all(c("Noturus leptacanthus",
                                              "Noturus sp. c.f. leptacanthus") %in% names(.)),
                                        `Noturus leptacanthus` + `Noturus sp. c.f. leptacanthus`,
                                        c("Noturus leptacanthus",
                                          "Noturus sp. c.f. leptacanthus")[(c("Noturus leptacanthus",
                                                                              "Noturus sp. c.f. leptacanthus") %in% names(.))]),
           `Moxostoma erythrurum` = ifelse(all(c("Moxostoma erythrurum",
                                                 "Moxostoma sp cf erythrurum") %in% names(.)),
                                           `Moxostoma erythrurum` + `Moxostoma sp cf erythrurum`,
                                           c("Moxostoma erythrurum",
                                             "Moxostoma sp cf erythrurum")[(c("Moxostoma erythrurum",
                                                                              "Moxostoma sp cf erythrurum") %in% names(.))]),
           `Moxostoma lachneri` = ifelse(all(c("Moxostoma lachneri",
                                                 "Moxostoma cf. lachneri") %in% names(.)),
                                           `Moxostoma lachneri` + `Moxostoma cf. lachneri`,
                                           c("Moxostoma lachneri",
                                             "Moxostoma cf. lachneri")[(c("Moxostoma lachneri",
                                                                              "Moxostoma cf. lachneri") %in% names(.))]),
           `Moxostoma poecilurum` = ifelse(all(c("Moxostoma poecilurum",
                                               "Moxostoma cf. poecilurum") %in% names(.)),
                                         `Moxostoma poecilurum` + `Moxostoma cf. poecilurum`,
                                         c("Moxostoma poecilurum",
                                           "Moxostoma cf. poecilurum")[(c("Moxostoma poecilurum",
                                                                        "Moxostoma cf. poecilurum") %in% names(.))]),
           `Moxostoma duquesnei` = ifelse(all(c("Moxostoma duquesnei",
                                                 "Moxostoma duquesnii") %in% names(.)),
                                           `Moxostoma duquesnei` + `Moxostoma duquesnii`,
                                           c("Moxostoma duquesnei",
                                             "Moxostoma duquesnii")[(c("Moxostoma duquesnei",
                                                                            "Moxostoma duquesnii") %in% names(.))]),
           `Etheostoma chlorosoma` = ifelse(all(c("Etheostoma chlorosoma",
                                                "Etheostoma chlorosomum") %in% names(.)),
                                          `Etheostoma chlorosoma` + `Etheostoma chlorosomum`,
                                          c("Etheostoma chlorosoma",
                                            "Etheostoma chlorosomum")[(c("Etheostoma chlorosoma",
                                                                      "Etheostoma chlorosomum") %in% names(.))]),
           `Fundulus stellifer` = ifelse(all(c("Fundulus stellifer",
                                                  "Fundulus stellifera") %in% names(.)),
                                            `Fundulus stellifer` + `Fundulus stellifera`,
                                            c("Fundulus stellifer",
                                              "Fundulus stellifera")[(c("Fundulus stellifer",
                                                                           "Fundulus stellifera") %in% names(.))]),
           `Lepomis gulosus` = ifelse(all(c("Lepomis gulosus",
                                               "Chaenobryttus gulosus") %in% names(.)),
                                         `Lepomis gulosus` + `Chaenobryttus gulosus`,
                                         c("Lepomis gulosus",
                                           "Chaenobryttus gulosus")[(c("Lepomis gulosus",
                                                                       "Chaenobryttus gulosus") %in% names(.))]),
           `Notropis dorsalis` = ifelse(all(c("Notropis dorsalis",
                                               "Hybopsis dorsalis") %in% names(.)),
                                         `Notropis dorsalis` + `Hybopsis dorsalis`,
                                         c("Notropis dorsalis",
                                           "Hybopsis dorsalis")[(c("Notropis dorsalis",
                                                                   "Hybopsis dorsalis") %in% names(.))]),
           `Cyprinella zanema` = ifelse(all(c("Cyprinella zanema",
                                               "Hybopsis zanema") %in% names(.)),
                                         `Cyprinella zanema` + `Hybopsis zanema`,
                                         c("Cyprinella zanema",
                                           "Hybopsis zanema")[(c("Cyprinella zanema",
                                                                 "Hybopsis zanema") %in% names(.))])) %>%
    rowwise() %>%
    mutate(`Oncorhynchus clarkii` = sum(c_across(contains("Oncorhynchus clarki"))),
           `Esox americanus` = sum(c_across(contains("Esox americanus"))),
           `Oncorhynchus mykiss` = sum(c_across(contains("Oncorhynchus mykiss")))
    ) %>%
    ungroup() %>%
    relocate(`Oncorhynchus clarkii`, .before = `Oncorhynchus clarki virginalis`) %>%
    relocate(`Esox americanus`, .before = `Esox americanus americanus`) %>%
  dplyr::select(-c(tidyselect::contains("Esox americanus ")),
                -c(tidyselect::contains(" sp.")),
                -c(tidyselect::contains("Oncorhynchus clarkii ")),
                -tidyselect::any_of(c("Cottus bairdi",
                                     "Macrhybopsis cf. aestivalis",
                                     "Notropis cf. spectrunculus",
                                     "Catostomus cf. latipinnis",
                                     "Cyprinella cf. zanema",
                                     "Noturus sp. c.f. leptacanthus",
                                     "Moxostoma cf. poecilurum",
                                     "Moxostoma cf. lachneri",
                                     "Moxostoma sp cf erythrurum",
                                     #remove clinch sculpin (undescribed spp)
                                     "Cottus cf. broadband sculpin",
                                     "Oncorhynchus clarki virginalis",
                                     "Oncorhynchus mykiss gairdneri",
                                     "Moxostoma duquesnii",
                                     "Etheostoma chlorosomum",
                                     "Fundulus stellifera",
                                     "Hybopsis zanema",
                                     "Hybopsis dorsalis",
                                     "Chaenobryttus gulosus"
                                     )))  %>%
    mutate(FISH_PROTOCOL = ifelse(FISH_PROTOCOL == "SM_WADEABLE",
                                  "Small Wadeable",
                                  ifelse(FISH_PROTOCOL == "BOATABLE",
                                         "Boatable",
                                         "Large Wadeable")))



  if(!isTRUE(hybrids)) {
    full_fish <- full_fish %>%
      dplyr::select(-c(tidyselect::contains(" x "), -c(tidyselect::contains(".x."))))

  }


  return(data.frame(full_fish))

}
