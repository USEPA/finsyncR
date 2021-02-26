#' Access clean USGS Macro-Invertebrate Dataset
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"AlgalGroup"}, \code{"Phylum"}, \code{"Class"}, \code{"Order"},
#'   \code{"Family"}, \code{"Genus"}, or \code{"Species"}.
#' @param program The program name(s) that should be included in the output
#'   dataset. See \code{Details} below for more information.
#' @param lifestage logical. Should the output dataset should include lifestage information for
#'   each individual? \code{TRUE} or \code{FALSE}.
#' @param abunMeasure Measure of abundance, either \code{"abundance"} or
#'   \code{"density"}.
#'
#' @return A species by sample data frame with site, stream reach, and
#'   sample information.
#'
#' @details Note: There are 81 sampling events (sampling location - collection
#'   date) with replicate samples (not duplicates). We have left these replicate
#'   samples in the dataset, because some replicates are different in the
#'   stream habitat sampled, which may be of interest for certain ecological
#'   questions.
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
#'   "Cooperative Water Program")} for both NAWQA and Cooperative Water Programs)
#'
#'   If \code{dataType = "abun"} AND \code{abunMeasure = "density"},
#'   ~0.5% of samples (22 samples) removed from the dataset, because area
#'   sampled was not provided for these samples.
#'
#' @examples
#' \dontrun{
#' Inverts <- getInvertData(taxonLevel = "Family")
#'
#' }
#'
#' @export

getInvertData <- function(dataType = "abun",
                          taxonLevel = "Species",
                          program = "National Water Quality Assessment",
                          lifestage = FALSE,
                          abunMeasure = "density"){
  if(!(abunMeasure %in% c("density", "abundance"))) {
    stop('abunMeasure must be either "density" or "abundance".')}

  if(!(dataType %in% c("abun", "occur"))) {
    stop('dataType must be either "abun" or "occur".')}

  if(!(taxonLevel %in% .TaxLevCols_Inverts$Phylum$taxcols)){
    stop(paste('taxonLevel must be set between ranks "Phylum" and "Subspecies";',
         'see "Details" in ?getInvertData.'))
  }

  if(dataType == "abun" && abunMeasure == "density"){
    message(paste('22 samples are being dropped from full dataset, because area',
            'samples was missing for these samples and density could not be',
            'estimated.'))
  }

  if(lifestage != TRUE && lifestage != FALSE){
    stop('lifestage must be set to either TRUE or FALSE.')
  }

  Inverts <- utils::read.csv(unzip(system.file("extdata",
                                               "InvertResults.zip",
                                               package = "StreamData")),
                      colClasses = c("SiteNumber" = "character"))
  if(colnames(Inverts)[1] != "SIDNO"){
    colnames(Inverts)[1] = "SIDNO"
  }
  Project <- utils::read.csv(system.file("extdata",
                                    "20201217.0749.Project.csv",
                                    package = "StreamData"),
                        comment.char="#")
  if(program == "ALL") {
    database <- c("National Water Quality Assessment",
                  "Cooperative Water Program",
                  "Collection of Basic Records",
                  "Other Federal Agencies")
  } else {database <- program }


  Inverts <- Inverts %>%
    dplyr::filter(ProjectLabel %in% (Project %>%
                                       dplyr::filter(Program %in% database) %>%
                                       dplyr::distinct(ProjectLabel,
                                                       .keep_all = FALSE))[ , "ProjectLabel"])

  SamplingRatio_SamplerType <- .SamplingRatio_SamplerType

<<<<<<< HEAD
  Inverts <- dplyr::left_join(Inverts,
=======
  Inverts <- left_join(Inverts,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                       SamplingRatio_SamplerType,
                       by = "LabRecordID")

  ### Have to sum invertebrate abundance of duplicate species entries for
  ### all samplers
  ## Folsom samplers split the sample repeatedly, and count ***ALL*** individuals
  ## towards the abundance measures
  ## So, this first step in determining 'Abundance' is to sum (e.g., collapse) all
  ## entries for a given species at each collection site for a single
  ## 'Abundance' measure

  ## "IRTH" == The RTH theoretically supports the faunistically
  ## richest invertebrate community and is typically represented by a
  ## coarse-grained riffle or a woody snag. The semi-quantitative RTH sample
  ## consists of a series of discrete collections (for example, five Slack
  ## samples or 10 woody snag sections) that are processed and combined
  ## into a single composited sample.

  ## 'FieldComponent' "M" refers to the "Main-body sample"
  ## Convert 'CollectionDate' to date format from factor
  ## Added in a distinct 'SIDNO' (sample number) x 'SiteNumber' x 'CollectionDate'
  ## identifier to each row, Easier to sort on this later

  ## Based on Devin Jones's notes, the biodata data dictionary, and my
  ## understanding of the datasets, I believe it is best to go with
  ## "PublishedTaxonName" as the basis for the "SampleGrouping".


  Inverts <- Inverts %>%
<<<<<<< HEAD
    dplyr::filter(SampleTypeCode %in% c("IRTH", "BERW")) %>%
    dplyr::filter(FieldComponent == "M") %>%
    dplyr::mutate(CollectionDate = as.Date(CollectionDate,
=======
    filter(SampleTypeCode %in% c("IRTH", "BERW")) %>%
    filter(FieldComponent == "M") %>%
    mutate(CollectionDate = as.Date(CollectionDate,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                                    format = "%m/%d/%Y"),
           Identifier = paste(SIDNO,
                              SiteNumber,
                              CollectionDate, sep = "_"),
           SampleGrouping = paste(SIDNO,
                                  SiteNumber,
                                  CollectionDate,
                                  PublishedTaxonName,
                                  Lifestage,
                                  sep = "_" ))

  ### *** It is at this stage, we COULD filter for a given 'Lifestage'
  ### ( (blank), L [larvae], P [pupae], A [adult]);
  ### we will leave in ALL 'Lifestage' at this moment***
  ### *** We will HAVE TO include 'Lifestage' within our unique identifiers for
  ### each site, as not to lose this level of detail for analyses***


  ## We now have to clean the data for sites that used a Folsom Sampler that
  ## identified invertebrates with multiple 'LabSubsamplingRatio'
  ### IF n_distinct(LabSubsamplingRatio) == 1, then the sample was either fully
  ### counted or only the subsample was entered (WHICH IS WHAT WE WANT)
  ### IF n_distinct(LabSubsamplingRatio) >= 2, then the sample was split multiple
  ### times (FS), or includes "lab large rare" individuals (e.g., species entry
  ### will include 1:1 and other subsampled ratio) on a Gridded tray
  ## So we first filter all sites for those with >= 2 'LabSubsamplingRatio'
  Invert_MixedRatios <- Inverts %>%
<<<<<<< HEAD
    dplyr::group_by(Identifier) %>%
    dplyr::filter(n_distinct(LabSubsamplingRatio) >= 2) %>%
    dplyr::ungroup()

  Invert_SingleRatios <- suppressMessages({dplyr::anti_join(Inverts, Invert_MixedRatios) %>%
      dplyr::mutate(DatasetPortion = "SingleRatios")})
=======
    group_by(Identifier) %>%
    filter(n_distinct(LabSubsamplingRatio) >= 2) %>%
    ungroup()

  Invert_SingleRatios <- suppressMessages({anti_join(Inverts, Invert_MixedRatios) %>%
    mutate(DatasetPortion = "SingleRatios")})
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7

  SingleRatio_Duplicates <-
    Invert_SingleRatios[duplicated(Invert_SingleRatios$SampleGrouping) |
                          duplicated(Invert_SingleRatios$SampleGrouping,
                                     fromLast = TRUE), ] %>%
<<<<<<< HEAD
    dplyr::group_by(SampleGrouping) %>%
    dplyr::mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    dplyr::ungroup()


  SumSingleRatioData = suppressWarnings({SingleRatio_Duplicates %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::mutate(SummedAbundance = sum(Abundance)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Abundance = SummedAbundance,
=======
    group_by(SampleGrouping) %>%
    mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    ungroup()


  SumSingleRatioData = suppressWarnings({SingleRatio_Duplicates %>%
    group_by(SampleGrouping) %>%
    mutate(SummedAbundance = sum(Abundance)) %>%
    ungroup() %>%
    group_by(SampleGrouping) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Abundance = SummedAbundance,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
           Density_m2 = as.numeric("NA"),
           AdjRawCount = as.numeric("NA"),
           RawCount = as.numeric("NA"),
           TotAreaSampled_m2 = as.numeric("NA"),
           FieldSplitRatio = as.numeric("NA"),
           Note = "Abundance measure is summed 'Abundance' across multiple inputs
         (see 'LabRecordsIDs'); Likely due to changed identification
         (e.g., BenchNotes)",
           DatasetPortion = "SummedAbundance_SingleRatios") %>%
    dplyr::select(-SummedAbundance)})

<<<<<<< HEAD
  Corrected_SingleRatios <- suppressMessages({dplyr::bind_rows((dplyr::anti_join(Invert_SingleRatios,
                                                 SingleRatio_Duplicates) %>%
                                                   dplyr::mutate(DatasetPortion =
=======
  Corrected_SingleRatios <- suppressMessages({bind_rows((anti_join(Invert_SingleRatios,
                                                 SingleRatio_Duplicates) %>%
                                         mutate(DatasetPortion =
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                                                  "NonDuplicate_SingleRatio")),
                                      SumSingleRatioData)})

  ### For those with "1:1 FS + Grid ratio", a "lab-large rare" individual was
  ### recorded following a gridded tray subsample
  ### We have to remove that biased individual(s) from the abundance data,
  ### as they are identified and recorded from the the remaining sample outside
  ### the selected number of gridded cells
  ### Permits us to examine those sites with different sampler types
  ### (e.g., n_distinct(SamplerType) == 2, which is 1:1 FS + Grid)

  ### Extracts the portion of the site sample that was correctly sampled
  ### as part of the gridded tray
  ### *** THIS DATASET IS THE CORRECT GRIDDED SUBSAMPLE INFORMATION WHICH NEEDS
  ### TO BE COMBINED WITH SUMMED FOLSOM SAMPLER DATA***
  ### (CORRECTED: NEED TO SEARCH FOR DUPLICATE LISTINGS IN THIS DATASET AS WELL)

  Invert_MixedRatios_GridsAndFS <- Invert_MixedRatios %>%
<<<<<<< HEAD
    dplyr::group_by(Identifier) %>%
    dplyr::filter(dplyr::n_distinct(SamplerType) == 2) %>%
    dplyr::ungroup()

  Invert_MixedSamplerType_GridOnly_LLRRemoved <- Invert_MixedRatios_GridsAndFS %>%
    dplyr::group_by(Identifier) %>%
    dplyr::filter(SamplerType == "Grid") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DatasetPortion = "Grid_LRRRemoved")
=======
    group_by(Identifier) %>%
    filter(n_distinct(SamplerType) == 2) %>%
    ungroup()

  Invert_MixedSamplerType_GridOnly_LLRRemoved <- Invert_MixedRatios_GridsAndFS %>%
    group_by(Identifier) %>%
    filter(SamplerType == "Grid") %>%
    ungroup() %>%
    mutate(DatasetPortion = "Grid_LRRRemoved")
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7

  ## Now that we have all the gridded portion of samples
  ## we can check the subsampled gridded samples for any duplicates
  Gridded_LLRRemoved_Duplicates <-
    Invert_MixedSamplerType_GridOnly_LLRRemoved[
      duplicated(Invert_MixedSamplerType_GridOnly_LLRRemoved$SampleGrouping) |
        duplicated(Invert_MixedSamplerType_GridOnly_LLRRemoved$SampleGrouping,
                   fromLast = TRUE), ]

  ## Add identifiers so we can see which columns were collapsed
  ### Now that we have all duplicated 'SingleRatio' samples,
  ### we have to combine the 'Abundance' measures
  ### (per communication with Scott Grotheer 6-17-19: 'BenchNotes' of duplicated
  ### entries indicate changed identification/sample type of specimens;
  ### can combine either 'Abundance' or 'RawCount')
  ### In order to know which samples are being grouped, we first need to identify
  ### which 'LabRecordID' are included in the summed 'Abundance' values
  ### This enables us to track which samples would be included in this measure

  ### We can also add the unique 'Ratio' included in the duplicates
  ### (which should be the same for all duplicates)
  ### Now we can the 'LabRecord_Labels' and 'Ratio_Labels' to the duplicated data

  SumGridLLRData <- Gridded_LLRRemoved_Duplicates %>%
<<<<<<< HEAD
    dplyr::group_by(SampleGrouping) %>%
    dplyr::mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DatasetPortion = "Gridded_LLRRemoved_Duplicates")
=======
    group_by(SampleGrouping) %>%
    mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    ungroup() %>%
    mutate(DatasetPortion = "Gridded_LLRRemoved_Duplicates")
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7

  ## Moving the numeric values of each row to the end of the dataset to better
  ## visualize the changes we're making
  ## Removed columns from the dataset that we have created values for
  ## (e.g., LabRecord_Labels, Ratio_Labels)
  ## or served their purpose (e.g., Proprietary, SamplerRatio_SamplerType)
  ## Groups the unique 'SampleGrouping' and sums the 'Abundance' data for each
  ## Adds the 'SummedAbundance' value to each row based on the 'SampleGrouping' ID
  ## Removes the first row of each unique 'SampleGrouping'; Takes a single row,
  ## which we will then place the 'SummedAbundance' value in
  ## To not overwrite the previous step so we can always modify later
  ## Replacing the 'Abundance' value with the 'SummedAbundance' value


  SumGridLLRData2 = suppressWarnings({SumGridLLRData %>%
    dplyr::select(-"LabRecordID", -"NWQLSubsamplingCode", -"Ratio",
                  -"X", -"NumbEntries") %>%
<<<<<<< HEAD
      dplyr::group_by(SampleGrouping) %>%
      dplyr::mutate(SummedAbundance = sum(Abundance)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Abundance = SummedAbundance,
=======
    group_by(SampleGrouping) %>%
    mutate(SummedAbundance = sum(Abundance)) %>%
    ungroup() %>%
    group_by(SampleGrouping) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Abundance = SummedAbundance,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
           Density_m2 = as.numeric("NA"),
           AdjRawCount = as.numeric("NA"),
           RawCount = as.numeric("NA"),
           TotAreaSampled_m2 = as.numeric("NA"),
           FieldSplitRatio = as.numeric("NA"),
           Note = "Abundance measure is summed 'Abundance' across multiple inputs
         (see 'LabRecordsIDs'); Likely due to changed identification
         (e.g., BenchNotes)",
           DatasetPortion = "SummedAbundance_Gridded_LLRRemoved") %>%
    dplyr::select(-SummedAbundance)})

  ## Now that we have the corrected summed values for the
  ## 'Gridded_LLRRemoved_Duplicates', we need to add these rows to the
  ## non-duplicated rows within the 'SingleRatio' portions
<<<<<<< HEAD
  Corrected_Gridded_LLRRemoved <- suppressMessages({dplyr::bind_rows(
    (dplyr::anti_join(Invert_MixedSamplerType_GridOnly_LLRRemoved,
               Gridded_LLRRemoved_Duplicates) %>%
       dplyr::mutate(DatasetPortion =
=======
  Corrected_Gridded_LLRRemoved <- suppressMessages({bind_rows(
    (anti_join(Invert_MixedSamplerType_GridOnly_LLRRemoved,
               Gridded_LLRRemoved_Duplicates) %>%
       mutate(DatasetPortion =
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                "NonDuplicate_Gridded_LLRRemoved")),
    SumGridLLRData2)})

  ## We now have to clean the data for sites that used a Folsom Sampler that
  ## identified invertebrates with multiple 'LabSubsamplingRatio'
  ### IF n_distinct(LabSubsamplingRatio) == 1, then the sample was either
  ###  fully counted or only the subsample was entered (WHICH IS WHAT WE WANT)
  ### IF n_distinct(LabSubsamplingRatio) >= 2, then the sample was split multiple
  ###  times (FS), or includes "lab large rare" individuals (e.g., species entry
  ###  will include 1:1 and other subsampled ratio) on a Gridded tray
  ### So we first filter all sites for those with >= 2 'LabSubsamplingRatio'
  ### (duplicate STEP 6 action)
  ## Now that we have these ***FOLSOM SAMPLER ONLY*** samples,
  ## we can combine the 'Abundance' data for duplicate species entries
  ### We first have to create a unique variable that encompasses
  ### each site-by-collection-date-by-biota-by-life-stage combination

  Invert_MixedRatios_FolsomSamplerOnly_AntiJoin <- suppressMessages({Inverts %>%
<<<<<<< HEAD
      dplyr::group_by(SIDNO, SiteNumber, CollectionDate) %>%
      dplyr::filter(n_distinct(LabSubsamplingRatio) >= 2) %>%
      dplyr::anti_join(Invert_MixedRatios_GridsAndFS)})
=======
    group_by(SIDNO, SiteNumber, CollectionDate) %>%
    filter(n_distinct(LabSubsamplingRatio) >= 2) %>%
    anti_join(Invert_MixedRatios_GridsAndFS)})
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7

  ## Now we can reduce the dataset to duplicate entries
  ## We will include both "from the first duplicate" and
  ## "from the last duplicate" to bookend each duplicate entry
  Invert_MixedRatios_Folsom_AntiJoin_Duplicates <-
    Invert_MixedRatios_FolsomSamplerOnly_AntiJoin[
      duplicated(Invert_MixedRatios_FolsomSamplerOnly_AntiJoin$SampleGrouping)|
        duplicated(Invert_MixedRatios_FolsomSamplerOnly_AntiJoin$SampleGrouping,
                   fromLast = TRUE), ]

  ## Now that we have all duplicated 'FS' samples, we have to combine the
  ## 'Abundance' measures (as per communication with Scott Grotheer 5-3-19:
  ## "always is a pretty BIG word, but I think that (summing 'Abundance')
  ## is a safe way to go")
  ### In order to know which samples are being grouped, we first need to
  ### identify which 'LabRecordID' are included in the summed 'Abundance'
  ### This enables us to track samples to be included in this summed measure

  Invert_MixedRatios_Folsom_AntiJoin_Duplicates <-
    Invert_MixedRatios_Folsom_AntiJoin_Duplicates  %>%
<<<<<<< HEAD
    dplyr::group_by(SampleGrouping) %>%
    dplyr::mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    dplyr::ungroup()
=======
    group_by(SampleGrouping) %>%
    mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    ungroup()
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7

  ## Moving the numeric values of each row to the end of the dataset to better
  ## visualize the changes we're making
  ## Removed columns from the dataset that we have created values for
  ## (e.g., LabRecord_Labels, Ratio_Labels) or served their purpose
  ## (e.g., Proprietary, SamplerRatio_SamplerType)
  ## Groups the unique 'SampleGrouping' and sums the 'Abundance' data
  ## for each group of values
  ## Adds the 'SummedAbundance' value to each row based on the 'SampleGrouping' ID
  ## Removes the first row of each unique 'SampleGrouping';
  ## Takes a single row, which we will then place the 'SummedAbundance' value in

  SumData <- suppressWarnings({Invert_MixedRatios_Folsom_AntiJoin_Duplicates %>%
    dplyr::select(-"LabRecordID", -"NWQLSubsamplingCode", -"Ratio",
                  -"X", -"NumbEntries") %>%
<<<<<<< HEAD
      dplyr::group_by(SampleGrouping) %>%
      dplyr::mutate(SummedAbundance = sum(Abundance)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Abundance = SummedAbundance,
=======
    group_by(SampleGrouping) %>%
    mutate(SummedAbundance = sum(Abundance)) %>%
    ungroup() %>%
    group_by(SampleGrouping) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Abundance = SummedAbundance,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
           Density_m2 = as.numeric("NA"),
           AdjRawCount = as.numeric("NA"),
           RawCount = as.numeric("NA"),
           TotAreaSampled_m2 = as.numeric("NA"),
           FieldSplitRatio = as.numeric("NA"),
           Note = "Abundance measure is summed 'Abundance' across multiple
         folsom sampler inputs (see 'LabRecordsIDs')",
           DatasetPortion = "SummedAbundance_FolsomSampler") %>%
    dplyr::select(-SummedAbundance)})

<<<<<<< HEAD
  Corrected_MixedRatios_FolsomSamplerOnly <- suppressMessages({dplyr::bind_rows((
    dplyr::anti_join(Invert_MixedRatios_FolsomSamplerOnly_AntiJoin,
              Invert_MixedRatios_Folsom_AntiJoin_Duplicates,
              by = "SampleGrouping") %>%
      dplyr::mutate(DatasetPortion = "NonDuplicate_FolsomSampler")),
=======
  Corrected_MixedRatios_FolsomSamplerOnly <- suppressMessages({bind_rows((
    anti_join(Invert_MixedRatios_FolsomSamplerOnly_AntiJoin,
              Invert_MixedRatios_Folsom_AntiJoin_Duplicates,
              by = "SampleGrouping") %>%
      mutate(DatasetPortion = "NonDuplicate_FolsomSampler")),
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
    SumData)})

  TotalRows <- do.call(rbind,list(Corrected_MixedRatios_FolsomSamplerOnly,
                                  Corrected_Gridded_LLRRemoved,
                                  Corrected_SingleRatios))

  ###The above code, hypothetically, could be removed to a separate, hidden
  ## function. Would take a little bit of work, but could easily be done.

  ###At this point, we need to join information on stream, site, and sample (?)
  ## information
  ### Need to get sampled area from the Sample dataset
  ###Then, we need to pivot_wider, filter based on taxon level, remove excess
  ## columns, and change the data type
  ###Can use code from the "getAlgaeData" function to work on this portion.
  ###Need to add a section for Lifestage "T/F"; if true count separately, if false count together

  invertsamp = utils::read.csv(system.file("extdata",
                                    "20201217.0749.InvertSamp.csv",
                                    package = "StreamData"),
                        colClasses = c("SiteNumber" = "character")) %>%
    dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
    dplyr::select(SIDNO,
                  SiteNumber,
                  AreaSampTot_m2,
                  GeomorphicChannelUnit,
                  ChannelBoundaries,
                  ChannelFeatures)

  invertsampinv = utils::read.csv(system.file("extdata",
                                       "20201217.0749.SampleInv.csv",
                                       package = "StreamData"),
                           colClasses = c("SiteNumber" = "character")) %>%
    dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
    dplyr::select(SIDNO,
                  ReplicateType)


  invertsite = utils::read.csv(system.file("extdata",
                                    "20201217.0749.SiteInfo.csv",
                                    package = "StreamData"),
                        colClasses = c("SiteNumber" = "character")) %>%
    dplyr::select(SiteNumber,
                  Latitude_dd,
                  Longitude_dd,
                  CoordinateDatum,
                  HUCCode,
                  DrainageArea_mi2,
                  SiteTypeName,
                  CountyFIPSCode,
                  StateFIPSCode)

<<<<<<< HEAD
  invertsampinfo = dplyr::left_join(dplyr::left_join(invertsamp,
=======
  invertsampinfo = left_join(left_join(invertsamp,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                                       invertsampinv,
                                       by = "SIDNO"),
                             invertsite,
                             by = "SiteNumber") %>%
    dplyr::select(-SiteNumber) %>%
<<<<<<< HEAD
    dplyr::mutate(CountyFIPSCode = sprintf("%03d", CountyFIPSCode),
           StateFIPSCode = sprintf("%02d", StateFIPSCode))

  TotalRows = dplyr::left_join(TotalRows,
=======
    mutate(CountyFIPSCode = sprintf("%03d", CountyFIPSCode),
           StateFIPSCode = sprintf("%02d", StateFIPSCode))

  TotalRows = left_join(TotalRows,
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                        invertsampinfo,
                        by = "SIDNO")

  if(abunMeasure == "abundance"){
    notAbun <- "Density_m2"
  } else {
    notAbun <- "Abundance"
    abunMeasure <- "Density_m2"}

  mycols = .TaxLevCols_Inverts[[which(names(.TaxLevCols_Inverts) == taxonLevel)]]$mycols
  taxcols = .TaxLevCols_Inverts[[which(names(.TaxLevCols_Inverts) == taxonLevel)]]$taxcols


  ###Need to fix this here with abundance v. density!

  if(isTRUE(lifestage)) {
    #Lifestage-taxon combinations
    invert_comms1 = TotalRows %>%
<<<<<<< HEAD
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
      dplyr::filter_at(vars(all_of(taxonLevel)), any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(SIDNO, all_of(taxonLevel), Lifestage), sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(Abundance = sum(Abundance)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Density_m2 = ifelse(is.na(AreaSampTot_m2),
=======
      filter(PublishedTaxonNameLevel %in% taxcols) %>%
      filter_at(vars(all_of(taxonLevel)), any_vars(. != "")) %>%
      unite(UNIQUEID, c(SIDNO, all_of(taxonLevel), Lifestage), sep = "_", remove = FALSE) %>%
      group_by(UNIQUEID) %>%
      mutate(Abundance = sum(Abundance)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(Density_m2 = ifelse(is.na(AreaSampTot_m2),
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                                 NA,
                                 Abundance / AreaSampTot_m2)) %>%
      dplyr::select(-any_of(c("LabOrderID", "LabRecordID", "FieldComponent",
                              "LabComponent", "LabProcName",
                              "TaxonomicResultReviewStatus",
                              "PublishedSortOrder", "BioDataTaxonName", "BioDataShortName",
                              "BenchTaxonName", "BenchTaxonNameReferenceCode",
                              "AdjRawCount", "RawCount",
                              "FieldSplitRatio", "LabSubsamplingRatio", "UniqueTaxonFlag",
                              "TargetLevelNotReachedReason", "Artifact", "BenchNotes",
                              "TaxonRecordSource", "IdentificationDate",
                              "VerificationEntity", "VerificationDate", "CurationEntity",
                              "CurationDate", 'ITIS_TSN', "ITIS_MatchCode", "PublishedTaxonName",
                              "PublishedTaxonNameAuthority", "ScientificName",
                              "TaxonVersionNumber", "NWQLSubsamplingCode",
                              "Ratio", 'X', "NumbEntries", "SampleGrouping", "LabRecordIDs",
                              "Ratios", "Note", "UNIQUEID", "PublishedTaxonNameLevel",
                              "Identifier", "ReleaseCategory", "SampleTypeCode",
                              "ProjectAssignedSampleLabel", "SiteVisitSampleNumber"))) %>%
      dplyr::select(-any_of(mycols),
                    -any_of(notAbun)) %>%
<<<<<<< HEAD
      tidyr::unite(Taxon_Life, c(all_of(taxonLevel), Lifestage), sep = "_") %>%
      tidyr::pivot_wider(names_from = all_of(Taxon_Life),
=======
      unite(Taxon_Life, c(all_of(taxonLevel), Lifestage), sep = "_") %>%
      pivot_wider(names_from = all_of(Taxon_Life),
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                  names_prefix = "tax_",
                  values_from = all_of(abunMeasure),
                  values_fill = 0)

  } else {
    #All species are one
    invert_comms1 = TotalRows %>%
<<<<<<< HEAD
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
      dplyr::filter_at(vars(all_of(taxonLevel)), any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(SIDNO, all_of(taxonLevel)), sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(Abundance = sum(Abundance)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Density_m2 = ifelse(is.na(AreaSampTot_m2),
                                 NA,
                                 Abundance / AreaSampTot_m2)) %>%
      dplyr::select(-tidyselect::any_of(c("LabOrderID", "LabRecordID", "FieldComponent",
=======
      filter(PublishedTaxonNameLevel %in% taxcols) %>%
      filter_at(vars(all_of(taxonLevel)), any_vars(. != "")) %>%
      unite(UNIQUEID, c(SIDNO, all_of(taxonLevel)), sep = "_", remove = FALSE) %>%
      group_by(UNIQUEID) %>%
      mutate(Abundance = sum(Abundance)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(Density_m2 = ifelse(is.na(AreaSampTot_m2),
                                 NA,
                                 Abundance / AreaSampTot_m2)) %>%
      dplyr::select(-any_of(c("LabOrderID", "LabRecordID", "FieldComponent",
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                              "LabComponent", "LabProcName",
                              "TaxonomicResultReviewStatus",
                              "PublishedSortOrder", "BioDataTaxonName", "BioDataShortName",
                              "BenchTaxonName", "BenchTaxonNameReferenceCode",
                              "AdjRawCount", "RawCount",
                              "FieldSplitRatio", "LabSubsamplingRatio", "UniqueTaxonFlag",
                              "TargetLevelNotReachedReason", "Artifact", "BenchNotes",
                              "TaxonRecordSource", "IdentificationDate",
                              "VerificationEntity", "VerificationDate", "CurationEntity",
                              "CurationDate", 'ITIS_TSN', "ITIS_MatchCode", "PublishedTaxonName",
                              "PublishedTaxonNameAuthority", "ScientificName",
                              "TaxonVersionNumber", "NWQLSubsamplingCode",
                              "Ratio", 'X', "NumbEntries", "SampleGrouping", "LabRecordIDs",
                              "Ratios", "Note", "Lifestage", "UNIQUEID", "PublishedTaxonNameLevel",
                              "SamplerType", "DatasetPortion"))) %>%
<<<<<<< HEAD
      dplyr::select(-tidyselect::any_of(mycols)) %>%
      dplyr::select(-tidyselect::any_of(notAbun)) %>%
      tidyr::pivot_wider(names_from = all_of(taxonLevel),
=======
      dplyr::select(-any_of(mycols)) %>%
      dplyr::select(-any_of(notAbun)) %>%
      pivot_wider(names_from = all_of(taxonLevel),
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                  names_prefix = "tax_",
                  values_from = all_of(abunMeasure),
                  values_fill = 0)
  }

  if(dataType == "Occur") {
    invert_comms1 = invert_comms1 %>%
<<<<<<< HEAD
      dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
=======
      mutate(across(contains("tax_"),
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
                    ~replace(., . > 0, 1)))
  }

  colnames(invert_comms1) = sub("tax_", "", colnames(invert_comms1))


  invert_comms1 = invert_comms1 %>%
    dplyr::select(-SiteVisitSampleNumber,
                  -Identifier,
                  -SIDNO,
                  -ReleaseCategory,
                  -TotAreaSampled_m2) %>%
<<<<<<< HEAD
    dplyr::relocate(tidyselect::any_of(.ReorderUSGSBioDataColNames))
=======
    relocate(any_of(.ReorderUSGSBioDataColNames))
>>>>>>> f8d3df37c7a32a472e8809cf986b05e0440051b7
  return(data.frame(invert_comms1))

}
