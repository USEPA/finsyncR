#' Access clean USGS Macro-Invertebrate Dataset
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"AlgalGroup"}, \code{"Phylum"}, \code{"Class"}, \code{"Order"},
#'   \code{"Family"}, \code{"Genus"}, or \code{"Species"}.
#' @param taxonFix How to deal with changes in taxonomy across time, must
#'   be one of: \code{"none"}, \code{"lump"}, \code{"remove"}.
#' @param program The program name(s) that should be included in the output
#'   dataset. See \code{Details} below for more information.
#' @param lifestage logical. Should the output dataset should include lifestage information for
#'   each individual? \code{TRUE} or \code{FALSE}.
#' @param abunMeasure Measure of abundance, either \code{"abundance"} or
#'   \code{"density"}.
#' @param rarefy logical. Should samples be standardized by the number of individuals
#'   identified within each sample? \code{TRUE} or \code{FALSE}. See \code{Details}
#'   below for more information.
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
#'   The function adjusts taxa abundances from samples for lab subsampling
#'   ratio. As a result, duplicate taxa were combined (abundances summed)
#'   following adjustments for lab subsampling.
#'
#'   Currently, lab large rares are removed from all samples.
#'
#'   \code{taxonFix} provides ways to handle changes in taxonomy across time,
#'   especially in instances in which species have been reorganized into new
#'   genera. \code{taxonFix} operates on the genera level. \code{taxonFix = "none"}
#'   makes no adjustment. \code{taxonFix = "lump"} prioritizes retaining observations
#'   by giving a unified genera name to all species and genera that have been linked
#'   through changes in taxonomy (e.g. genera1/genera2/genera3). Note: of 98
#'   problematic genera that exist throughout all BioData, \code{taxonFix = "lump"}
#'   results in 13 "lumped" genera. Most new "lumped" genera are <5 "old" genera joined.
#'   One "lumped" genera (within Ephemeroptera) includes 70 "old" genera.
#'   \code{taxonFix = "drop"} prioritizes accurate identification by dropping
#'   observations from problematic genera that do not have species identification.
#'   Without a species-level identification from the bench, there is no way to
#'   assure correct membership in a updated genera.
#'
#'   \code{program} refers to the local, regional, or national program project
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
#'   ~0.5 \% of samples (22 samples) removed from the dataset, because area
#'   sampled was not provided for these samples.
#'
#'   If \code{rarefy = TRUE}, only samples with 300+ individuals identified (RawCount)
#'   will be retained. Thus, ~17 \% of samples will be removed, as they have <300
#'   individuals sampled. We set the rarefaction threshold at 300, because 1) with
#'   every 50 individuals identified, ~1 genera are added to the sample and 2) 82.8 \%
#'   of samples have at least 300 individuals identified. Thus, lowering the threshold
#'   to 200 individuals removed ~2 genera per sample, but only added an additional
#'   7.3 \% of samples included (90.1 \% from 82.8 \%). Similarly, increasing the
#'   threshold to 400 individuals added ~2 genera per sample, but reduced samples to
#'   30.3 \% of all samples. Use \code{set.seed()} to get consistent output of
#'   community data. NOTE: \code{rarefy = TRUE} can be used when wanting
#'   occurrence data (presence/absence) OR proportional data (each taxon represents
#'   a certain percent of a sample). Use \code{rarefy = FALSE} when densities are
#'   the measure that you are interested in using.
#'
#' @examples
#' \dontrun{
#' Inverts <- getInvertData(taxonLevel = "Family")
#'
#' set.seed(1)
#' RarefyInverts <- getInvertData(taxonLevel = "Genus",
#'                                rarefy = TRUE)
#' }
#'
#' @export

getInvertData <- function(dataType = "abun",
                          taxonLevel = "Genus",
                          taxonFix = "none",
                          program = "National Water Quality Assessment",
                          lifestage = FALSE,
                          abunMeasure = "density",
                          rarefy = TRUE){
  if(!(abunMeasure %in% c("density", "abundance"))) {
    stop('abunMeasure must be either "density" or "abundance".')}

  if(!(dataType %in% c("abun", "occur"))) {
    stop('dataType must be either "abun" or "occur".')}

  if(!(taxonLevel %in% StreamData:::.TaxLevCols_Inverts$Phylum$taxcols)){
    stop(paste('taxonLevel must be set between ranks "Phylum" and "Subspecies";',
         'see "Details" in ?getInvertData.'))
  }
  if(!(taxonFix %in% c("none", "lump","remove"))){
    stop(paste("Provide taxonFix as 'none' (do nothing) or 'lump'",
    "(lump genera across years) or 'remove' (remove observations if",
    "no species level ID given)"))
  }

  if(dataType == "abun" && abunMeasure == "density"){
    message(paste('22 samples are being dropped from full dataset, because area',
            'samples was missing for these samples and density could not be',
            'estimated.'))
  }

  if(lifestage != TRUE && lifestage != FALSE){
    stop('lifestage must be set to either TRUE or FALSE.')
  }

  if(rarefy != TRUE && rarefy != FALSE){
    stop('rarefy must be set to either TRUE or FALSE.')
  }

  Inverts <- utils::read.csv(unzip(system.file("extdata",
                                               "InvertResults.zip",
                                               package = "StreamData")),
                      colClasses = c("SiteNumber" = "character"),
                      stringsAsFactors = FALSE)
  if(colnames(Inverts)[1] != "SIDNO"){
    colnames(Inverts)[1] = "SIDNO"
  }
  if(file.exists(system.file("extdata",
                             "20201217.0749.InvertResults.csv",
                             package = "StreamData"))){
    unlink(system.file("extdata",
                       "20201217.0749.InvertResults.csv",
                       package = "StreamData"))
  }
  Project <- utils::read.csv(system.file("extdata",
                                    "20201217.0749.Project.csv",
                                    package = "StreamData"),
                        comment.char="#",
                        stringsAsFactors = FALSE)
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

  SamplingRatio_SamplerType <- StreamData:::.SamplingRatio_SamplerType

  Inverts <- dplyr::left_join(Inverts,
                       SamplingRatio_SamplerType,
                       by = "LabRecordID")

  ### Have to sum invertebrate abundance of duplicate species entries for
  ### all samplers
  ## Folsom samplers split the sample repeatedly, and count ***ALL*** individuals
  ## towards the abundance measures
  ## So, this first step in determining 'Abundance' is to sum (e.g., collapse) all
  ## entries for a given species at each collection site for a single
  ## 'Abundance' measure

  ## "IRTH" == "Invertebrate Richest Taxa Habitat" theoretically supports the faunistically
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
    dplyr::filter(SampleTypeCode %in% c("IRTH", "BERW")) %>%
    dplyr::filter(FieldComponent == "M") %>%
    dplyr::mutate(CollectionDate = as.Date(CollectionDate,
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
    dplyr::group_by(Identifier) %>%
    dplyr::filter(dplyr::n_distinct(LabSubsamplingRatio) >= 2) %>%
    dplyr::ungroup()

  Invert_SingleRatios <- suppressMessages({dplyr::anti_join(Inverts, Invert_MixedRatios) %>%
      dplyr::mutate(DatasetPortion = "SingleRatios")})

  SingleRatio_Duplicates <-
    Invert_SingleRatios[duplicated(Invert_SingleRatios$SampleGrouping) |
                          duplicated(Invert_SingleRatios$SampleGrouping,
                                     fromLast = TRUE), ] %>%
    dplyr::group_by(SampleGrouping) %>%
    dplyr::mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    dplyr::ungroup()


  SumSingleRatioData = suppressWarnings({SingleRatio_Duplicates %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::mutate(SummedAbundance = sum(Abundance),
                    SummedRawCount = sum(RawCount)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Abundance = SummedAbundance,
                    Density_m2 = as.numeric("NA"),
                    AdjRawCount = as.numeric("NA"),
                    RawCount =SummedRawCount,
                    TotAreaSampled_m2 = as.numeric("NA"),
                    FieldSplitRatio = as.numeric("NA"),
                    Note = "Abundance measure is summed 'Abundance' across multiple inputs
         (see 'LabRecordsIDs'); Likely due to changed identification
         (e.g., BenchNotes)",
                    DatasetPortion = "SummedAbundance_SingleRatios") %>%
      dplyr::select(-SummedAbundance, -SummedRawCount)})

  Corrected_SingleRatios <- suppressMessages({dplyr::bind_rows((dplyr::anti_join(Invert_SingleRatios,
                                                 SingleRatio_Duplicates) %>%
                                                   dplyr::mutate(DatasetPortion =
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

  Invert_MixedRatios_GridsAndFS <- Invert_MixedRatios %>%
    dplyr::group_by(Identifier) %>%
    dplyr::filter(dplyr::n_distinct(SamplerType) == 2) %>%
    dplyr::ungroup()

  Invert_MixedSamplerType_GridOnly_LLRRemoved <- Invert_MixedRatios_GridsAndFS %>%
    dplyr::group_by(Identifier) %>%
    dplyr::filter(SamplerType == "Grid") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DatasetPortion = "Grid_LRRRemoved")

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
    dplyr::group_by(SampleGrouping) %>%
    dplyr::mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DatasetPortion = "Gridded_LLRRemoved_Duplicates")

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
      dplyr::group_by(SampleGrouping) %>%
      dplyr::mutate(SummedAbundance = sum(Abundance),
                    SummedRawCount = sum(RawCount)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Abundance = SummedAbundance,
                    Density_m2 = as.numeric("NA"),
                    AdjRawCount = as.numeric("NA"),
                    RawCount = SummedRawCount,
                    TotAreaSampled_m2 = as.numeric("NA"),
                    FieldSplitRatio = as.numeric("NA"),
                    Note = "Abundance measure is summed 'Abundance' across multiple inputs
         (see 'LabRecordsIDs'); Likely due to changed identification
         (e.g., BenchNotes)",
                    DatasetPortion = "SummedAbundance_Gridded_LLRRemoved") %>%
      dplyr::select(-SummedAbundance, -SummedRawCount)})

  ## Now that we have the corrected summed values for the
  ## 'Gridded_LLRRemoved_Duplicates', we need to add these rows to the
  ## non-duplicated rows within the 'SingleRatio' portions
  Corrected_Gridded_LLRRemoved <- suppressMessages({dplyr::bind_rows(
    (dplyr::anti_join(Invert_MixedSamplerType_GridOnly_LLRRemoved,
               Gridded_LLRRemoved_Duplicates) %>%
       dplyr::mutate(DatasetPortion =
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
      dplyr::group_by(SIDNO, SiteNumber, CollectionDate) %>%
      dplyr::filter(dplyr::n_distinct(LabSubsamplingRatio) >= 2) %>%
      dplyr::anti_join(Invert_MixedRatios_GridsAndFS)})

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
    dplyr::group_by(SampleGrouping) %>%
    dplyr::mutate(LabRecordIDs = paste(LabRecordID, collapse = "_"),
           Ratios = paste(Ratio, collapse = "_")) %>%
    dplyr::ungroup()

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
      dplyr::group_by(SampleGrouping) %>%
      dplyr::mutate(SummedAbundance = sum(Abundance),
                    SummedRawCount = sum(RawCount)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SampleGrouping) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Abundance = SummedAbundance,
                    Density_m2 = as.numeric("NA"),
                    AdjRawCount = as.numeric("NA"),
                    RawCount = SummedRawCount,
                    TotAreaSampled_m2 = as.numeric("NA"),
                    FieldSplitRatio = as.numeric("NA"),
                    Note = "Abundance measure is summed 'Abundance' across multiple
         folsom sampler inputs (see 'LabRecordsIDs')",
                    DatasetPortion = "SummedAbundance_FolsomSampler") %>%
      dplyr::select(-SummedAbundance, -SummedRawCount)})


  Corrected_MixedRatios_FolsomSamplerOnly <- suppressMessages({dplyr::bind_rows((
    dplyr::anti_join(Invert_MixedRatios_FolsomSamplerOnly_AntiJoin,
              Invert_MixedRatios_Folsom_AntiJoin_Duplicates,
              by = "SampleGrouping") %>%
      dplyr::mutate(DatasetPortion = "NonDuplicate_FolsomSampler")),
    SumData)})

  TotalRows <- do.call(dplyr::bind_rows,list(Corrected_MixedRatios_FolsomSamplerOnly,
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
                        colClasses = c("SiteNumber" = "character"),
                        stringsAsFactors = FALSE) %>%
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
                           colClasses = c("SiteNumber" = "character"),
                           stringsAsFactors = FALSE) %>%
    dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
    dplyr::select(SIDNO,
                  ReplicateType)


  invertsite = utils::read.csv(system.file("extdata",
                                    "20201217.0749.SiteInfo.csv",
                                    package = "StreamData"),
                        colClasses = c("SiteNumber" = "character"),
                        stringsAsFactors = FALSE) %>%
    dplyr::select(SiteNumber,
                  Latitude_dd,
                  Longitude_dd,
                  CoordinateDatum,
                  HUCCode,
                  DrainageArea_mi2,
                  SiteTypeName,
                  CountyFIPSCode,
                  StateFIPSCode)

  invertsampinfo = dplyr::left_join(dplyr::left_join(invertsamp,
                                       invertsampinv,
                                       by = "SIDNO"),
                             invertsite,
                             by = "SiteNumber") %>%
    dplyr::select(-SiteNumber) %>%
    dplyr::mutate(CountyFIPSCode = sprintf("%03d", CountyFIPSCode),
           StateFIPSCode = sprintf("%02d", StateFIPSCode))

  TotalRows = dplyr::left_join(TotalRows,
                        invertsampinfo,
                        by = "SIDNO")

  if(abunMeasure == "abundance"){
    abunMeasure = "Abundance"
    notAbun <- "Density_m2"
  } else {
    notAbun <- "Abundance"
    abunMeasure <- "Density_m2"}

  mycols = StreamData:::.TaxLevCols_Inverts[[which(names(StreamData:::.TaxLevCols_Inverts) == taxonLevel)]]$mycols
  taxcols = StreamData:::.TaxLevCols_Inverts[[which(names(StreamData:::.TaxLevCols_Inverts) == taxonLevel)]]$taxcols

  ##MIKE NOTE BELOW
  ###NOTE: NEED TO MOVE THIS AFTER THE RANDOM SAMPLING, BECAUSE IT IS CAUSING SAMPLES TO BE DROPPED
  ##THIS COULD AFFECT SOME THINGS, BUT MIGHT BE REALLY EASY


  if(isTRUE(rarefy)) {
    TotalRows = TotalRows %>%
      dplyr::group_by(SIDNO) %>%
      dplyr::mutate(indcounted = sum(RawCount)) %>%
      dplyr::filter(indcounted > 299) %>%
      dplyr::select(-indcounted) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(SIDNO) %>%
      dplyr::slice(rep(1:dplyr::n(), times=RawCount)) %>%
      dplyr::sample_n(size = 300) %>%
      dplyr::group_by(SIDNO, PublishedTaxonName) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {}



  ##SLR - ADD OPTIONS TO 1) GROUP PROBLEMATIC IDENTIFICATIONS OR 2) THROW OUT PROBLEMATIC OBSERVATION WITH MISSING SPP DATA

  #create variable taxonFix = none, lump, remove
  #none = no change, lump = lump genera through time, remove = remove observation only if spp. level ID does not exist

  if(taxonFix == "none"){

  }else if(taxonFix == "lump"){

    #create bench genus in TotalRows
    TotalRows <- TotalRows %>%
      dplyr::mutate(BenchGenus = as.character(gsub( " .*$", "", BenchTaxonName)))

    #If bench genera that are one of bench genera in clust_labels, rename the Genus with the lump label from clust_labels
    #else, keep the original Genus label
    TotalRows$Genus <- ifelse(TotalRows$BenchGenus %in% StreamData:::.clust_labels$genus,
                              StreamData:::.clust_labels$lump[match(TotalRows$BenchGenus,StreamData:::.clust_labels$genus)],
                              TotalRows$Genus)
    TotalRows <- TotalRows %>%
      dplyr::select(-BenchGenus)

  }else if(taxonFix == "remove"){

    #filter out rows that have bench genus from problem list & no species ID
    TotalRows <- TotalRows %>%
      dplyr::filter(!(Genus %in% StreamData:::.clust_labels$genus & PublishedTaxonNameLevel == "Genus"))

  }

  if(isTRUE(lifestage)) {
    #Lifestage-taxon combinations
    invert_comms1 = TotalRows %>%
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
      dplyr::filter_at(vars(all_of(taxonLevel)), any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(SIDNO, all_of(taxonLevel), Lifestage), sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(Abundance = sum(Abundance)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Density_m2 = ifelse(is.na(AreaSampTot_m2),
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
                              "SamplerType", "DatasetPortion", "TotAreaSampled_m2"))) %>%
      dplyr::select(-any_of(mycols),
                    -any_of(notAbun)) %>%
      tidyr::unite(Taxon_Life, c(all_of(taxonLevel), Lifestage), sep = "_") %>%
      tidyr::pivot_wider(names_from = all_of(Taxon_Life),
                         names_prefix = "tax_",
                         values_from = all_of(abunMeasure),
                         values_fill = 0)

  } else {
    #All species are one
    invert_comms1 = TotalRows %>%
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
                                          "SamplerType", "DatasetPortion", "TotAreaSampled_m2"))) %>%
      dplyr::select(-tidyselect::any_of(mycols)) %>%
      dplyr::select(-tidyselect::any_of(notAbun)) %>%
      tidyr::pivot_wider(names_from = all_of(taxonLevel),
                         names_prefix = "tax_",
                         values_from = all_of(abunMeasure),
                         values_fill = 0)
  }

  if(dataType == "occur") {
    invert_comms1 = invert_comms1 %>%
      dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                    ~replace(., . > 0, 1)))
  }

  colnames(invert_comms1) = sub("tax_", "", colnames(invert_comms1))


  invert_comms1 = invert_comms1 %>%
    dplyr::select(-SiteVisitSampleNumber,
                  -Identifier,
                  -SIDNO,
                  -ReleaseCategory) %>%
    dplyr::relocate(tidyselect::any_of(StreamData:::.ReorderUSGSBioDataColNames))

  return(data.frame(invert_comms1))

}
