#' Access clean USGS Macro-Invertebrate Dataset
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"Phylum"}, \code{"Class"}, \code{"Order"},
#'   \code{"Family"}, \code{"Genus"}, or \code{"Species"}.
#' @param taxonFix How to deal with changes in taxonomy across time, must
#'   be one of: \code{"none"}, \code{"lump"}, \code{"remove"}.
#' @param agency The agency name(s) (e.g., "USGS" and "EPA") that should be
#'   included in the output dataset. As of now, "USGS" must be present in the
#'   agency vector. See \code{Details} below for more information.
#' @param lifestage logical. Should the output dataset should include lifestage information for
#'   each individual? \code{TRUE} or \code{FALSE}.
#' @param rarefy logical. Should samples be standardized by the number of individuals
#'   identified within each sample? \code{TRUE} or \code{FALSE}. See \code{Details}
#'   below for more information.
#' @param seed numeric. Set seed for \code{rarefy} to get consistent results with every
#'   iteration of the function.
#' @param sharedTaxa logical. Should Genera be limited to those that appear in
#'   both the NRSA and USGS datasets? \code{TRUE} or \code{FALSE}.
#'
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
#'   \code{taxonFix = "remove"} prioritizes accurate identification by dropping
#'   observations from problematic genera that do not have species identification.
#'   Without a species-level identification from the bench, there is no way to
#'   assure correct membership in a updated genera. NOTE: "slash" genera that NRSA
#'   and NAWQA roll up into family/subfamily are now being included as genera. As
#'   a result, there are fewer individual genera, but this results in more data
#'   being included at the genus level. When \code{taxonFix = "lump"}, these "slash"
#'   genera are rolled into the larger linked genera, as above. \code{taxonFix = "remove"}
#'   still prioritizes accurate identifications by dropping all obscure identifications,
#'   such that ALL slash genera are dropped; this will result in much less data
#'   being included in the final dataset. Finally, \code{taxonFix = "none"}, still
#'   generate "slash" genera, but, it does not link these genera to larger linked
#'   genera.
#'
#'   \code{agency} refers to agency that collected the invertebrate samples. If
#'   you want to use data from both agencies, set \code{agency = c("USGS", "EPA")}.
#'   For the "USGS" dataset, this includes all programs with SampleMethodCodes
#'   of "BERW", "IRTH", "SWAMP", "EMAP", "CDPHE", and "PNAMP". If \code{agency}
#'   includes "EPA", samples from the EPA National Stream and River
#'   Assessment programs (2018-2019, 2013-2014, 2008-2009) and Wadeable Stream Assessment
#'   (2000-2004) will be included. Note that from these samples, only moving
#'   waters classified as "wadeable" are included and only samples that are
#'   "reach-wide" are included. Some information included in the USGS dataset
#'   are not included in the EPA datasets, and thus will appear as "NA".
#'   Similar to the USGS data, there were inherent taxonomic issues with the
#'   EPA data. As such, we have taken the same steps as described above under
#'   \code{taxonFix} to address these concerns. NOTE: As of now, \code{agency}
#'   must include "USGS". This will be fixed in future versions.
#'
#'   If \code{rarefy = TRUE}, only samples with 300+ individuals identified (RawCount)
#'   will be retained. Thus, ~17 \% of samples will be removed, as they have <300
#'   individuals sampled. We set the rarefaction threshold at 300, because 1) with
#'   every 50 individuals identified, ~1 genera are added to the sample and 2) 82.8 \%
#'   of samples have at least 300 individuals identified. Thus, lowering the threshold
#'   to 200 individuals removed ~2 genera per sample, but only added an additional
#'   7.3 \% of samples included (90.1 \% from 82.8 \%). Similarly, increasing the
#'   threshold to 400 individuals added ~2 genera per sample, but reduced samples to
#'   30.3 \% of all samples. Use \code{seed = ...} to get consistent output of
#'   community data. NOTE: \code{rarefy = TRUE} can be used when wanting
#'   occurrence data (presence/absence) OR proportional data (each taxon represents
#'   a certain percent of a sample). Use \code{rarefy = FALSE} when densities are
#'   the measure that you are interested in using.
#'
#'
#' @examples
#' \dontrun{
#' Inverts <- getInvertData(taxonLevel = "Genus")
#'
#' RarefyInverts <- getInvertData(taxonLevel = "Genus",
#'                                rarefy = TRUE,
#'                                seed = 10)
#' }
#'
#' @export

getInvertData <- function(dataType = "occur",
                          taxonLevel = "Genus",
                          taxonFix = "lump",
                          agency = c("USGS", "EPA"),
                          lifestage = FALSE,
                          rarefy = TRUE,
                          sharedTaxa = FALSE,
                          seed = 0){

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

  if(lifestage != TRUE && lifestage != FALSE){
    stop('lifestage must be set to either TRUE or FALSE.')
  }

  if(rarefy != TRUE && rarefy != FALSE){
    stop('rarefy must be set to either TRUE or FALSE.')
  }

  if(rarefy == TRUE && dataType == "abun"){
    stop('rarefy must be set to FALSE when requesting abundance data')
  }

  if(rarefy == FALSE && dataType == "occur"){
    warning('rarefy should be set to TRUE when requesting occurrence data')
  }

  if(!any(grepl("USGS", agency))){
    stop('agency must contain "USGS" at this time')
  }

  Inverts <- utils::read.csv(base::unz(base::system.file("extdata",
                                                         "20201217.0749.InvertResults.zip",
                                                         package = "StreamData"),
                                       "20201217.0749.InvertResults.csv"),
                             colClasses = c("SiteNumber" = "character"),
                             stringsAsFactors = FALSE)
  if(colnames(Inverts)[1] != "SIDNO"){
    colnames(Inverts)[1] = "SIDNO"
  }

  Project <- utils::read.csv(base::system.file("extdata",
                                               "20201217.0749.Project.csv",
                                               package = "StreamData"),
                             comment.char="#",
                             stringsAsFactors = FALSE)

  if(any(grepl("USGS", agency))) {
    database <- c("National Water Quality Assessment",
                  "Cooperative Water Program",
                  "Collection of Basic Records",
                  "Other Federal Agencies")
  }

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

  ## Need to get the SamplingMethodReference data from invertsamp
  invertSampleRef = utils::read.csv(system.file("extdata",
                                                "20201217.0749.InvertSamp.csv",
                                                package = "StreamData"),
                                    colClasses = c("SiteNumber" = "character"),
                                    stringsAsFactors = FALSE) %>%
    dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
    dplyr::select(SIDNO,
                  SamplingMethodReference)

  ##Pull the information from invertsample to Inverts
  Inverts$SamplingMethodReference <- invertSampleRef$SamplingMethodReference[match(Inverts$SIDNO,
                                                                                   invertSampleRef$SIDNO)]

  Inverts$SampleTypeCode[grep("CSQA", Inverts$ProjectLabel)] <- "SWAMP"
  Inverts$SampleTypeCode[grep("EMAP 1990", Inverts$SamplingMethodReference)] <- "EMAP"
  Inverts$SampleTypeCode[grep("CDPHE Riffle",
                              Inverts$SamplingMethodReference)] <- "CDPHE RR"
  Inverts$SampleTypeCode[grep("PNAMP",
                              Inverts$SamplingMethodReference)] <- "PNAMP"


  Inverts <- Inverts %>%
    dplyr::select(-SamplingMethodReference)

  Inverts <- Inverts %>%
    dplyr::filter(SampleTypeCode %in% c("IRTH", "BERW", "SWAMP",
                                        "EMAP", "CDPHE RR", "PNAMP")) %>%
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


  ##Generate the list of "slash" genera that appear in the NAWQA dataset
  ##Select those BenchTaxon that have a "/", then remove all that have a space (" ")
  ## in the string, as this indicates a species level identification, then remove
  ## those with "idae", as this indicates a family level identification

  slashgen <- unique(Inverts[grepl("/", Inverts$BenchTaxonName),]$BenchTaxonName)
  slashgen_no_spp <- slashgen[!(grepl(" ", slashgen))]
  slashgen_fin <- slashgen_no_spp[!(grepl("idae", slashgen_no_spp))]

  ##Rename the Genus column with the slashed genera, if the BenchTaxonName is one
  ## of the slash genera, otherwise leave it as the current "Genus" designation
  ##If BenchTaxonName is one of the slash genera, put "PublishedTaxonNameLevel"
  ## as genus, otherwise leave it as the current designation

  Inverts <- Inverts %>%
    dplyr::mutate(Genus = ifelse(BenchTaxonName %in% slashgen_fin,
                                 BenchTaxonName,
                                 Genus),
                  PublishedTaxonNameLevel = ifelse(BenchTaxonName %in% slashgen_fin,
                                                   "Genus",
                                                   PublishedTaxonNameLevel)
    )

  ##Remove 8 samples with >1 Field Split Ratio, because samples are listed as
  ##1 and 0.33; a sample cannot be split >1 time, so samples are being dropped
  ##All 8 samples are from CCYK BioTDB

  Inverts <- data.frame(Inverts %>%
                          dplyr::group_by(SIDNO) %>%
                          dplyr::mutate(nfsr = length(unique(FieldSplitRatio))) %>%
                          dplyr::filter(nfsr < 2) %>%
                          dplyr::ungroup() %>%
                          dplyr::select(-nfsr))

  ##Generate proportional mean labsubsampling ratios for samples with multiple
  ##labsaubsampling ratios; add these to those samples with single labsubsampling
  ##ratios; add fieldsplitratios to this;
  ##output the full dataset to be combined later in dataset

  ##Generate a Ratios dataset, to reduce code repitition
  Ratios <- Inverts %>%
    dplyr::select(SIDNO, LabSubsamplingRatio, SamplerType, RawCount) %>%
    dplyr::group_by(SIDNO) %>%
    # find the number of unique LSSR and SamplerTypes per SIDNO,
    # if number of LSSR is > 1 and the observation's LSSR is 1, this is a Lab Large Rare
    # These need to be dropped
    dplyr::mutate(nfsr = length(unique(LabSubsamplingRatio)),
                  samplerN = dplyr::n_distinct(SamplerType),
                  drop = ifelse(nfsr > 1 & LabSubsamplingRatio == 1,
                                "LLR",
                                "NoDrop"))

  ##Code to get LabSubsamplingRatio when there were multiple LSSR and sampler
  ## types per SIDNO, but multiple ratios after LLR were removed
  Multiratio <-  Ratios %>%
    dplyr::filter(nfsr > 1) %>%
    dplyr::filter(samplerN > 1) %>%
    dplyr::filter(drop != "LLR") %>%
    dplyr::mutate(nfsr = length(unique(LabSubsamplingRatio)),
                  samplerN = dplyr::n_distinct(SamplerType)) %>%
    dplyr::filter(nfsr > 1) %>%
    dplyr::group_by(SIDNO, LabSubsamplingRatio) %>%
    dplyr::summarize(sumcount = sum(RawCount),
                     .groups = 'drop') %>%
    dplyr::mutate(prop1 = sumcount * LabSubsamplingRatio) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SIDNO) %>%
    dplyr::summarize(count = sum(sumcount),
                     fullprop = sum(prop1),
                     .groups = 'drop') %>%
    dplyr::mutate(LabSubsamplingRatio = fullprop / count) %>%
    dplyr::select(SIDNO, LabSubsamplingRatio)

  ##Code to get LabSubsamplingRatio when there were single LSSR per SIDNO
  SingleRatios = Ratios %>%
    dplyr::filter(nfsr == 1) %>%
    dplyr::filter(drop != "LLR") %>%
    dplyr::mutate(nfsr = length(unique(LabSubsamplingRatio)),
                  samplerN = dplyr::n_distinct(SamplerType)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SIDNO) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(SIDNO, LabSubsamplingRatio)

  ##Code to get LabSubsamplingRatio when there were multiple LSSR and sampler
  ## types per SIDNO, but a single ratio after LLR were removed
  MultiratioSingleRatio <- Ratios %>%
    dplyr::filter(nfsr > 1) %>%
    dplyr::filter(samplerN > 1) %>%
    dplyr::filter(drop != "LLR") %>%
    dplyr::mutate(nfsr = length(unique(LabSubsamplingRatio)),
                  samplerN = dplyr::n_distinct(SamplerType)) %>%
    dplyr::filter(nfsr == 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SIDNO) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(SIDNO, LabSubsamplingRatio)

  ##Code to get LabSubsamplingRatio when there were multiple LSSR per SIDNO,
  ## and single sampler type, but single ratio after LLR were removed
  MultiratioSingleSamplerSingleRatio <- Ratios %>%
    dplyr::filter(nfsr > 1) %>%
    dplyr::filter(samplerN == 1) %>%
    dplyr::filter(drop != "LLR") %>%
    dplyr::mutate(nfsr = length(unique(LabSubsamplingRatio)),
                  samplerN = dplyr::n_distinct(SamplerType)) %>%
    dplyr::filter(nfsr == 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SIDNO) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(SIDNO, LabSubsamplingRatio)

  ##Code to get LabSubsamplingRatio when there were multiple ratios per SIDNO,
  ##but a multiple ratios with 1 sampler type after LLR were removed
  MultipleRatiosSingle <- Ratios %>%
    dplyr::filter(nfsr > 1) %>%
    dplyr::filter(samplerN == 1) %>%
    dplyr::filter(drop != "LLR") %>%
    dplyr::mutate(nfsr = length(unique(LabSubsamplingRatio)),
                  samplerN = dplyr::n_distinct(SamplerType)) %>%
    dplyr::filter(nfsr > 1) %>%
    dplyr::group_by(SIDNO, LabSubsamplingRatio) %>%
    dplyr::summarize(sumcount = sum(RawCount),
                     .groups = 'drop') %>%
    dplyr::mutate(prop1 = sumcount * LabSubsamplingRatio) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SIDNO) %>%
    dplyr::summarize(count = sum(sumcount),
                     fullprop = sum(prop1),
                     .groups = 'drop') %>%
    dplyr::mutate(LabSubsamplingRatio = fullprop / count) %>%
    dplyr::select(SIDNO, LabSubsamplingRatio)

  ##rbind all back together and join to the Inverts Field Split Ratio data
  FieldLabRatios <- dplyr::bind_rows(SingleRatios, Multiratio, MultiratioSingleRatio,
                                     MultiratioSingleSamplerSingleRatio,
                                     MultipleRatiosSingle) %>%
    left_join(Inverts %>%
                dplyr::select(SIDNO, FieldSplitRatio) %>%
                dplyr::group_by(SIDNO) %>%
                dplyr::slice(1) %>%
                dplyr::ungroup(),
              by = "SIDNO"
    )

  ## Generate the actual abundance data to be used for the site x species matrix
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
                    RawCount = SummedRawCount,
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
  ### Now we can add the 'LabRecord_Labels' and 'Ratio_Labels' to the duplicated data

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

  ##Replace the field and lab split ratios in TotalRows with the estimated ones
  ##in FieldLabRatios
  ##PropID (Proportion of field sample identified is the FieldSplitRatio * LSSR)

  TotalRows$FieldSplitRatio <- FieldLabRatios$FieldSplitRatio[match(TotalRows$SIDNO,
                             FieldLabRatios$SIDNO)]
  TotalRows$LabSubsamplingRatio <- FieldLabRatios$LabSubsamplingRatio[match(TotalRows$SIDNO,
                                       FieldLabRatios$SIDNO)]
  TotalRows$PropID = TotalRows$FieldSplitRatio * TotalRows$LabSubsamplingRatio

  ###At this point, we need to join information on stream, site, and sample (?)
  ## information
  ### Need to get sampled area from the Sample dataset
  ###Then, we need to pivot_wider, filter based on taxon level, remove excess
  ## columns, and change the data type

  ##Gather sample information
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

  ##Gather Sample inventory information (specifically, just replicate type)
  invertsampinv = utils::read.csv(system.file("extdata",
                                              "20201217.0749.SampleInv.csv",
                                              package = "StreamData"),
                                  colClasses = c("SiteNumber" = "character"),
                                  stringsAsFactors = FALSE) %>%
    dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
    dplyr::select(SIDNO,
                  ReplicateType)

  ##Gather site level information
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

  ##Join all of the site, sample, and inventory information together
  invertsampinfo = dplyr::left_join(dplyr::left_join(invertsamp,
                                                     invertsampinv,
                                                     by = "SIDNO"),
                                    invertsite,
                                    by = "SiteNumber") %>%
    dplyr::select(-SiteNumber) %>%
    dplyr::mutate(CountyFIPSCode = sprintf("%03d", CountyFIPSCode),
                  StateFIPSCode = sprintf("%02d", StateFIPSCode))

  ##Join the abundance data with the sample/site/inventory information
  TotalRows = dplyr::left_join(TotalRows,
                               invertsampinfo,
                               by = "SIDNO")

  ##These are the column names that should be removed (mycols) and which rows
  ##of data should be retained based on taxonomic resolution
  ##(eg if taxonLevel == "Family", retain ALL taxonomic levels at Family and Below)
  mycols = StreamData:::.TaxLevCols_Inverts[[which(names(StreamData:::.TaxLevCols_Inverts) == taxonLevel)]]$mycols
  taxcols = StreamData:::.TaxLevCols_Inverts[[which(names(StreamData:::.TaxLevCols_Inverts) == taxonLevel)]]$taxcols

  ##Before any final data manipulation, if dataset is occurence and rarify is true
  ##then rarify based on the RAWCOUNT (individuals actually identified)
  if(dataType == "occur"){
    if(isTRUE(rarefy)) {
      set.seed(seed)
      TotalRows = TotalRows %>%
        dplyr::group_by(SIDNO) %>%
        dplyr::mutate(indcounted = sum(RawCount)) %>%
        dplyr::filter(indcounted > 299) %>%
        dplyr::select(-indcounted) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(SIDNO, PublishedTaxonName) %>%
        dplyr::slice(rep(1:dplyr::n(), times=RawCount)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(SIDNO) %>%
        dplyr::sample_n(size = 300) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(SIDNO, PublishedTaxonName) %>%
        dplyr::mutate(RawCount = n()) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    } else {}
  }   else {}

  if(taxonLevel == "Genus"){

    #create variable taxonFix = none, lump, remove
    #none = no change, lump = lump genera through time, remove = remove observation only if spp. level ID does not exist
    ##Generate a list of those individual genera that make up the slash genera
    slashedgen <- unique(c(sub("\\/.*", "", slashgen_fin),
                           sub(".*\\/", "", slashgen_fin),
                           "Neocloeon"))

    cnt = c()
    hldr = c()
    gns = c()
    slashedgen <- slashedgen[order(slashedgen)]
    for(i in slashedgen){
      hldr <- grep(i, slashgen_fin, fixed = T)
      cnt <- c(cnt, hldr)
      gns = c(gns, rep(i, times = length(hldr)))
    }

    dat1 = data.frame(Genus = gns,
                      Slash = slashgen_fin[cnt])

    ##Fix a naming issue. Needs to include "Glyptotendipes"
    dat1$Slash[grep("Chironomus/Einfeldia", dat1$Slash)] <- "Chironomus/Einfeldia/Glyptotendipes"

    ##From the genus to slash dataset from above, remove all of those that do not
    ## appear in the clust_labels dataset
    ##First, match the group information based on the genera present in both the dat1
    ## and clust_labels dataset
    dat1$group <- StreamData:::.clust_labels[match(dat1$Genus,
                                                   StreamData:::.clust_labels$genus),]$group

    #There are some problems here, since there are multiple "slash" genera per
    #individual genus, so need to lump these
    #Select those genera that appear in >1 "slash" genera
    probslash <- dat1 %>%
      group_by(Genus) %>%
      mutate(count = n()) %>%
      filter(count >1) %>%
      dplyr::select(-count)

    ##Split this dataset
    probslashl <- split(probslash, probslash$Genus)

    ##Take the unique genera in the "slash" genera and join them into a larger
    ##lump "slash" genus
    for(i in 1:length(probslashl)){
      probslashl[[i]]$Fix <- paste(sort(unique(c(sub("\\/.*",
                                                     "",
                                                     probslashl[[i]]$Slash),
                                                 sub(".*\\/",
                                                     "",
                                                     probslashl[[i]]$Slash)))),
                                   collapse = "/")
    }

    ##Bind these together
    fix_slash <- dplyr::bind_rows(probslashl)

    ##Fix a naming issue; going to make sure this is fixed.

    ##Remove all observations with an NA for the group in dat1 (does not appear in
    ## clust_labels); then take 1 observation for each slash genus and generate
    ## information (this does not matter) to better join this dataset with the
    ## clust_labels dataset
    dat1L <- dat1 %>%
      dplyr::filter(!is.na(group)) %>%
      dplyr::group_by(Slash) %>%
      dplyr::slice(1) %>%
      dplyr::mutate(X = 10.65,
                    num = 65,
                    genus = Slash) %>%
      dplyr::ungroup() %>%
      dplyr::select(X, num, group, genus)

    dat1L$genus <- ifelse(dat1L$genus %in% fix_slash$Slash,
                          fix_slash$Fix[match(dat1L$genus,
                                              fix_slash$Slash)],
                          dat1L$genus)

    ##Remove replicate genus from the dat1L list
    dat1L <- dat1L %>%
      dplyr::group_by(genus) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    ##Add "Anafroptilum.Centroptilum.Procloeon" to the fix list
    dat1L[(nrow(dat1L) + 1),] <- list(10.6, 65, 1, "Anafroptilum.Centroptilum.Procloeon")

    ##Third, pull the "lump" information from clust_labels based on the "group"
    ## from dat1L
    dat1L$lump <- StreamData:::.clust_labels[match(dat1L$group,
                                                   StreamData:::.clust_labels$group),]$lump

    ##Finally, join these datasets, so that all slashes will be successfully pulled
    ## into the appropriate "lump" group
    slashlump <- dplyr::bind_rows(list(StreamData:::.clust_labels,
                                       dat1L))

    ##Fix a naming issue with "Einfeldia" groups
    slashlump$lump[grep("Einfeldia/Glyptotendipes", slashlump$lump)] <- "Chironomus/Einfeldia/Glyptotendipes"

    if(taxonFix == "none"){

    } else if(taxonFix == "lump"){

      #If genera that are one of genera in dat1, rename the Genus with the slash
      #label from dat1, else, keep the original Genus label
      TotalRows$Genus <- ifelse(TotalRows$Genus %in% dat1$Genus,
                                dat1$Slash[match(TotalRows$Genus,
                                                 dat1$Genus)],
                                TotalRows$Genus)

      #If genera that are one of problem slash genera, rename the Genus with the lumped
      #label from fix_slash, else, keep the original Genus label
      TotalRows$Genus <- ifelse(TotalRows$Genus %in% fix_slash$Slash,
                                fix_slash$Fix[match(TotalRows$Genus,
                                                    fix_slash$Slash)],
                                TotalRows$Genus)

      ##Do the same for those in slashlump
      TotalRows$Genus <- ifelse(TotalRows$Genus %in% slashlump$genus,
                                slashlump$lump[match(TotalRows$Genus,
                                                     slashlump$genus)],
                                TotalRows$Genus)

      #create bench genus in TotalRows
      TotalRows <- TotalRows %>%
        dplyr::mutate(BenchGenus = as.character(gsub( " .*$", "", BenchTaxonName)))


      #If bench genera that are one of bench genera in clust_labels, rename the Genus with the lump label from clust_labels
      #else, keep the original Genus label
      TotalRows$Genus <- ifelse(TotalRows$BenchGenus %in% slashlump$genus,
                                slashlump$lump[match(TotalRows$BenchGenus,
                                                     slashlump$genus)],
                                TotalRows$Genus)

      TotalRows <- TotalRows %>%
        dplyr::select(-BenchGenus)

    } else if(taxonFix == "remove"){

      #filter out observations that have ambiguous taxa designations
      ##remove those genera in the slashlump and remove all "/" genera
      TotalRows <- TotalRows %>%
        dplyr::filter(!(Genus %in% slashlump$genus &
                          PublishedTaxonNameLevel == "Genus")) %>%
        dplyr::filter(!(grepl("/", Genus)))

    }

  }

  ##Fix this; remove the notAbun stuff in the future; just drop Density_m2
  if(dataType == "occur") {
    abunMeasure = "RawCount"
    notMeasure = "Density"
  } else {
    abunMeasure = "Density"
    notMeasure = "RawCount"
  }


  if(isTRUE(lifestage)) {
    #Lifestage-taxon combinations
    invert_comms1 = TotalRows %>%
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel)), any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(SIDNO, tidyselect::all_of(taxonLevel), Lifestage), sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(Abundance = sum(Abundance),
                    RawCount = sum(RawCount)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Density = ifelse(is.na(AreaSampTot_m2),
                                        NA,
                                        Abundance / AreaSampTot_m2)) %>%
      ##Remvoe these rows of data (not needed)
      dplyr::select(-any_of(c("LabOrderID", "LabRecordID", "FieldComponent",
                              "LabComponent", "LabProcName",
                              "Density_m2",
                              "TaxonomicResultReviewStatus",
                              "PublishedSortOrder", "BioDataTaxonName", "BioDataShortName",
                              "BenchTaxonName", "BenchTaxonNameReferenceCode",
                              "AdjRawCount", "Abundance",
                              "UniqueTaxonFlag",
                              "TargetLevelNotReachedReason", "Artifact", "BenchNotes",
                              "TaxonRecordSource", "IdentificationDate",
                              "VerificationEntity", "VerificationDate", "CurationEntity",
                              "CurationDate", 'ITIS_TSN', "ITIS_MatchCode", "PublishedTaxonName",
                              "PublishedTaxonNameAuthority", "ScientificName",
                              "TaxonVersionNumber", "NWQLSubsamplingCode",
                              "Ratio", 'X', "NumbEntries", "SampleGrouping", "LabRecordIDs",
                              "Ratios", "Note", "UNIQUEID", "PublishedTaxonNameLevel",
                              "SamplerType", "DatasetPortion", "TotAreaSampled_m2"))) %>%
      dplyr::select(-tidyselect::any_of(mycols))
      dplyr::select(-tidyselect::any_of(notMeasure)) %>%
      tidyr::unite(Taxon_Life, c(tidyselect::all_of(taxonLevel), Lifestage), sep = "_") %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(Taxon_Life),
                         names_prefix = "tax_",
                         values_from = all_of(abunMeasure),
                         values_fill = 0)

  } else {
    #All lifestages are one species
    invert_comms1 = TotalRows %>%
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel)), dplyr::any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(SIDNO, tidyselect::all_of(taxonLevel)), sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(Abundance = sum(Abundance),
                    RawCount = sum(RawCount)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Density = ifelse(is.na(AreaSampTot_m2),
                                     NA,
                                     Abundance / AreaSampTot_m2)) %>%
      ##Drop these values
      dplyr::select(-tidyselect::any_of(c("LabOrderID", "LabRecordID", "FieldComponent",
                                          "LabComponent", "LabProcName",
                                          "Density_m2",
                                          "TaxonomicResultReviewStatus",
                                          "PublishedSortOrder", "BioDataTaxonName", "BioDataShortName",
                                          "BenchTaxonName", "BenchTaxonNameReferenceCode",
                                          "AdjRawCount", "Abundance",
                                          "UniqueTaxonFlag",
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
      dplyr::select(-tidyselect::any_of(notMeasure)) %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(taxonLevel),
                         names_prefix = "tax_",
                         values_from = tidyselect::all_of(abunMeasure),
                         values_fill = 0)
  }

  ##If datatype is abundance, remove all instances of when areasampled total is
  ##not recorded OR is equal to 0
  if(dataType == "abun"){
    invert_comms1 = invert_comms1 %>%
      dplyr::filter(!is.na(AreaSampTot_m2)) %>%
      dplyr::filter(AreaSampTot_m2 != 0)
  }

  ##site x species matrix, select only those columns that we need
  invert_comms1 = invert_comms1 %>%
    dplyr::select(-Identifier,
                  -SIDNO,
                  -ReleaseCategory) %>%
    dplyr::relocate(tidyselect::any_of(c(StreamData:::.ReorderUSGSBioDataColNames[-26],
                                       "FieldSplitRatio", "LabSubsamplingRatio",
                                       "PropID", "AreaSampTot_m2"))) %>%
    dplyr::mutate(SiteNumber = paste("USGS-", SiteNumber, sep = ""))

  if(any(grepl("EPA", agency))){

    ##Read in datasets directly from EPA website - may want a more stable source
    ##in the future (github repo?)
    NRSA_1819_inverts = read.csv("https://www.epa.gov/sites/production/files/2021-04/nrsa_1819_benthic_macroinvertebrate_count_-_data.csv",
                                 colClasses = c("UID" = "character"),
                                 stringsAsFactors = FALSE)
    NRSA_1819_sites = read.csv("https://www.epa.gov/system/files/other-files/2022-01/nrsa-1819-site-information-data-updated.csv",
                               colClasses = c("UID" = "character"),
                               stringsAsFactors = FALSE)

    NRSA_1314_inverts = read.csv("https://www.epa.gov/sites/production/files/2019-04/nrsa1314_bentcnts_04232019.csv",
                                 colClasses = c("UID" = "character"),
                                 stringsAsFactors = FALSE)
    NRSA_1314_sites = read.csv("https://www.epa.gov/sites/production/files/2019-04/nrsa1314_siteinformation_wide_04292019.csv",
                               colClasses = c("UID" = "character",
                                              "STATECTY" = "character"),
                               stringsAsFactors = FALSE)

    NRSA_0809_inverts = read.csv("https://www.epa.gov/sites/production/files/2016-11/nrsa0809bentcts.csv",
                                 colClasses = c("UID" = "character"),
                                 stringsAsFactors = FALSE)
    NRSA_0809_inverts_tax = read.csv("https://www.epa.gov/sites/production/files/2016-06/nrsa_0809_benttaxa.csv",
                                     stringsAsFactors = FALSE)

    NRSA_0809_sites = read.csv("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                               colClasses = c("UID" = "character"),
                               stringsAsFactors = FALSE)

    NRSA_0304_inverts = rbind(read.csv("https://www.epa.gov/sites/production/files/2014-10/wsa_bencnt_genus_ts_final_part1.csv",
                                       stringsAsFactors = FALSE),
                              read.csv("https://www.epa.gov/sites/production/files/2014-10/wsa_bencnt_genus_ts_final_part2.csv",
                                       stringsAsFactors = FALSE))
    NRSA_0304_sites = read.csv("https://www.epa.gov/sites/production/files/2014-10/wsa_siteinfo_ts_final.csv",
                               stringsAsFactors = FALSE)

    #############
    ##First step:
    ##Filter SAMPLE_TYPE to "BERW", "BERWW", or "REACHWIDE" in NRSA_inverts
    sampletype = c("BERW", "BERWW", "REACHWIDE")

    ##2003/2004
    NRSA_0304_inverts = NRSA_0304_inverts %>%
      dplyr::filter(INDEX_SAMPTYPE %in% sampletype) %>%
      dplyr::select(-DISTINCT, -HABIT, -PTV, -FLAG_PTV, -FFG)

    ##Replace NAs with "", which is consistent with the other NRSA datasets
    NRSA_0304_inverts$GENUS = ifelse(is.na(NRSA_0304_inverts$GENUS),
                                     "",
                                     NRSA_0304_inverts$GENUS)
    NRSA_0304_inverts$FAMILY = ifelse(is.na(NRSA_0304_inverts$FAMILY),
                                      "",
                                      NRSA_0304_inverts$FAMILY)
    NRSA_0304_inverts$ORDER = ifelse(is.na(NRSA_0304_inverts$ORDER),
                                     "",
                                     NRSA_0304_inverts$ORDER)
    NRSA_0304_inverts$CLASS = ifelse(is.na(NRSA_0304_inverts$CLASS),
                                     "",
                                     NRSA_0304_inverts$CLASS)

    ##Use the names directly in Genus, Family, Order, Class, or Phylum
    ##Some names don't match or are at the subfamily level, which doesn't exist for all
    ##Datasets
    NRSA_0304_inverts$TARGET_TAXON <- ifelse(NRSA_0304_inverts$GENUS != "",
                                             NRSA_0304_inverts$GENUS,
                                             ifelse(NRSA_0304_inverts$FAMILY != "",
                                                    NRSA_0304_inverts$FAMILY,
                                                    ifelse(NRSA_0304_inverts$ORDER != "",
                                                           NRSA_0304_inverts$ORDER,
                                                           ifelse(NRSA_0304_inverts$CLASS != "",
                                                                  NRSA_0304_inverts$CLASS,
                                                                  NRSA_0304_inverts$PHYLUM
                                                           )
                                                    )
                                             )
    )

    ##Create UID based on WSA, Site_id, and Visit_no
    NRSA_0304_inverts$UID <- paste("200304",
                                   NRSA_0304_inverts$SITE_ID,
                                   NRSA_0304_inverts$VISIT_NO,
                                   sep = "_")

    ##Update column names to match those of 08/09 and 13/14
    colnames(NRSA_0304_inverts)[c(4,10)] = c("SAMPLE_TYPE", "TOTAL")

    ##Rearrange columns to match those of 08/09 and 13/14
    NRSA_0304_inverts <- NRSA_0304_inverts %>%
      dplyr::relocate(any_of(c("UID", "SITE_ID", "YEAR", "VISIT_NO", "SAMPLE_TYPE",
                               "TARGET_TAXON", "TOTAL", "PHYLUM", "CLASS", "ORDER",
                               "FAMILY", "GENUS")))

    ##2008/2009
    ##Filter to BERW; remove columns that are not needed
    NRSA_0809_inverts = NRSA_0809_inverts %>%
      dplyr::filter(SAMPLE_TYPE %in% sampletype) %>%
      dplyr::select(-IS_DISTINCT, -TOTAL300, -IS_DISTINCT300, -BENT_COM, -DATE_BENT,
                    -SAMPLE_CAT, -PUBLICATION_DATE) %>%
      dplyr::mutate(YEAR = paste("20", stringr::str_sub(DATE_COL, -2,-1), sep = ""),
                    YEAR = as.numeric(YEAR)) %>%
      dplyr::select(-DATE_COL)

    ##Join the count data to the taxa data to match those of 03/04
    NRSA_0809_inverts<- NRSA_0809_inverts %>%
      dplyr::left_join(NRSA_0809_inverts_tax %>%
                         dplyr::select(TAXA_ID, PHYLUM, CLASS, ORDER, FAMILY, GENUS),
                       by = "TAXA_ID")

    ##Update this weird TARGET_TAXON that is THIENEMANNIMYIA GENUS GR., but does not
    ##have a genus associated with it; so make genus = "THIENEMANNIMYIA"
    NRSA_0809_inverts$GENUS = ifelse(grepl("GENUS", NRSA_0809_inverts$TARGET_TAXON),
                                     "THIENEMANNIMYIA",
                                     NRSA_0809_inverts$GENUS)

    ##Rearrange columns to match those of 08/09 and 13/14
    NRSA_0809_inverts <- NRSA_0809_inverts %>%
      dplyr::relocate(YEAR, .before = SAMPLE_TYPE) %>%
      dplyr::select(-TAXA_ID)

    ##2013/2014
    ##Filter to BERW; remove columns that are not needed (taxonomic resolutions are
    ##not available in all datasets, so remove those that are not found across data)
    NRSA_1314_inverts = NRSA_1314_inverts %>%
      dplyr::filter(SAMPLE_TYPE %in% sampletype) %>%
      dplyr::select(-IS_DISTINCT, -TOTAL300, -IS_DISTINCT300, -TOTAL300_OE,
                    -PUBLICATION_DATE, -TRIBE, -SUBFAMILY, -TAXA_ID)

    ##Update this weird TARGET_TAXON that is THIENEMANNIMYIA GENUS GR., but does not
    ##have a genus associated with it; so make genus = "THIENEMANNIMYIA"
    NRSA_1314_inverts$GENUS = ifelse(grepl("GENUS", NRSA_1314_inverts$TARGET_TAXON),
                                     "THIENEMANNIMYIA",
                                     NRSA_1314_inverts$GENUS)

    ##2018/2019

    ##Filter to BERW; remove columns that are not needed (taxonomic resolutions are
    ##not available in all datasets, so remove those that are not found across data)
    NRSA_1819_inverts <- NRSA_1819_inverts %>%
      dplyr::filter(SAMPLE_TYPE %in% sampletype) %>%
      dplyr::select(-IS_DISTINCT, -TOTAL300, -IS_DISTINCT300, -EPA_REG,
                    -PUBLICATION_DATE, -TRIBE, -SUBFAMILY, -TAXA_ID,
                    -FFG, -HABIT, -PTV, -AG_ECO9, -NON_TARGET, -SITESAMP,
                    -STATE, -UNIQUE_ID) %>%
      dplyr::mutate(YEAR = str_sub(DATE_COL, -4,-1),
                    YEAR = as.numeric(YEAR)) %>%
      dplyr::select(-DATE_COL) %>%
      dplyr::relocate(colnames(NRSA_1314_inverts))


    ##Update this weird TARGET_TAXON that is THIENEMANNIMYIA GENUS GR., but does not
    ##have a genus associated with it; so make genus = "THIENEMANNIMYIA"
    NRSA_1819_inverts$GENUS = ifelse(grepl("GENUS", NRSA_1819_inverts$TARGET_TAXON),
                                     "THIENEMANNIMYIA",
                                     NRSA_1819_inverts$GENUS)
    ##Bind all
    NRSA_inverts <- dplyr::bind_rows(list(NRSA_0304_inverts, NRSA_0809_inverts,
                                          NRSA_1314_inverts, NRSA_1819_inverts))


    ##Catch the "/" genus and make sure it is put in the GENUS column
    NRSA_inverts <- NRSA_inverts %>%
      mutate(GENUS = ifelse(grepl("/", TARGET_TAXON),
                            TARGET_TAXON,
                            GENUS))

    ##Convert Taxa names from all caps to sentence case (e.g., GENUS to Genus)
    NRSA_inverts$GENUS <- stringr::str_to_sentence(NRSA_inverts$GENUS)


    ##Fix issue w/ str_to_sentence that is causing Orthocladius to be lowercase
    NRSA_inverts$GENUS <- ifelse(NRSA_inverts$GENUS == "Cricotopus/orthocladius",
                                 "Cricotopus/Orthocladius",
                                 NRSA_inverts$GENUS)

    ##Read in the density conversion dataset from the EPA
    NRSADenconv <- utils::read.csv(base::system.file("extdata",
                                                     "EPA_DensityConv.csv",
                                                     package = "StreamData"),
                                   colClasses = c("SITE_ID" = "character"),
                                   stringsAsFactors = FALSE) %>%
      dplyr::select(-ABUNDCNT, -TOTLDENS, -UNIQUE_ID)

    ##Pair down the EPA dataset to only those site-year-visit_no combinations
    ##that appear in the NRSA_inverts dataset
    NRSADenconv <- NRSADenconv[which((paste(NRSADenconv$SITE_ID,
                                            NRSADenconv$YEAR,
                                            NRSADenconv$VISIT_NO,
                                            sep = "_") %in%
                                        paste(NRSA_inverts$SITE_ID,
                                              NRSA_inverts$YEAR,
                                              NRSA_inverts$VISIT_NO,
                                              sep = "_"))),]


    NRSA_inverts <- NRSA_inverts %>%
      dplyr::left_join(NRSADenconv, by = c("SITE_ID", "YEAR", "VISIT_NO"))


    ##Incorporate abundance conversions here
    if(dataType == "abun"){

      ##Join the datasets together; convert TOTAL to density, using the
      ##DenAbunRatio; multiple this by 10.76 to convert from ind ft^-2 to ind m^-2
      ##Remove the DenAbunRatio from the final dataset; and output
      NRSA_inverts <- NRSA_inverts %>%
        dplyr::mutate(TOTAL = round(((TOTAL / PCTCOUNT) / NUMTRANS) * 10.76, 4))

    }

    ##Second step:
    ##Rarefy samples to 300 in the same manner as the NAQWA data for consistency
    if(isTRUE(rarefy)) {
      set.seed(seed)
      NRSA_inverts <- NRSA_inverts %>%
        ##Create unique grouping based on UID, SITE_ID, YEAR, and VISIT_NO
        ##Group by this column
        ##Take the total individuals counted, remove those that are less than 300
        tidyr::unite(Unique, c(UID, SITE_ID, YEAR, VISIT_NO), sep = "_", remove = F) %>%
        dplyr::group_by(Unique) %>%
        dplyr::mutate(indcounted = sum(TOTAL)) %>%
        dplyr::filter(indcounted > 299) %>%
        dplyr::select(-indcounted) %>%
        dplyr::ungroup() %>%
        ##Again group by the unique sample column
        ##Replicate each unique sample and target taxon by the number of individuals
        ## found in the sample, then take 300 random individuals from these samples
        dplyr::group_by(Unique, TARGET_TAXON) %>%
        dplyr::slice(rep(1:dplyr::n(), times=TOTAL)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Unique) %>%
        dplyr::sample_n(size = 300) %>%
        dplyr::group_by(Unique, TARGET_TAXON) %>%
        dplyr::mutate(TOTAL = dplyr::n()) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-Unique)
    } else {}
    ##Third step:
    ##FIX ALL TAXONOMIC ISSUES; only needed IF taxonLevel = "Genus"
    ##NEED TO UPDATE THIS FOR FAMILY

    if(taxonLevel == "Genus") {

      ##Convert those genera that need to be updated
      NRSA_inverts$GENUS <- ifelse(NRSA_inverts$GENUS %in% StreamData:::.switch1to1$BenchGenus,
                                   StreamData:::.switch1to1$Genus[match(NRSA_inverts$GENUS,
                                                                        StreamData:::.switch1to1$BenchGenus)],
                                   NRSA_inverts$GENUS)

      ##This is the same code as the NAWQA taxonomy fix
      if(taxonFix == "none"){

      } else if(taxonFix == "lump"){
        #If genera that are one of genera in dat1, rename the Genus with the slash
        #label from dat1, else, keep the original Genus label
        NRSA_inverts$GENUS <- ifelse(NRSA_inverts$GENUS %in% dat1$Genus,
                                     dat1$Slash[match(NRSA_inverts$GENUS,
                                                      dat1$Genus)],
                                     NRSA_inverts$GENUS)

        #If genera that are one of problem slash genera, rename the Genus with the lumped
        #label from fix_slash, else, keep the original Genus label
        NRSA_inverts$GENUS <- ifelse(NRSA_inverts$GENUS %in% fix_slash$Slash,
                                     fix_slash$Fix[match(NRSA_inverts$GENUS,
                                                         fix_slash$Slash)],
                                     NRSA_inverts$GENUS)

        #If bench genera that are one of bench genera in clust_labels, rename the Genus with the lump label from clust_labels
        #else, keep the original Genus label
        NRSA_inverts$GENUS <- ifelse(NRSA_inverts$GENUS %in% slashlump$genus,
                                     slashlump$lump[match(NRSA_inverts$GENUS,
                                                          slashlump$genus)],
                                     NRSA_inverts$GENUS)

      }else if(taxonFix == "remove"){

        #filter out rows that have bench genus from problem list & no species ID
        NRSA_inverts <- NRSA_inverts %>%
          dplyr::filter(!(GENUS %in% StreamData:::.clust_labels$genus)) %>%
          dplyr::filter(!(grepl("/", GENUS)))
      }

    }
    ##When "taxonLevel" isn't in all caps (in the function), create a NRSA specific
    ##taxonLevel that is in all caps
    taxonLevel.nrsa <- base::toupper(taxonLevel)

    if(taxonLevel != "Genus"){
    NRSA_inverts <- NRSA_inverts %>%
      dplyr::mutate(across(tidyselect::all_of(taxonLevel.nrsa), ~ stringr::str_to_sentence(.)))
    }

    if(isTRUE(sharedTaxa)){
      ##List of NAWQA taxa
      NAWQAtaxa <- c(unique(TotalRows[,taxonLevel]))[[taxonLevel]]

      ##List of NRSA taxa
      NRSAtaxa <- unique(NRSA_inverts[,taxonLevel.nrsa])

      ##Filter NRSA to only those genera in NAWQA
      NRSA_inverts <- NRSA_inverts %>%
        filter(.[[taxonLevel.nrsa]] %in% NAWQAtaxa)

      ##Select only those taxa that appear in NAWQA
      ##add "tax_" prefix to the names, as this is how the genera names appear
      ##as columns in the NAWQA dataset
      NAWQAtaxaONLY <- paste("tax_",
                               NAWQAtaxa[!(NAWQAtaxa %in% NRSAtaxa$GENUS)],
                               sep = "")

      ##Filter NAWQA to only those genera in NRSA (-select [delete] any that
      ##appear in columns in the invert_comms1 dataset)
      invert_comms1 <- invert_comms1 %>%
        dplyr::select(-tidyselect::any_of(NAWQAtaxaONLY))

    } else {}

    ##Fourth step: (can get code from the getInvertData function)
    ## NOTE: this step is only needed when looking at taxonomic resolutions ABOVE genus
    ##Join all Target_taxon within each UID (sample)

    ##UPDATE THIS FOR NRSA_MYCOLS
    mycols = c("TARGET_TAXON",
               "PHYLUM",
               "CLASS",
               "ORDER",
               "FAMILY",
               "GENUS")

    mycols <- mycols[!(mycols %in% tidyselect::all_of(taxonLevel.nrsa))]

    nrsa_comms1 = NRSA_inverts %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel.nrsa)), dplyr::any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(UID, SITE_ID, YEAR, VISIT_NO, all_of(taxonLevel.nrsa)),
                   sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(TOTAL = sum(TOTAL)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-UNIQUEID) %>%
      dplyr::select(-tidyselect::any_of(mycols)) %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(taxonLevel.nrsa),
                         names_prefix = "tax_",
                         values_from = TOTAL,
                         values_fill = 0)

    ##Step 6: join w/ site level data

    ##18/19 - MISSING RT_NRSA, will put in ""
    ##UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE, LAT_DD83, LON_DD83,
    ##AG_ECO9, URBN_NRS18, US_L3CODE, US_L3NAME
    NRSA_1819_sites <- NRSA_1819_sites %>%
      dplyr::select(UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE,
                    LAT_DD83, LON_DD83, AG_ECO9, URBN_NRS18,
                    US_L3CODE, US_L3NAME) %>%
      dplyr::mutate(RT_NRSA = "",
                    DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"),
                    VISIT_NO = as.character(VISIT_NO),
                    MASTER_SITEID = SITE_ID) %>%
      dplyr::relocate(RT_NRSA, .before = US_L3CODE) %>%
      dplyr::relocate(MASTER_SITEID, .after = SITE_ID)

    ##13/14
    ##UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE, LAT_DD83, LON_DD83,
    ##AG_ECO9, NRS13_Urban, RT_NRSA, US_L3CODE, US_L3NAME,
    NRSA_1314_sites <- NRSA_1314_sites %>%
      dplyr::select(UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE,
                    LAT_DD83, LON_DD83, AG_ECO9, NRS13_URBN, RT_NRSA,
                    US_L3CODE, US_L3NAME) %>%
      dplyr::mutate(RT_NRSA = ifelse(RT_NRSA == "?",
                                     "",
                                     RT_NRSA),
                    DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"),
                    VISIT_NO = as.character(VISIT_NO),
                    MASTER_SITEID = SITE_ID) %>%
      dplyr::relocate(MASTER_SITEID, .after = SITE_ID)

    ##08/09 - MISSING L3 NAME (not a problem)
    ##UID, SITE_ID, VISIT_NO, SITE_CLASS, DATE_COL, STATE, LAT_DD83, LONG_DD83,
    ##AGGR_ECO9_2015, URBAN, RT_NRSA, US_L3CODE_2015
    NRSA_0809_sites <- NRSA_0809_sites %>%
      dplyr::select(UID, SITE_ID, MASTER_SITEID, VISIT_NO, SITE_CLASS, DATE_COL, STATE,
                    LAT_DD83, LON_DD83, AGGR_ECO9_2015, URBAN, RT_NRSA,
                    US_L3CODE_2015) %>%
      dplyr::mutate(RT_NRSA = ifelse(RT_NRSA == "R",
                                     "R",
                                     ifelse(RT_NRSA == "S",
                                            "In",
                                            ifelse(RT_NRSA == "T",
                                                   "Im",
                                                   "")))) %>%
      dplyr::mutate(US_L3NAME = "",
                    DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"),
                    VISIT_NO = as.character(VISIT_NO))

    ##For some reason, one MASTER_SITEID is missing for one replicate sample, so give it the site id
    NRSA_0809_sites$MASTER_SITEID[which(NRSA_0809_sites$SITE_ID == "FW08LA004")] = "FW08LA004"

    ##03/04 - MISSING UID (will create in the same way as above), URBAN
    ##SITE_ID, VISIT_NO, SITETYPE, DATE_COL, STATE, LAT_DD, LON_DD,
    ##ECOWSA9, RT_WSA, ECO3, ECO3_NM
    NRSA_0304_sites <- NRSA_0304_sites %>%
      dplyr::select(SITE_ID, VISIT_NO, SITETYPE, DATE_COL, STATE, LAT_DD, LON_DD,
                    ECOWSA9, RT_WSA, ECO3, ECO3_NM) %>%
      dplyr::mutate(RT_WSA = ifelse(RT_WSA == "R",
                                    "R",
                                    ifelse(RT_WSA == "S",
                                           "In",
                                           ifelse(RT_WSA == "T",
                                                  "Im",
                                                  "")))) %>%
      dplyr::mutate(URBAN = "",
                    UID = paste("200304", SITE_ID, VISIT_NO,
                                sep = "_"),
                    DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"),
                    VISIT_NO = as.character(VISIT_NO),
                    MASTER_SITEID = SITE_ID) %>%
      dplyr::relocate(UID, .before = SITE_ID) %>%
      dplyr::relocate(URBAN, .before = RT_WSA) %>%
      dplyr::relocate(MASTER_SITEID, .after = SITE_ID)

    ##Set all column names equal to each other
    colnames(NRSA_0304_sites) =
      colnames(NRSA_0809_sites) =
      colnames(NRSA_1819_sites) =
      colnames(NRSA_1314_sites)

    NRSA_sites <-  dplyr::bind_rows(list(NRSA_1819_sites, NRSA_1314_sites,
                                         NRSA_0809_sites, NRSA_0304_sites))
    NRSA_sites$YEAR = lubridate::year(NRSA_sites$DATE_COL)

    ##Join with nrsa_comms1 to get site-level data

    nrsa_comms1 = nrsa_comms1 %>%
      tidyr::unite(UNIQUEID, c(UID, SITE_ID, YEAR, VISIT_NO),
                   sep = "_", remove = FALSE) %>%
      dplyr::left_join(NRSA_sites %>%
                         tidyr::unite(UNIQUEID, c(UID, SITE_ID, YEAR, VISIT_NO),
                                      sep = "_", remove = T), by = "UNIQUEID") %>%
      dplyr::relocate(tidyselect::contains("tax_"), .after = last_col()) %>%
      dplyr::mutate(ProjectLabel = ifelse(YEAR %in% c(2013, 2014),
                                          "NRSA1314",
                                          ifelse(YEAR %in% c(2008, 2009),
                                                 "NRSA0809",
                                                 ifelse(YEAR %in% c(2018, 2019),
                                                        "NRSA1819",
                                                        "WSA"))),
                    ProjectAssignedSampleLabel = UID,
                    NAWQA.SMCOD = UNIQUEID,
                    NAWQAStudyUnitCode = SITETYPE,
                    CollectionDate = DATE_COL,
                    StartTime = NA,
                    TimeDatum = NA,
                    CollectionYear = YEAR,
                    CollectionMonth = lubridate::month(DATE_COL),
                    CollectionDayOfYear = lubridate::yday(DATE_COL),
                    SiteVisitSampleNumber = VISIT_NO,
                    ProvisionalData = NA,
                    SiteNumber = MASTER_SITEID,
                    SiteName = MASTER_SITEID,
                    StateFIPSCode = NA,
                    CountyFIPSCode = NA,
                    Latitude_dd = LAT_DD83,
                    Longitude_dd = LON_DD83,
                    CoordinateDatum = "NAD83",
                    HUCCode = NA,
                    DrainageArea_mi2 = NA ,
                    SampleTypeCode = SAMPLE_TYPE,
                    IdentificationEntity = NA,
                    AreaSampTot_m2  = NA,
                    GeomorphicChannelUnit = NA,
                    ChannelBoundaries = NA,
                    ChannelFeatures = NA,
                    ReplicateType  = NA,
                    FieldSplitRatio = NA,
                    LabSubsamplingRatio = NA,
                    PropID = PCTCOUNT,
                    AreaSampTot_m2 = round(NUMTRANS / 10.76, 3)
      ) %>%
      dplyr::select(-SAMPLE_TYPE, -LAT_DD83, -LON_DD83, -SITETYPE,
                    -SITE_ID, -MASTER_SITEID, -UID, -UNIQUEID, -DATE_COL,
                    -YEAR, -PSTL_CODE, -US_L3CODE, -US_L3NAME, -VISIT_NO,
                    -AG_ECO9, -NRS13_URBN, -RT_NRSA, -PCTCOUNT, -NUMTRANS) %>%
      dplyr::relocate(tidyselect::contains("tax_"), .after = last_col())

    ##To make sure the NRSA sites are correct crosswalked across sampling rounds
    # rename the nrsa_comms1$SiteNumber based on the master crosswalk list from
    # Richard Mitchell (w/ updates to include MASTER_SITEID)

    ##if site number in nrsa_comms1 is in the SITEID in the master crosswalk list,
    ##match the numbers and pull the corresponding unique id, which is the crosswalked site id,
    ##else provide an NA
    nrsa_comms1$UNIQUE_ID <- ifelse(nrsa_comms1$SiteNumber %in% StreamData:::.NRSA_siteIDs$SITE_ID,
                                    StreamData:::.NRSA_siteIDs$UNIQUE_ID[match(nrsa_comms1$SiteNumber,
                                                                               StreamData:::.NRSA_siteIDs$SITE_ID)],
                                    NA)

    ##if site number in nrsa_comms1 is in the MASTER_SITEID in the master crosswalk list,
    ##match the numbers and pull the corresponding unique id, which is the crosswalked site id,
    ##else give the current UNIQUE ID
    nrsa_comms1$UNIQUE_ID <- ifelse(nrsa_comms1$SiteNumber %in% StreamData:::.NRSA_siteIDs$MASTER_SITEID,
                                    StreamData:::.NRSA_siteIDs$UNIQUE_ID[match(nrsa_comms1$SiteNumber,
                                                                               StreamData:::.NRSA_siteIDs$MASTER_SITEID)],
                                    nrsa_comms1$UNIQUE_ID)

    ##if there are any NA values in UNIQUE ID, replace these with the SiteNumber
    nrsa_comms1$SiteNumber = ifelse(is.na(nrsa_comms1$UNIQUE_ID),
                                    nrsa_comms1$SiteNumber,
                                    nrsa_comms1$UNIQUE_ID)

    ##remove the UNIQUEID column, as it is no longer needed
    nrsa_comms1 <- nrsa_comms1 %>%
      select(-UNIQUE_ID)

    ##Need to then join this dataset to invert_comms1
    invert_comms1[setdiff(names(nrsa_comms1), names(invert_comms1))] <- NA
    nrsa_comms1[setdiff(names(invert_comms1), names(nrsa_comms1))] <- NA

    ##Add Agency columns
    invert_comms1$Agency <- "USGS"
    nrsa_comms1$Agency <- "EPA"

    invert_comms1 <- invert_comms1  %>%
      dplyr::relocate(tidyselect::contains("tax_"), .after = last_col())

    nrsa_comms1 <- nrsa_comms1  %>%
      dplyr::relocate(tidyselect::contains("tax_"), .after = last_col()) %>%
      dplyr::relocate(tidyselect::any_of(colnames(invert_comms1)))

    invert_comms1 <- dplyr::bind_rows(invert_comms1, nrsa_comms1)

    invert_comms1 = invert_comms1 %>%
      dplyr::mutate(dplyr::across(tidyselect::starts_with("tax_"),
                                  ~ifelse(is.na(.x),
                                          0,
                                          .x)))

  } else{ }

  if(dataType == "occur") {
    invert_comms1 = invert_comms1 %>%
      dplyr::mutate(dplyr::across(tidyselect::starts_with("tax_"),
                                  ~replace(., . > 0, 1)))
  }

  invert_comms1$CollectionDate = as.Date(invert_comms1$CollectionDayOfYear,
                                         origin = paste(invert_comms1$CollectionYear-1,
                                                        '12-31',
                                                        sep = "-"))

  ##Remove the "tax_" prefix
  colnames(invert_comms1) = sub("tax_", "", colnames(invert_comms1))

  return(data.frame(invert_comms1))

}
