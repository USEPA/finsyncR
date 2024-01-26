##internal functions and datasets

#Aquire datasets
##no defaults
##only one agency at a time
##Needs to include
###taxa (fish, inverts)
###agency
###waterbody (streams, lakes, coastal)
acquireData <- function(taxa,
                       agency,
                       waterbody){
  if(!(taxa %in% c("inverts","fishes"))){
    stop("taxa must be set to 'inverts' or 'fishes'")}

  if(!(agency %in% c("USGS","EPA"))){
      stop("agency must be set to 'USGS', 'EPA', or both")
  }

  if(waterbody != "streams"){
    stop(paste("'aquiredata()' only works for 'streams' currently,",
               "future versions of 'finsyncR' will include data from",
               "lakes and coastal systems", sep = "\n"))
  }

  if(taxa == "inverts"){
    if(waterbody == "streams"){
      if(agency == "USGS"){
        Inverts <- utils::read.csv(base::unz(base::system.file("extdata",
                                                               "20201217.0749.InvertResults.zip",
                                                               package = "finsyncR"),
                                             "20201217.0749.InvertResults.csv"),
                                   colClasses = c("SiteNumber" = "character"),
                                   stringsAsFactors = FALSE)

        if(colnames(Inverts)[1] != "SIDNO"){
          colnames(Inverts)[1] = "SIDNO"
        }

        if(file.exists(system.file("extdata",
                                   "20201217.0749.InvertResults.csv",
                                   package = "finsyncR"))){
          unlink(system.file("extdata",
                             "20201217.0749.InvertResults.csv",
                             package = "finsyncR"))
        }

        Project <- data.table::fread(base::system.file("extdata",
                                                       "20201217.0749.Project.csv",
                                                       package = "finsyncR"),
                                     stringsAsFactors = FALSE,
                                     showProgress = F,
                                     data.table = F)

        database <- c("National Water Quality Assessment",
                      "Cooperative Water Program",
                      "Collection of Basic Records",
                      "Other Federal Agencies")

        Inverts <- Inverts %>%
          dplyr::filter(ProjectLabel %in% (Project %>%
                                             dplyr::filter(Program %in% database) %>%
                                             dplyr::distinct(ProjectLabel,
                                                             .keep_all = FALSE))[ , "ProjectLabel"])

        SamplingRatio_SamplerType <- .SamplingRatio_SamplerType

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
        invertSampleRef = data.table::fread(system.file("extdata",
                                                        "20201217.0749.InvertSamp.csv",
                                                        package = "finsyncR"),
                                            colClasses = c("SiteNumber" = "character"),
                                            stringsAsFactors = FALSE,
                                            data.table = F) %>%
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
        invertsamp = data.table::fread(system.file("extdata",
                                                   "20201217.0749.InvertSamp.csv",
                                                   package = "finsyncR"),
                                       colClasses = c("SiteNumber" = "character"),
                                       stringsAsFactors = FALSE,
                                       data.table = F) %>%
          dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
          dplyr::select(SIDNO,
                        SiteNumber,
                        AreaSampTot_m2,
                        GeomorphicChannelUnit,
                        ChannelBoundaries,
                        ChannelFeatures)

        ##Gather Sample inventory information (specifically, just replicate type)
        invertsampinv = data.table::fread(system.file("extdata",
                                                      "20201217.0749.SampleInv.csv",
                                                      package = "finsyncR"),
                                          colClasses = c("SiteNumber" = "character"),
                                          stringsAsFactors = FALSE,
                                          data.table = F) %>%
          dplyr::rename(SIDNO = grep("SIDNO", names(.))) %>%
          dplyr::select(SIDNO,
                        ReplicateType)

        ##Gather site level information
        invertsite = data.table::fread(system.file("extdata",
                                                   "20201217.0749.SiteInfo.csv",
                                                   package = "finsyncR"),
                                       colClasses = c("SiteNumber" = "character"),
                                       stringsAsFactors = FALSE,
                                       data.table = F) %>%
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
        return(TotalRows)
      }
      else if(agency == "EPA"){

        UA <- paste('Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:98.0)',
                    'Gecko/20100101 Firefox/98.0')
        ##Read in datasets directly from EPA website - may want a more stable source
        ##in the future (github repo?)
        NRSA_1819_inverts = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2021-04/nrsa_1819_benthic_macroinvertebrate_count_-_data.csv",
                                                                       httr::add_headers(`User-Agent` = UA)),
                                                             encoding = "UTF-8", as = "text"),
                                               colClasses = c("UID" = "character"),
                                               stringsAsFactors = FALSE,
                                               data.table = F))

        NRSA_1819_sites = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/system/files/other-files/2022-01/nrsa-1819-site-information-data-updated.csv",
                                                                     httr::add_headers(`User-Agent` = UA)),
                                                           encoding = "UTF-8", as = "text"),
                                             colClasses = c("UID" = "character"),
                                             stringsAsFactors = FALSE,
                                             data.table = F))

        NRSA_1314_inverts = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2019-04/nrsa1314_bentcnts_04232019.csv",
                                                                       httr::add_headers(`User-Agent` = UA)),
                                                             encoding = "UTF-8", as = "text"),
                                               colClasses = c("UID" = "character"),
                                               stringsAsFactors = FALSE,
                                               data.table = F))

        ##ISSUE when as = "text", so allow it to parse automatically for now
        tmp <- tempfile(fileext = '.csv')
        writeBin(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2019-04/nrsa1314_siteinformation_wide_04292019.csv",
                                         httr::add_headers(`User-Agent` = UA)),
                               encoding = "UTF-8", as = "raw"),
                 tmp)
        NRSA_1314_sites <- utils::read.csv(tmp,
                                           colClasses = c("UID" = "character"),
                                           stringsAsFactors = FALSE)
        if(file.exists(tmp)){
          unlink(tmp)
        }

        NRSA_0809_inverts = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2016-11/nrsa0809bentcts.csv",
                                                                       httr::add_headers(`User-Agent` = UA)),
                                                             encoding = "UTF-8", as = "text"),
                                               colClasses = c("UID" = "character"),
                                               stringsAsFactors = FALSE,
                                               data.table = F))

        NRSA_0809_inverts_tax = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2016-06/nrsa_0809_benttaxa.csv",
                                                                           httr::add_headers(`User-Agent` = UA)),
                                                                 encoding = "UTF-8", as = "text"),
                                                   stringsAsFactors = FALSE,
                                                   data.table = F))

        NRSA_0809_sites = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                                                                     httr::add_headers(`User-Agent` = UA)),
                                                           encoding = "UTF-8", as = "text"),
                                             colClasses = c("UID" = "character"),
                                             stringsAsFactors = FALSE,
                                             data.table = F))

        NRSA_0304_inverts = rbind((data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2014-10/wsa_bencnt_genus_ts_final_part1.csv",
                                                                             httr::add_headers(`User-Agent` = UA)),
                                                                   encoding = "UTF-8", as = "text"),
                                                     stringsAsFactors = FALSE,
                                                     data.table = F)),
                                  data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2014-10/wsa_bencnt_genus_ts_final_part2.csv",
                                                                            httr::add_headers(`User-Agent` = UA)),
                                                                  encoding = "UTF-8", as = "text"),
                                                    stringsAsFactors = FALSE,
                                                    data.table = F))

        NRSA_0304_sites = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2014-10/wsa_siteinfo_ts_final.csv",
                                                                     httr::add_headers(`User-Agent` = UA)),
                                                           encoding = "UTF-8", as = "text"),
                                             stringsAsFactors = FALSE,
                                             data.table = F))


        ##add wetted width to datasets
        WSAhab <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2014-10/phabbest.csv",
                                                             httr::add_headers(`User-Agent` = UA)),
                                                   encoding = "UTF-8", as = "text"),
                                     stringsAsFactors = FALSE,
                                     data.table = F)) %>%
          dplyr::select(SITE_ID, YEAR, VISIT_NO, XWIDTH)

        hab0809 <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2015-09/phabmed.csv",
                                                              httr::add_headers(`User-Agent` = UA)),
                                                    encoding = "UTF-8", as = "text"),
                                      stringsAsFactors = FALSE,
                                      data.table = F)) %>%
          dplyr::select(SITE_ID, YEAR, VISIT_NO, XWIDTH)

        hab1314 <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_phabmed_04232019.csv",
                                                              httr::add_headers(`User-Agent` = UA)),
                                                    encoding = "UTF-8", as = "text"),
                                      stringsAsFactors = FALSE,
                                      data.table = F)) %>%
          dplyr::select(SITE_ID, UID, VISIT_NO, XWIDTH) %>%
          dplyr::mutate(UID = as.character(UID))

        hab1819 <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2021-04/nrsa_1819_physical_habitat_larger_set_of_metrics_-_data.csv",
                                                              httr::add_headers(`User-Agent` = UA)),
                                                    encoding = "UTF-8", as = "text"),
                                      stringsAsFactors = FALSE,
                                      data.table = F)) %>%
          dplyr::select(SITE_ID, DATE_COL, VISIT_NO, XWIDTH)

        ##2003/2004
        NRSA_0304_inverts = NRSA_0304_inverts %>%
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
        NRSA_0304_inverts$UID <- paste("200004",
                                       NRSA_0304_inverts$SITE_ID,
                                       NRSA_0304_inverts$VISIT_NO,
                                       sep = "-")

        ##Update column names to match those of 08/09 and 13/14
        colnames(NRSA_0304_inverts)[c(4,10)] = c("SAMPLE_TYPE", "TOTAL")

        ##Rearrange columns to match those of 08/09 and 13/14
        NRSA_0304_inverts <- NRSA_0304_inverts %>%
          dplyr::relocate(any_of(c("UID", "SITE_ID", "YEAR", "VISIT_NO", "SAMPLE_TYPE",
                                   "TARGET_TAXON", "TOTAL", "PHYLUM", "CLASS", "ORDER",
                                   "FAMILY", "GENUS")))

        NRSA_0304_inverts <- NRSA_0304_inverts %>%
          dplyr::left_join(NRSA_0809_inverts_tax %>%
                             dplyr::select(SUBFAMILY, GENUS),
                           by = "GENUS") %>%
          dplyr::mutate(SUBFAMILY = ifelse(is.na(SUBFAMILY),
                                    "",
                                    SUBFAMILY)) %>%
          dplyr::relocate("SUBFAMILY",.after = "FAMILY")

        ##2008/2009
        ##Filter to BERW; remove columns that are not needed
        NRSA_0809_inverts = NRSA_0809_inverts %>%
          dplyr::select(-IS_DISTINCT, -TOTAL300, -IS_DISTINCT300, -BENT_COM, -DATE_BENT,
                        -SAMPLE_CAT, -PUBLICATION_DATE) %>%
          dplyr::mutate(YEAR = paste("20", stringr::str_sub(DATE_COL, -2,-1), sep = ""),
                        YEAR = as.numeric(YEAR)) %>%
          dplyr::select(-DATE_COL)

        ##Join the count data to the taxa data to match those of 03/04
        NRSA_0809_inverts<- NRSA_0809_inverts %>%
          dplyr::left_join(NRSA_0809_inverts_tax %>%
                             dplyr::select(TAXA_ID, PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, GENUS),
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
          dplyr::select(-IS_DISTINCT, -TOTAL300, -IS_DISTINCT300, -TOTAL300_OE,
                        -PUBLICATION_DATE, -TRIBE, -TAXA_ID)

        ##Update this weird TARGET_TAXON that is THIENEMANNIMYIA GENUS GR., but does not
        ##have a genus associated with it; so make genus = "THIENEMANNIMYIA"
        NRSA_1314_inverts$GENUS = ifelse(grepl("GENUS", NRSA_1314_inverts$TARGET_TAXON),
                                         "THIENEMANNIMYIA",
                                         NRSA_1314_inverts$GENUS)


        ##2018/2019

        ##Filter to BERW; remove columns that are not needed (taxonomic resolutions are
        ##not available in all datasets, so remove those that are not found across data)
        NRSA_1819_inverts <- NRSA_1819_inverts %>%
          dplyr::select(-IS_DISTINCT, -TOTAL300, -IS_DISTINCT300, -EPA_REG,
                        -PUBLICATION_DATE, -TRIBE, -TAXA_ID,
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
        NRSA_inverts$GENUS <- ifelse(NRSA_inverts$GENUS == "NA",
               "",
               NRSA_inverts$GENUS)


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
        NRSADenconv <- data.table::fread(base::system.file("extdata",
                                                           "EPA_DensityConv.csv",
                                                           package = "finsyncR"),
                                         colClasses = c("SITE_ID" = "character"),
                                         stringsAsFactors = FALSE,
                                         data.table = F) %>%
          dplyr::select(-UNIQUE_ID,-SAMPLE_TYPE)

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
        ##Step 6: join w/ site level data

        ##18/19 - MISSING RT_NRSA, will put in ""
        ##UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE, LAT_DD83, LON_DD83,
        ##AG_ECO9, URBN_NRS18, US_L3CODE, US_L3NAME
        NRSA_1819_sites <- suppressMessages({NRSA_1819_sites %>%
            dplyr::left_join(hab1819) %>%
            dplyr::select(UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE,
                          LAT_DD83, LON_DD83, AG_ECO9, URBN_NRS18,
                          US_L3CODE, US_L3NAME, XWIDTH) %>%
            dplyr::mutate(RT_NRSA = "",
                          DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"),
                          VISIT_NO = as.character(VISIT_NO),
                          MASTER_SITEID = SITE_ID) %>%
            dplyr::relocate(RT_NRSA, .before = US_L3CODE) %>%
            dplyr::relocate(MASTER_SITEID, .after = SITE_ID)})

        ##13/14
        ##UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE, LAT_DD83, LON_DD83,
        ##AG_ECO9, NRS13_Urban, RT_NRSA, US_L3CODE, US_L3NAME,
        NRSA_1314_sites <- suppressMessages({NRSA_1314_sites %>%
            dplyr::left_join(hab1314) %>%
            dplyr::select(UID, SITE_ID, VISIT_NO, SITETYPE, DATE_COL, PSTL_CODE,
                          LAT_DD83, LON_DD83, AG_ECO9, NRS13_URBN, RT_NRSA,
                          US_L3CODE, US_L3NAME, XWIDTH) %>%
            dplyr::mutate(RT_NRSA = ifelse(RT_NRSA == "?",
                                           "",
                                           RT_NRSA),
                          DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"),
                          VISIT_NO = as.character(VISIT_NO),
                          MASTER_SITEID = SITE_ID) %>%
            dplyr::relocate(MASTER_SITEID, .after = SITE_ID)})

        ##08/09 - MISSING L3 NAME (not a problem)
        ##UID, SITE_ID, VISIT_NO, SITE_CLASS, DATE_COL, STATE, LAT_DD83, LONG_DD83,
        ##AGGR_ECO9_2015, URBAN, RT_NRSA, US_L3CODE_2015
        NRSA_0809_sites <- suppressMessages({NRSA_0809_sites %>%
            dplyr::left_join(hab0809) %>%
            dplyr::select(UID, SITE_ID, MASTER_SITEID, VISIT_NO, SITE_CLASS, DATE_COL, STATE,
                          LAT_DD83, LON_DD83, AGGR_ECO9_2015, URBAN, RT_NRSA,
                          US_L3CODE_2015, XWIDTH) %>%
            dplyr::mutate(RT_NRSA = ifelse(RT_NRSA == "R",
                                           "R",
                                           ifelse(RT_NRSA == "S",
                                                  "In",
                                                  ifelse(RT_NRSA == "T",
                                                         "Im",
                                                         "")))) %>%
            dplyr::mutate(US_L3NAME = as.character(""),
                          DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"),
                          VISIT_NO = as.character(VISIT_NO))%>%
            dplyr::relocate(XWIDTH, .after = US_L3NAME)})

        ##For some reason, one MASTER_SITEID is missing for one replicate sample, so give it the site id
        NRSA_0809_sites$MASTER_SITEID[which(NRSA_0809_sites$SITE_ID == "FW08LA004")] = "FW08LA004"

        ##03/04 - MISSING UID (will create in the same way as above), URBAN
        ##SITE_ID, VISIT_NO, SITETYPE, DATE_COL, STATE, LAT_DD, LON_DD,
        ##ECOWSA9, RT_WSA, ECO3, ECO3_NM
        NRSA_0304_sites <- suppressMessages({NRSA_0304_sites %>%
            dplyr::left_join(WSAhab) %>%
            dplyr::select(SITE_ID, VISIT_NO, SITETYPE, DATE_COL, STATE, LAT_DD, LON_DD,
                          ECOWSA9, RT_WSA, ECO3, ECO3_NM, XWIDTH) %>%
            dplyr::mutate(RT_WSA = ifelse(RT_WSA == "R",
                                          "R",
                                          ifelse(RT_WSA == "S",
                                                 "In",
                                                 ifelse(RT_WSA == "T",
                                                        "Im",
                                                        "")))) %>%
            dplyr::mutate(URBAN = "",
                          UID = paste("200004", SITE_ID, VISIT_NO,
                                      sep = "-"),
                          DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"),
                          VISIT_NO = as.character(VISIT_NO),
                          MASTER_SITEID = SITE_ID) %>%
            dplyr::relocate(UID, .before = SITE_ID) %>%
            dplyr::relocate(URBAN, .before = RT_WSA) %>%
            dplyr::relocate(MASTER_SITEID, .after = SITE_ID)})

        ##Set all column names equal to each other
        colnames(NRSA_0304_sites) =
          colnames(NRSA_0809_sites) =
          colnames(NRSA_1819_sites) =
          colnames(NRSA_1314_sites)

        NRSA_sites <-  dplyr::bind_rows(list(NRSA_1819_sites, NRSA_1314_sites,
                                             NRSA_0809_sites, NRSA_0304_sites))
        NRSA_sites$YEAR = lubridate::year(NRSA_sites$DATE_COL)



        ##Join with nrsa_comms1 to get site-level data

        NRSA_inverts = NRSA_inverts %>%
          tidyr::unite(UNIQUEID, c(UID, SITE_ID, YEAR, VISIT_NO),
                       sep = "_", remove = FALSE) %>%
          dplyr::left_join(NRSA_sites %>%
                             tidyr::unite(UNIQUEID, c(UID, SITE_ID, YEAR, VISIT_NO),
                                          sep = "_", remove = T), by = "UNIQUEID") %>%
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
                        GeomorphicChannelUnit = NA,
                        ChannelBoundaries = NA,
                        ChannelFeatures = NA,
                        ReplicateType  = NA,
                        FieldSplitRatio = NA,
                        LabSubsamplingRatio = NA,
                        PropID = PCTCOUNT,
                        AreaSampTot_m2 = round((NUMTRANS*CONVS) / 10.76, 3),
                        WettedWidth = XWIDTH
          ) %>%
          dplyr::select(-LAT_DD83, -LON_DD83, -SITETYPE, -SAMPLE_TYPE,
                        -SITE_ID, -MASTER_SITEID, -UID, -UNIQUEID, -DATE_COL,
                        -YEAR, -PSTL_CODE, -US_L3CODE, -US_L3NAME, -VISIT_NO,
                        -AG_ECO9, -NRS13_URBN, -RT_NRSA, -PCTCOUNT, -NUMTRANS,-CONVS,
                        -XWIDTH)

        ##To make sure the NRSA sites are correct crosswalked across sampling rounds
        # rename the NRSA_inverts$SiteNumber based on the master crosswalk list from
        # Richard Mitchell (w/ updates to include MASTER_SITEID)

        ##if site number in NRSA_inverts is in the SITEID in the master crosswalk list,
        ##match the numbers and pull the corresponding unique id, which is the crosswalked site id,
        ##else provide an NA
        NRSA_inverts$UNIQUE_ID <- ifelse(NRSA_inverts$SiteNumber %in% .NRSA_siteIDs$SITE_ID,
                                        .NRSA_siteIDs$UNIQUE_ID[match(NRSA_inverts$SiteNumber,
                                                                      .NRSA_siteIDs$SITE_ID)],
                                        NA)

        ##if site number in NRSA_inverts is in the MASTER_SITEID in the master crosswalk list,
        ##match the numbers and pull the corresponding unique id, which is the crosswalked site id,
        ##else give the current UNIQUE ID
        NRSA_inverts$UNIQUE_ID <- ifelse(NRSA_inverts$SiteNumber %in% .NRSA_siteIDs$MASTER_SITEID,
                                        .NRSA_siteIDs$UNIQUE_ID[match(NRSA_inverts$SiteNumber,
                                                                      .NRSA_siteIDs$MASTER_SITEID)],
                                        NRSA_inverts$UNIQUE_ID)

        ##if there are any NA values in UNIQUE ID, replace these with the SiteNumber
        NRSA_inverts$SiteNumber = ifelse(is.na(NRSA_inverts$UNIQUE_ID),
                                        NRSA_inverts$SiteNumber,
                                        NRSA_inverts$UNIQUE_ID)
        NRSA_inverts = NRSA_inverts%>%
          dplyr::select(-UNIQUE_ID)
        return(NRSA_inverts)

      }
    }

  } else
    if(taxa == "fishes"){

    if(agency == "USGS"){
      fish <- utils::read.csv(base::unz(base::system.file("extdata",
                                                          "20201217.0745.FishResults.zip",
                                                          package = "finsyncR"),
                                        "20201217.0745.FishResults.csv"),
                              colClasses = c("SiteNumber" = "character"),
                              stringsAsFactors = FALSE)

      if(colnames(fish)[1] != "SIDNO"){
        colnames(fish)[1] = "SIDNO"
      }
      ##Remove the unzipped file from the system
      if(file.exists(system.file("extdata",
                                 "20201217.0745.FishResults.csv",
                                 package = "finsyncR"))){
        unlink(system.file("extdata",
                           "20201217.0745.FishResults.csv",
                           package = "finsyncR"))
      }
      Project <- data.table::fread(system.file("extdata",
                                               "20201217.0745.Project.csv",
                                               package = "finsyncR"),
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
                                            package = "finsyncR"),
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
                                              package = "finsyncR"),
                                  colClasses = c("SiteNumber" = "character"),
                                  data.table = F)
      if(colnames(sample)[1] != "SIDNO"){
        colnames(sample)[1] = "SIDNO"
      }

      sample = sample %>%
        dplyr::select(SIDNO, ReachLengthFished_m)

      samplemethod = data.table::fread(system.file("extdata",
                                                   "20201217.0745.FishMethodAndSubreachInfo.csv",
                                                   package = "finsyncR"),
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

      fish_info = fish_info %>%
        group_by(SIDNO) %>%
        mutate(clipper = paste(MethodCode, collapse = "_")) %>%
        ungroup() %>%
        mutate(FISH_PROTOCOL = ifelse(grepl("Boat|Beach|Snorkel|Additional|Gill",clipper) & !grepl("Tow|Back",clipper),
                                      "BOATABLE",
                                      ifelse(clipper == "BeachSiene",
                                             "BOATABLE",
                                             ifelse(grepl("Tow",clipper),
                                                    "LG_WADEABLE",
                                                    ifelse(grepl("Back|NRSA",clipper),
                                                           "SM_WADEABLE",
                                                           "SM_WADEABLE"))))) %>%
        dplyr::select(-clipper)

    return(data.frame(fish_info))

    } else
      if(agency == "EPA"){
      UA <- paste('Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:98.0)',
                  'Gecko/20100101 Firefox/98.0')
      #READ IN STUFF
      ##Read in datasets directly from EPA website - may want a more stable source
      ##in the future (github repo?)
      NRSA_1819_fishcnt = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/system/files/other-files/2022-03/nrsa-1819-fish-count-data.csv",
                                                                     httr::add_headers(`User-Agent` = UA)),
                                                           encoding = "UTF-8", as = "text"),
                                             colClasses = c("UID" = "character"),
                                             stringsAsFactors = FALSE,
                                             data.table = F))

      NRSA_1819_sites = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/system/files/other-files/2022-01/nrsa-1819-site-information-data-updated.csv",
                                                                   httr::add_headers(`User-Agent` = UA)),
                                                         encoding = "UTF-8", as = "text"),
                                           colClasses = c("UID" = "character"),
                                           stringsAsFactors = FALSE,
                                           data.table = F))

      ##FIX 1819 COUNT UIDs
      NRSA_1819_fishcnt$UID <- NRSA_1819_sites$UID[match(paste(NRSA_1819_fishcnt$SITE_ID,
                                                               NRSA_1819_fishcnt$DATE_COL, sep = "_"),
                                                         paste(NRSA_1819_sites$SITE_ID,
                                                               NRSA_1819_sites$DATE_COL, sep = "_"))]


      NRSA_1314_fishcnt = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_fishcts_04232019.csv",
                                                                     httr::add_headers(`User-Agent` = UA)),
                                                           encoding = "UTF-8", as = "text"),
                                             colClasses = c("UID" = "character"),
                                             stringsAsFactors = FALSE,
                                             data.table = F))

      tmp <- tempfile(fileext = '.csv')
      writeBin(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2019-04/nrsa1314_siteinformation_wide_04292019.csv",
                                       httr::add_headers(`User-Agent` = UA)),
                             encoding = "UTF-8", as = "raw"),
               tmp)
      NRSA_1314_sites <- utils::read.csv(tmp,
                                         colClasses = c("UID" = "character"),
                                         stringsAsFactors = FALSE)
      if(file.exists(tmp)){
        unlink(tmp)
      }


      NRSA_0809_fishcnts = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2015-09/fishcts.csv",
                                                                      httr::add_headers(`User-Agent` = UA)),
                                                            encoding = "UTF-8", as = "text"),
                                              colClasses = c("UID" = "character"),
                                              stringsAsFactors = FALSE,
                                              data.table = F))

      NRSA_0809_sites = (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                                                                   httr::add_headers(`User-Agent` = UA)),
                                                         encoding = "UTF-8", as = "text"),
                                           colClasses = c("UID" = "character"),
                                           stringsAsFactors = FALSE,
                                           data.table = F))
      ##add wetted width to datasets
      hab0809 <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2015-09/phabmed.csv",
                                                            httr::add_headers(`User-Agent` = UA)),
                                                  encoding = "UTF-8", as = "text"),
                                    stringsAsFactors = FALSE,
                                    data.table = F)) %>%
        dplyr::select(SITE_ID, YEAR, VISIT_NO, XWIDTH)

      hab1314 <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_phabmed_04232019.csv",
                                                            httr::add_headers(`User-Agent` = UA)),
                                                  encoding = "UTF-8", as = "text"),
                                    stringsAsFactors = FALSE,
                                    data.table = F)) %>%
        dplyr::select(SITE_ID, UID, VISIT_NO, XWIDTH) %>%
        dplyr::mutate(UID = as.character(UID))

      hab1819 <- (data.table::fread(httr::content(httr::GET("https://www.epa.gov/sites/default/files/2021-04/nrsa_1819_physical_habitat_larger_set_of_metrics_-_data.csv",
                                                            httr::add_headers(`User-Agent` = UA)),
                                                  encoding = "UTF-8", as = "text"),
                                    stringsAsFactors = FALSE,
                                    data.table = F)) %>%
        dplyr::select(SITE_ID, DATE_COL, VISIT_NO, XWIDTH)

      NRSA_1314_fishtax <- data.table::fread(system.file("extdata",
                                                         "updateNRSAfishtax.csv",
                                                         package = "finsyncR"),
                                             data.table = F)

      NRSA_fish_sampleinfo <- data.table::fread(system.file("extdata",
                                                            "NRSA_Fish_SamplingInfo.csv",
                                                            package = "finsyncR"),
                                                colClasses = c("UID" = "character"),
                                                stringsAsFactors = FALSE,
                                                data.table = F)

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

      suppressMessages({NRSA_0809_s <- NRSA_0809_sites %>%
        dplyr::left_join(hab0809) %>%
        dplyr::select(UID, SITE_ID, MASTER_SITEID, DATE_COL, VISIT_NO, STATE, LOC_NAME, LAT_DD83, LON_DD83, XWIDTH) %>%
        mutate(DATE_COL = as.Date(DATE_COL, "%d-%B-%y"))})

      suppressMessages({NRSA_1314_s <- NRSA_1314_sites %>%
        dplyr::left_join(hab1314) %>%
        dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, BOAT_WADE, NARS_NAME, LAT_DD83, LON_DD83, XWIDTH) %>%
        mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))})

      suppressMessages({NRSA_1819_s <- NRSA_1819_sites %>%
        dplyr::left_join(hab1819) %>%
        dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, UNIQUE_ID, NARS_NAME, LAT_DD83, LON_DD83, XWIDTH) %>%
        mutate(VISIT_NO = as.numeric(ifelse(VISIT_NO == "R",
                                            2,
                                            VISIT_NO)))%>%
        mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))})

      NRSA_fish_sites <- bind_rows(NRSA_0809_s, NRSA_1314_s, NRSA_1819_s) %>%
        relocate(UNIQUE_ID, .after = SITE_ID) %>%
        mutate(SITENAME = ifelse(is.na(LOC_NAME),
                                 NARS_NAME,
                                 LOC_NAME)) %>%
        dplyr::select(-c(LOC_NAME, NARS_NAME)) %>%
        relocate(SITENAME, .after = VISIT_NO)

      ##MASTER ID LIST
      NRSA_fish_sites$UNIQUE_ID <- ifelse(NRSA_fish_sites$SITE_ID %in% .NRSA_siteIDs$SITE_ID,
                                          .NRSA_siteIDs$UNIQUE_ID[match(NRSA_fish_sites$SITE_ID,
                                                                        .NRSA_siteIDs$SITE_ID)],
                                          NA)

      ##if site number in nrsa_comms1 is in the MASTER_SITEID in the master crosswalk list,
      ##match the numbers and pull the corresponding unique id, which is the crosswalked site id,
      ##else give the current UNIQUE ID
      NRSA_fish_sites$UNIQUE_ID <- ifelse(NRSA_fish_sites$SITE_ID %in% .NRSA_siteIDs$MASTER_SITEID,
                                          .NRSA_siteIDs$UNIQUE_ID[match(NRSA_fish_sites$SITE_ID,
                                                                        .NRSA_siteIDs$MASTER_SITEID)],
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
        mutate(FISH_PROTOCOL2 = ifelse(FISH_PROTOCOL %in% c("BOATABLE",
                                                           "LG_NONWADEABLE",
                                                           "SM_NONWADEABLE"),
                                      "BOATABLE",
                                      ifelse(FISH_PROTOCOL == "WADEABLE" & XWIDTH >= 25,
                                             "LG_WADEABLE",
                                             ifelse(FISH_PROTOCOL %in% "WADEABLE" & XWIDTH < 25,
                                                    "SM_WADEABLE",
                                                    ifelse(FISH_PROTOCOL %in% c("",NA) & XWIDTH < 12.5,
                                                           "SM_WADEABLE",
                                                           ifelse(FISH_PROTOCOL == "" & XWIDTH <= 25,
                                                                  "LG_WADEABLE",
                                                                  ifelse(FISH_PROTOCOL == "" & XWIDTH > 25,
                                                                         "BOATABLE",
                                                                         FISH_PROTOCOL)))))),
               FISH_PROTOCOL2 = ifelse(is.na(FISH_PROTOCOL2),
                                       FISH_PROTOCOL,
                                       FISH_PROTOCOL2),
               FISH_PROTOCOL2 = ifelse(FISH_PROTOCOL2 == "WADEABLE",
                                       "SM_WADEABLE",
                                       FISH_PROTOCOL2),
               FISH_PROTOCOL = FISH_PROTOCOL2,
               StandardMethod = ifelse(MethodBasic == "Seine",
                                       NumberSeineHauls,
                                       MinutesShockTime)
        ) %>%
        dplyr::select(-BOAT_WADE,-FISH_PROTOCOL2)


      ##Add no fish collected rows
      NRSA_FISH_wSite <-  bind_rows(NRSA_FISH_wSite,
                                    NRSA_fish_info %>%
                                      filter(SAMPLED_FISH == "YES-NO FISH INFERRED") %>%
                                      mutate(RCH_LENGTH = 150,
                                             FISH_PROTOCOL = "SM_WADEABLE",
                                             MethodBasic = "Shocking",
                                             MinutesShockTime = 10,
                                             StandardMethod = MinutesShockTime,
                                             FAMILY = "No Fish",
                                             GENUS = "No fish",
                                             SPECIES = "No fish",
                                             SCIENTIFIC = "No fish collected") %>%
                                      dplyr::select(-BOAT_WADE,-XWIDTH)
      ) %>%
        rename("WettedWidth" = "XWIDTH")


      return(data.frame(NRSA_FISH_wSite))
    }

  }

}



##invert taxonomy fix function
invertTaxFix <- function(dataset,
                         TaxonFix,
                         agency,
                         taxonLevel){
  ##setup higher taxonomic resolution cleaning
    ##set up code to have the genus-level taxonomy fixed
  #create variable TaxonFix = none, lump, remove
  #none = no change, lump = lump genera through time, remove = remove observation only if spp. level ID does not exist
  ##Generate a list of those individual genera that make up the slash genera
  slashedgen <- unique(unlist(strsplit(.slashgen_fin,"/")))

  cnt = c()
  hldr = c()
  gns = c()
  slashedgen <- slashedgen[order(slashedgen)]
  for(i in slashedgen){
    hldr <- grep(i, .slashgen_fin, fixed = T)
    cnt <- c(cnt, hldr)
    gns = c(gns, rep(i, times = length(hldr)))
  }

  dat1 = data.frame(Genus = gns,
                    Slash = .slashgen_fin[cnt])

  dat1F <- dat1 %>%
    group_by(Genus) %>%
    slice(1) %>%
    ungroup()
  ##dat1 should NOT be combined with the cluster labels like this; instead it should be a separate step
  dat1L <- dat1


  ##From the genus to slash dataset from above, remove all of those that do not
  ## appear in the clust_labels dataset
  ##First, match the group information based on the genera present in both the dat1
  ## and clust_labels dataset
  dat1L$group <- .clust_labels[match(dat1L$Genus,
                                    .clust_labels$genus),]$group

  #There are some problems here, since there are multiple "slash" genera per
  #individual genus, so need to lump these
  #Select those genera that appear in >1 "slash" genera
  probslash <- dat1L %>%
    group_by(Genus) %>%
    mutate(count = n()) %>%
    filter(count >1) %>%
    dplyr::select(-count)

  ##Split this dataset
  probslashl <- split(probslash, probslash$Genus)

  ##Take the unique genera in the "slash" genera and join them into a larger
  ## "slash" genus
  for(i in 1:length(probslashl)){
    probslashl[[i]]$Fix <- paste(sort(unique(unlist(strsplit(probslashl[[i]]$Slash,"/")))),
                                 collapse = "/")
  }

  ##Bind these together
  fix_slash <- data.frame(dplyr::bind_rows(probslashl))

  ##Fix a naming issue; going to make sure this is fixed.

  ##Remove all observations with an NA for the group in dat1 (does not appear in
  ## clust_labels); then take 1 observation for each slash genus and generate
  ## information (this does not matter) to better join this dataset with the
  ## clust_labels dataset
  dat1L <- dat1L %>%
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
  dat1L$lump <- .clust_labels[match(dat1L$group,
                                    .clust_labels$group),]$lump

  ##Finally, join these datasets, so that all slashes will be successfully pulled
  ## into the appropriate "lump" group
  slashlump <- dplyr::bind_rows(list(.clust_labels,
                                     dat1L))

  ##Fix a naming issue with "Einfeldia" groups
  slashlump$lump[grep("Einfeldia/Glyptotendipes", slashlump$lump)] <- "Chironomus/Einfeldia/Glyptotendipes"

  if(agency == "USGS"){
    dataset$Order = ifelse(dataset$Family %in% .taxlu$Family,
                           .taxlu$Order[match(dataset$Family,
                                             .taxlu$Family)],
                           dataset$Order)

    # NEED TO FIX SLASHIES HERE


    dataset = dataset %>%
      mutate(Class = ifelse(Family == "Aeolosomatidae",
                            "",
                            Class),
             Family = ifelse(Family  %in% c("Naididae", "Tubificidae"),
                             "Tubificidae",
                             ifelse(Family  %in% c("Pilidae", "Ampullariidae"),
                                    "Ampullariidae",
                                    Family)),
             #Code below is only for USGS and taxonomy dataset
             Order = ifelse(Superfamily == "Ampullarioidea",
                            "Architaenioglossa",
                            ifelse(Superfamily %in% c("Truncatelloidea","Vermetoidea"),
                                   "Neotaenioglossa",
                                   Order)),
             Order = ifelse(Superfamily == "" & Family =="" & Order == "Mesogastropoda",
                            "",
                            Order))

    if(taxonLevel %in% c("Genus","Mixed")){

      ##Convert those genera that need to be updated
      dataset$Genus <- ifelse(dataset$Genus %in% .switch1to1$BenchGenus,
                              .switch1to1$Genus[match(dataset$Genus,
                                                      .switch1to1$BenchGenus)],
                              dataset$Genus)

        #If genera that are one of genera in dat1, rename the Genus with the slash
        #label from dat1, else, keep the original Genus label
        dataset$Genus <- ifelse(dataset$Genus %in% dat1F$Genus,
                                dat1F$Slash[match(dataset$Genus,
                                                 dat1F$Genus)],
                                dataset$Genus)

        #If genera that are one of problem slash genera, rename the Genus with the lumped
        #label from fix_slash, else, keep the original Genus label
        dataset$Genus <- ifelse(dataset$Genus %in% fix_slash$Slash,
                                fix_slash$Fix[match(dataset$Genus,
                                                    fix_slash$Slash)],
                                dataset$Genus)

      if(TaxonFix == "none"){

      } else if(TaxonFix == "lump"){
        #If bench genera that are one of bench genera in clust_labels, rename the Genus with the lump label from clust_labels
        #else, keep the original Genus label
        #create bench genus in dataset
        dataset <- dataset %>%
          dplyr::mutate(BenchGenus = as.character(gsub( " .*$", "", BenchTaxonName)))

        dataset$Genus <- ifelse(dataset$BenchGenus %in% .clust_labels$genus,
                                .clust_labels$lump[match(dataset$BenchGenus,
                                                         .clust_labels$genus)],
                                dataset$Genus)

        dataset <- dataset %>%
          dplyr::select(-BenchGenus)



        dataset$Genus <- ifelse(dataset$Genus %in% .clust_labels$genus,
                                .clust_labels$lump[match(dataset$Genus,
                                                         .clust_labels$genus)],
                                dataset$Genus)


        ##Do the same for those in slashlump
        dataset$Genus <- ifelse(dataset$Genus %in% slashlump$genus,
                                slashlump$lump[match(dataset$Genus,
                                                     slashlump$genus)],
                                dataset$Genus)



      } else if(TaxonFix == "remove"){

        #filter out observations that have ambiguous taxa designations
        ##remove those genera in the slashlump and remove all "/" genera
        dataset <- dataset %>%
          dplyr::filter(!(Genus %in% slashlump$genus &
                            PublishedTaxonNameLevel == "Genus")) %>%
          dplyr::filter(!(grepl("/", Genus)))
      }
    } else if(taxonLevel != "Genus"){}

    dataset$Mixed = ifelse(dataset$Genus != "",
                           dataset$Genus,
                           ifelse(dataset$Subfamily != "",
                                  dataset$Subfamily,
                                  ifelse(dataset$Family != "",
                                         dataset$Family,
                                         ifelse(dataset$Order != "",
                                                dataset$Order,
                                                ifelse(dataset$Class != "",
                                                       dataset$Class,
                                                       dataset$Phylum)))))

    if(taxonLevel == "Subfamily"){
      dataset$Subfamily = ifelse(dataset$Subfamily == "",
                                 dataset$Family,
                                 dataset$Subfamily)
    }

  }
  else if(agency == "EPA"){

    dataset$ORDER = ifelse(dataset$FAMILY %in% .taxlu$FAMILY,
                           .taxlu$Order[match(dataset$FAMILY,
                                             .taxlu$FAMILY)],
                           dataset$ORDER)

    dataset = dataset %>%
      mutate(CLASS = ifelse(FAMILY == toupper("Aeolosomatidae"),
                            "",
                            CLASS),
             FAMILY = ifelse(FAMILY  %in% toupper(c("Naididae", "Tubificidae")),
                             toupper("Tubificidae"),
                             ifelse(FAMILY  %in% toupper(c("Pilidae", "Ampullariidae")),
                                    toupper("Ampullariidae"),
                                    FAMILY)))

    taxonLevel.nrsa <- base::toupper(taxonLevel)

    if(taxonLevel %in% c("Genus","Mixed")){

      ##Convert those genera that need to be updated
      dataset$GENUS <- ifelse(dataset$GENUS %in% .switch1to1$BenchGenus,
                              .switch1to1$Genus[match(dataset$GENUS,
                                                      .switch1to1$BenchGenus)],
                              dataset$GENUS)

      #If genera that are one of genera in dat1, rename the Genus with the slash
      #label from dat1, else, keep the original Genus label
      dataset$GENUS <- ifelse(dataset$GENUS %in% dat1F$Genus,
                              dat1F$Slash[match(dataset$GENUS,
                                               dat1F$Genus)],
                              dataset$GENUS)

      #If genera that are one of problem slash genera, rename the Genus with the lumped
      #label from fix_slash, else, keep the original Genus label
      dataset$GENUS <- ifelse(dataset$GENUS %in% fix_slash$Slash,
                              fix_slash$Fix[match(dataset$GENUS,
                                                  fix_slash$Slash)],
                              dataset$GENUS)

    ##This is the same code as the NAWQA taxonomy fix
    if(TaxonFix == "none"){

    } else if(TaxonFix == "lump"){


      #If bench genera that are one of bench genera in clust_labels, rename the Genus with the lump label from clust_labels
      #else, keep the original Genus label
      dataset$GENUS <- ifelse(dataset$GENUS %in% slashlump$genus,
                              slashlump$lump[match(dataset$GENUS,
                                                   slashlump$genus)],
                              dataset$GENUS)

      #If bench genera that are one of bench genera in clust_labels, rename the Genus with the lump label from clust_labels
      #else, keep the original Genus label
      dataset$GENUS <- ifelse(dataset$GENUS %in% .clust_labels$genus,
                              .clust_labels$lump[match(dataset$GENUS,
                                                       .clust_labels$genus)],
                              dataset$GENUS)


    }else if(TaxonFix == "remove"){

      #filter out rows that have bench genus from problem list & no species ID
      dataset <- dataset %>%
        dplyr::filter(!(GENUS %in% .clust_labels$genus)) %>%
        dplyr::filter(!(grepl("/", GENUS)))
    }}

  else if(taxonLevel != "Genus" | taxonLevel != "Mixed"){
    dataset <- dataset %>%
      dplyr::mutate(across(tidyselect::all_of(taxonLevel.nrsa), ~ stringr::str_to_sentence(.)))
  }

    dataset$MIXED = ifelse(dataset$GENUS != "",
                           dataset$GENUS,
                           ifelse(dataset$SUBFAMILY != "",
                                  stringr::str_to_sentence(dataset$SUBFAMILY),
                                  ifelse(dataset$FAMILY != "",
                                         stringr::str_to_sentence(dataset$FAMILY),
                                         ifelse(dataset$ORDER != "",
                                                stringr::str_to_sentence(dataset$ORDER),
                                                ifelse(dataset$CLASS != "",
                                                       stringr::str_to_sentence(dataset$CLASS),
                                                       stringr::str_to_sentence(dataset$PHYLUM))))))

    if(taxonLevel == "Subfamily"){
      dataset$SUBFAMILY = ifelse(dataset$SUBFAMILY == "",
                                 stringr::str_to_sentence(dataset$FAMILY),
                                 dataset$SUBFAMILY)
    }
    }

return(dataset)
}

##fish taxonomy fix function
fishTaxFix <- function(dataset,
                       agency){

  if(agency == "USGS"){
    dataset$Species = ifelse(dataset$Species %in% .fishtaxlu$Old,
                                .fishtaxlu$New[match(dataset$Species,
                                                     .fishtaxlu$Old)],
                             dataset$Species)

    dataset$Genus = ifelse(dataset$Species %in% .fishtaxlu$New,
                             .fishtaxlu$NewGenus[match(dataset$Species,
                                                  .fishtaxlu$New)],
                             dataset$Genus)


    return(dataset)
  }
  if(agency == "EPA"){
    dataset$SPECIES = ifelse(grepl("Pomoxis sp", dataset$SCIENTIFIC),
                             "",
                             dataset$SPECIES)

    dataset$SCIENTIFIC = ifelse(dataset$SCIENTIFIC %in% .fishtaxlu$Old,
                                .fishtaxlu$New[match(dataset$SCIENTIFIC,
                                                     .fishtaxlu$Old)],
                                ifelse(grepl("Oncorhynchus clarki", dataset$SCIENTIFIC),
                                       "Oncorhynchus clarkii",
                                       ifelse(grepl("Esox americanus", dataset$SCIENTIFIC),
                                              "Esox americanus",
                                              ifelse(grepl("broadband sculpin", dataset$SCIENTIFIC),
                                                     "",
                                                     ifelse(grepl(" or ", dataset$SCIENTIFIC),
                                                            stringr::str_replace(dataset$SCIENTIFIC, " or ", " x "),
                                                            ifelse(grepl("Pomoxis sp", dataset$SCIENTIFIC),
                                                                   "Pomoxis ",
                                                            dataset$SCIENTIFIC))))))

    dataset$GENUS = ifelse(dataset$SCIENTIFIC %in% .fishtaxlu$New,
                           .fishtaxlu$NewGenus[match(dataset$SCIENTIFIC,
                                                     .fishtaxlu$New)],
                           dataset$GENUS)

    return(dataset)


  }


}


##fish standardization code
fishStandardization <- function(dataset,
                                dataType,
                                standardize,
                                agency){
  if(agency == "USGS") {
    if(dataType == "abun"){

      # calculate abundance matrices via raw abundance ("none"), CPUE standardization, or MGMS standardization
      if(standardize == "none"){
        fish_comm2 = dataset %>%
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

      if (standardize == "CPUE"){
        fish_comm2 <- dataset %>%
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
        fish_comm2 <- dataset %>%
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
      fish_comm2 = dataset %>%
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
        dplyr::select(-SecondsShockTime) %>%
        dplyr::relocate(SIDNO, tidyselect::any_of(.ReorderUSGSBioDataColNames))
    }

    return(fish_comm2)

  } else
    if(agency == "EPA"){
      if(dataType == "abun"){
        if(standardize == "none"){
          NRSA_FISH_comm <- dataset
        }
        if(standardize %in% c("CPUE", "MGMS")){
          NRSA_FISH_comm2 <- dataset %>%
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
      } else if(dataType == "occur"){
        NRSA_FISH_comm <- dataset
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
        rename(SampleID = UID,
               SiteNumber = SITE_ID,
               SiteName = SITENAME,
               CollectionDate = DATE_COL,
               Latitude_dd = LAT_DD83,
               Longitude_dd = LON_DD83,
               MethodBasic = MethodBasic,
               SiteVisitSampleNumber = VISIT_NO,
               ReachLengthFished_m = RCH_LENGTH) %>%
        dplyr::select(-STATE, -ELECTROFISH, -METHOD,-FISHED)

      return(NRSA_FISH_comm)
    }
}


