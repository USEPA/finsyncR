#' Access and harmonize macroinvertebrate data
#'
#' @description
#' This function generates an occurrence or abundance community matrix for
#' benthic macroinvertebrates sampled in rivers and streams from the US EPA
#' National Rivers and Streams Assessment and USGS BioData.
#'
#' @param dataType Output data type for the community matrix, either \code{"density"}
#'  (density) or \code{"occur"} (occurrence).
#' @param taxonLevel Level of taxonomic resolution for the community matrix. Input must be one of:
#'   \code{"Phylum"}, \code{"Class"}, \code{"Order"},
#'   \code{"Family"}, \code{"Subfamily"}, \code{"Genus"}, or \code{"Mixed"}.
#' @param taxonFix Option to account for changes in taxonomy across time, must be
#'   one of: \code{"none"}, \code{"lump"}, \code{"remove"}. See \code{Details}
#'   below for more information.
#' @param agency The agency name or names (e.g., "USGS" and "EPA") that are the
#'   source of data for the output community matrix. See \code{Details} below for more information.
#' @param lifestage logical. For USGS data only, should the output dataset include
#'   lifestage information for each taxa?  \code{TRUE} or \code{FALSE}. Default
#'   is \code{FALSE}.
#' @param rarefy logical. Should samples be standardized by the number of individuals
#'   identified within each sample? \code{TRUE} or \code{FALSE}. See \code{Details}
#'   below for more information.
#' @param rarefyCount integer. If \code{rarefy = TRUE}, the individual count to be used
#'   as the cutoff for rarefaction (standardizing samples by the number of individuals
#'   identified). Default is 300.
#' @param seed numeric. Set seed for \code{rarefy} to get consistent results with each new
#'   iteration of the function. Value gets passed to \code{set.seed} internally.
#' @param sharedTaxa logical. Should Genera be limited to those that appear in
#'   both the EPA and USGS datasets? \code{TRUE} or \code{FALSE}. Must be set to
#'   \code{FALSE} when only one agency is specified. Default is \code{FALSE}
#' @param boatableStreams logical. Should EPA boatable streams be included in the
#'   output dataset?  \code{TRUE} or \code{FALSE}. Note: all USGS streams are wadable.
#'   It is not advisable to include boatable streams when building a dataset including
#'   both EPA and USGS data. Boatable EPA data and wadeable USGS data are not
#'   considered comparable.
#'
#' @return A taxa by sample data frame with site, stream reach, and sample information.
#'
#' @details
#'   \code{agency} refers to the \code{agency} that collected the invertebrate samples.
#'   If you want to use data from both agencies, set \code{agency} = c("USGS", "EPA"),
#'   which is the default. For USGS data, sampling data include all USGS BioData
#'   with `SampleMethodCode` of "BERW", "IRTH", "SWAMP", "EMAP", "CDPHE RR", and "PNAMP".
#'   Note that by default, only moving waters classified as "wadeable" are
#'   included but setting \code{boatableStreams = TRUE} will include observations from
#'   non-wadeable streams. Some information included in the EPA dataset are not
#'   included in the USGS datasets, specifically observed wetted width of the
#'   stream/river.
#'
#'   \code{taxonLevel} refers to the taxonomic resolution (Genus, Class, Family, etc.)
#'   for the sample by taxa matrix. The input values for this parameter are case
#'   sensitive and must start with a capital letter. All observations taxonomically
#'   coarser than the `taxonLevel` provided are dropped from the output community matrix.
#'   For instance, if `taxonLevel = "Genus"` , then observations identified at
#'   Subfamily, Family, Order, Class, or Phylum levels are dropped. When
#'   `taxonLevel = "Subfamily"`, for taxa without subfamilies, the Family-level
#'   designation is returned. When `taxonLevel = "Subfamily"`, for taxa without subfamilies, the Family-level
#'   designation is returned. "Genus" is the finest level of taxonomic resolution
#'   provided for macroinvertebrates. The function also provides the option of returning
#'   the lowest level of taxonomic identification for all specimen (`taxonLevel = "Mixed"`).
#'
#'   \code{taxonFix} provides options to account for changes in taxonomy across time,
#'   especially in instances in which species have been reorganized into new genera.
#'   \code{taxonFix} operates on the genera level. \code{taxonFix = "none"}
#'   makes no adjustment. \code{taxonFix = "lump"} prioritizes retaining observations
#'   by giving a unified "slash" genera name to all species and genera that have been linked
#'   through changes in taxonomy through time (e.g. genera1/genera2/genera3). Note:
#'   of 82 problematic genera that exist throughout both datasets,
#'   \code{taxonFix = "lump"} results in 11 "lumped" genera. All but two complexes
#'   of genera were composed of two individual genera. A single complex of genera
#'   within the Ephemeroptera order included 70 individual genera, and a second
#'   complex of Ephemeroptera genera included six individual genera. Because of
#'   the complexity of Ephemeroptera taxonomy, careful consideration should be
#'   given to inferences that can be made when evaluating Ephemeroptera trends.
#'   The authors, for instance, do not advise that users evaluate temporal changes
#'   in abundance or richness of the linked groups of Ephemeroptera genera because
#'   taxonomic reorganizations likely obscure temporal patterns.
#'
#'   Alternatively, genera linked by taxonomic reorganization can be removed with
#'   \code{taxonFix = "remove"}. This option prioritizes accurate identification by
#'   dropping observations that cannot be confidently identified to a single genus,
#'   as in the complexes of genera previously described.  Without a species-level
#'   identification, there is no way to assure correct membership in an updated genus.
#'   Organisms with a species-level identification are cross-walked to an updated genus.
#'   NOTE on “slash” genera: When \code{taxonFix = "lump"}, these "slash"
#'   genera are rolled into the larger linked genera, as above. \code{taxonFix = "remove"}
#'   prioritizes accurate identifications by dropping all slash genera are, including
#'   those organisms identified as a “slash” genus in the lab; this option will
#'   result in many fewer genera in the final dataset. Finally, \code{taxonFix = "none"},
#'   includes "slash" genera, but it does not connect these genera to larger linked genera.
#'
#'   \code{taxonFix} operates on the genus level, so set \code{taxonFix = "none"}
#'   when taxonLevel is set to \code{"Family"} or higher taxonomic resolution.
#'   Care should be taken to harmonize taxonomy either with the approaches provided
#'   or some alternative when long time scale datasets on the entire community
#'   of macroinvertebrates are generated because changes in taxonomy can make it
#'   artificially appear as though some genera are either appearing or disappearing
#'   in time. See \code{vignette("GettingStarted")} for more information.
#'
#'   If \code{rarefy = TRUE}, samples with \code{rarefyCount}+ individuals
#'   identified (raw count) are retained. Thus, a percentage of samples will be
#'   removed, as they have <\code{rarefyCount} individuals sampled. The rarefaction
#'   threshold is default is 300 organisms, because 1) with every 50 individuals
#'   identified, ~1 genera are added to the sample and 2) 91.3 \% of samples have
#'   at least 300 individuals identified. Thus, lowering the threshold to 200
#'   individuals removed ~2 genera per sample, but only an additional 3.2 \% of
#'   samples are included (94.5 \% from 91.3 \%). Similarly, increasing the threshold
#'   to 400 individuals added ~2 genera per sample, but reduced samples to 70.6 \% of
#'   all samples. Use \code{seed = ...}  to get consistent output of community data.
#'   See \code{vignette("GettingStarted")} for more information regarding
#'   rarefaction. NOTE: \code{rarefy = TRUE} can be used when a user wants
#'   occurrence data (presence/absence) OR proportional data (each taxon represents
#'   a certain proportion of a sample). Use \code{rarefy = FALSE} when densities
#'   are the measure of interest.
#'
#'   When dataType = "density", the function calculates taxa densities from samples
#'   using lab subsampling ratios and area sampled
#'   \deqn{Taxa~abundance = n*frac{1}{PropID}}
#'   \deqn{Taxa~density = frac{Taxa~abundance}{Area~sampled~(m^2)}}
#'   where *n* is the number of specimens identified and *PropID* is the
#'   proportion of the sample that was identified at the lab bench. For
#'   the USGS dataset, this incorporates both "field split ratio" (proportion of
#'   the sample that was brought into the lab for specimen identification) and
#'   the "lab subsampling ratio" (proportion of grids used to identify invertebrates
#'   at the lab bench). For the EPA datasets, this is just the "lab subsampling ratio", the
#'   proportion of grids used to identify invertebrates at the lab bench. See
#'   \code{vignette("GettingStarted")} for more details on the calculation of
#'   taxa densities.
#'
#' @author Michael Mahon, Devin Jones, Samantha Rumschlag, Terry Brown
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
                          rarefyCount = 300,
                          sharedTaxa = FALSE,
                          seed = 0,
                          boatableStreams = FALSE){

  if(!(dataType %in% c("density", "occur"))) {
    stop('dataType must be either "density" or "occur".')}

  if(!(taxonLevel %in% c("Phylum","Order","Class","Family","Subfamily","Genus", "Mixed"))){
    stop(paste('taxonLevel must be set between ranks "Phylum" and "Genus";',
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

  if(rarefy == TRUE && dataType == "density"){
    stop('rarefy must be set to FALSE when requesting abundance data')
  }

  if(rarefy == FALSE && dataType == "occur"){
    warning('rarefy should be set to TRUE when requesting occurrence data')
  }

  if(isTRUE(sharedTaxa) && (all(grepl("USGS", agency)) | all(grepl("EPA", agency))) ){
    stop('sharedTaxa must be set to FALSE when agency is set to "USGS" or "EPA" alone')
  }

  if(boatableStreams == TRUE && any(grepl("USGS", agency))){
    warning('boatableStreams should be set to FALSE when requesting USGS data')
  }

  if(any(grepl("USGS", agency))){
    cat(" finsyncR is running:  Gathering and cleaning USGS raw data                    ")
    TotalRows = acquireData(taxa = "inverts",
                           agency = "USGS",
                           waterbody = "streams")

    ##These are the column names that should be removed (mycols) and which rows
    ##of data should be retained based on taxonomic resolution
    ##(eg if taxonLevel == "Family", retain ALL taxonomic levels at Family and Below)

    if(taxonLevel != "Mixed"){
        mycols = c(.TaxLevCols_Inverts[[which(names(.TaxLevCols_Inverts) == taxonLevel)]]$mycols,"Mixed")
        taxcols = c(.TaxLevCols_Inverts[[which(names(.TaxLevCols_Inverts) == taxonLevel)]]$taxcols,"Mixed")
    } else {
      taxcols = .TaxLevCols_Inverts[[which(names(.TaxLevCols_Inverts) == "Phylum")]]$taxcols
      mycols = c("Phylum",.TaxLevCols_Inverts[[which(names(.TaxLevCols_Inverts) == "Phylum")]]$mycols)
    }


    ##Before any final data manipulation, if dataset is occurence and rarify is true
    ##then rarify based on the RAWCOUNT (individuals actually identified)
    if(dataType == "occur"){
      if(isTRUE(rarefy)) {
        cat('\r',"finsyncR is running: Rarefying USGS data                                      ")
        set.seed(seed)
        TotalRows = TotalRows %>%
          dplyr::group_by(SIDNO) %>%
          dplyr::mutate(indcounted = sum(RawCount)) %>%
          dplyr::filter(indcounted >= rarefyCount) %>%
          dplyr::select(-indcounted) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(SIDNO, PublishedTaxonName) %>%
          dplyr::slice(rep(1:dplyr::n(), times=RawCount)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(SIDNO) %>%
          dplyr::sample_n(size = rarefyCount) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(SIDNO, PublishedTaxonName) %>%
          dplyr::mutate(RawCount = n()) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup()
      } else {}
    }   else {}

    cat('\r',"finsyncR is running: Applying taxonomy fixes to USGS data                    ")
    TotalRows = invertTaxFix(TotalRows,
                             taxonFix,
                             agency = "USGS",
                             taxonLevel)

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
        dplyr::select(-tidyselect::any_of(mycols)) %>%
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
    if(dataType == "density"){
      invert_comms1 = invert_comms1 %>%
        dplyr::filter(!is.na(AreaSampTot_m2)) %>%
        dplyr::filter(AreaSampTot_m2 != 0)
    }

    ##site x species matrix, select only those columns that we need
    invert_comms1 = invert_comms1 %>%
      dplyr::select(-Identifier,
                    -ReleaseCategory) %>%
      dplyr::relocate(tidyselect::any_of(c("SIDNO",.ReorderUSGSBioDataColNames[-26],
                                           "FieldSplitRatio", "LabSubsamplingRatio",
                                           "PropID", "AreaSampTot_m2"))) %>%
      dplyr::mutate(SiteNumber = paste("USGS-", SiteNumber, sep = ""))

    invert_comms1 <- invert_comms1  %>%
      mutate(WettedWidth = NA,
             Agency = "USGS") %>%
      select(Agency, SIDNO, tidyselect::any_of(.InvertIDCols), tidyselect::contains("tax_"))
  }

  if(any(grepl("EPA", agency))){
    cat('\r',"finsyncR is running: Gathering, joining, and cleaning EPA raw data                    ")
    NRSA_inverts = acquireData(taxa = "inverts",
                              agency = "EPA",
                              waterbody = "streams")

    ##First step after data has been gathered:
    ##Filter SAMPLE_TYPE to "BERW", "BERWW", or "REACHWIDE" in NRSA_inverts if not boatable
    sampletype = c("BERW", "BERWW", "REACHWIDE", "BELGB", "BETB", "BOATABLE")

    if(!isTRUE(boatableStreams)){
      sampletype = c("BERW", "BERWW", "REACHWIDE")
    }

    NRSA_inverts <- NRSA_inverts %>%
      filter(SampleTypeCode %in% sampletype)

    ##Second step:
    ##Rarefy samples to 300 in the same manner as the NAQWA data for consistency
    if(isTRUE(rarefy)) {
      cat('\r',"finsyncR is running: Rarefying EPA data                              ")
      set.seed(seed)
      NRSA_inverts <- NRSA_inverts %>%
        ##Create unique grouping based on UID, SITE_ID, YEAR, and VISIT_NO
        ##Group by this column
        ##Take the total individuals counted, remove those that are less than 300
        tidyr::unite(Unique, c(ProjectAssignedSampleLabel, SiteNumber,
                               CollectionYear, SiteVisitSampleNumber),
                     sep = "_", remove = F) %>%
        dplyr::group_by(Unique) %>%
        dplyr::mutate(indcounted = sum(TOTAL)) %>%
        dplyr::filter(indcounted >= rarefyCount) %>%
        dplyr::select(-indcounted) %>%
        dplyr::ungroup() %>%
        ##Again group by the unique sample column
        ##Replicate each unique sample and target taxon by the number of individuals
        ## found in the sample, then take 300 random individuals from these samples
        dplyr::group_by(Unique, TARGET_TAXON) %>%
        dplyr::slice(rep(1:dplyr::n(), times=TOTAL)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Unique) %>%
        dplyr::sample_n(size = rarefyCount) %>%
        dplyr::group_by(Unique, TARGET_TAXON) %>%
        dplyr::mutate(TOTAL = dplyr::n()) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-Unique)
    } else {}

    ##Incorporate abundance conversions
    if(dataType == "density"){

      ##Join the datasets together; convert TOTAL to density, using the
      ##DenAbunRatio; multiple this by 10.76 to convert from ind ft^-2 to ind m^-2
      ##Use CONVS to convert transect number to ft^2
      ##Remove the DenAbunRatio from the final dataset; and output
      NRSA_inverts <- NRSA_inverts %>%
        dplyr::mutate(TOTAL = round(((TOTAL / PropID) / AreaSampTot_m2) , 4))
    }

    ##Third step:
    ##FIX ALL TAXONOMIC ISSUES
    cat('\r',"finsyncR is running: Applying taxonomic fixes to EPA data                    ")
      NRSA_inverts = invertTaxFix(NRSA_inverts,
                                  TaxonFix = taxonFix,
                                  agency = "EPA",
                                  taxonLevel = taxonLevel)

      taxonLevel.nrsa <- base::toupper(taxonLevel)

    if(isTRUE(sharedTaxa) & any(grepl("EPA", agency)) & any(grepl("USGS", agency))){
      cat('\r',"finsyncR is running: Removing taxa not in both USGS and EPA datasets                 ")
      ##List of NAWQA taxa
      USGStaxa <- c(unique(TotalRows[,taxonLevel]))[[taxonLevel]]

      ##List of NRSA taxa (have to do this rarfy thing to fix some weird issue)
      if(isTRUE(rarefy)){
        NRSAtaxa <- c(unique(NRSA_inverts[,taxonLevel.nrsa]))[[taxonLevel.nrsa]]
      } else{ NRSAtaxa <- unique(NRSA_inverts[,taxonLevel.nrsa])}

      ##Filter NRSA to only those genera in NAWQA
      NRSA_inverts <- NRSA_inverts %>%
        filter(.[[taxonLevel.nrsa]] %in% USGStaxa)

      ##Select only those taxa that appear in USGS
      ##add "tax_" prefix to the names, as this is how the genera names appear
      ##as columns in the NAWQA dataset
      USGStaxaONLY <- paste("tax_",
                             USGStaxa[!(USGStaxa %in% NRSAtaxa)],
                               sep = "")

      ##Filter NAWQA to only those genera in NRSA (-select [delete] any that
      ##appear in columns in the invert_comms1 dataset)
      invert_comms1 <- invert_comms1 %>%
        dplyr::select(-tidyselect::any_of(USGStaxaONLY))

    } else {}

    ##Fourth step:
    ## NOTE: this step is only needed when looking at taxonomic resolutions ABOVE genus
    ##Join all Target_taxon within each UID (sample)

    ##UPDATE THIS FOR NRSA_MYCOLS
    mycols = c("TARGET_TAXON",
               "PHYLUM",
               "CLASS",
               "ORDER",
               "FAMILY",
               "SUBFAMILY",
               "GENUS",
               "MIXED")

    mycols <- mycols[!(mycols %in% (taxonLevel.nrsa))]

    nrsa_comms1 = NRSA_inverts %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel.nrsa)), dplyr::any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(ProjectAssignedSampleLabel, SiteNumber,
                               CollectionYear, SiteVisitSampleNumber, all_of(taxonLevel.nrsa)),
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
                         values_fill = 0) %>%
      dplyr::relocate(tidyselect::contains("tax_"), .after = last_col())

    ##remove the UNIQUEID column, as it is no longer needed
    nrsa_comms1 <- nrsa_comms1 %>%
      mutate(Agency = "EPA") %>%
      rename("SIDNO" = "ProjectAssignedSampleLabel") %>%
      select(Agency, SIDNO, tidyselect::any_of(.InvertIDCols), tidyselect::contains("tax_"))

  }

  if(all(grepl("EPA", agency))) {

    invert_comms1 <- nrsa_comms1

  } else if(any(grepl("EPA", agency)) & any(grepl("USGS", agency))) {
    cat("\r","finsyncR is running: Harmonizing USGS and EPA data                                     ")
    invert_comms1 <- dplyr::bind_rows(invert_comms1, nrsa_comms1)

  } else {}
  cat("\r","finsyncR is running: Finalizing data for output                          ")
  invert_comms1 = invert_comms1 %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("tax_"),
                                ~ifelse(is.na(.x),
                                        0,
                                        .x)))

  invert_comms1 <- invert_comms1 %>%
    left_join(.allsitesCOMID %>% filter(SiteNumber %in% invert_comms1$SiteNumber), by = dplyr::join_by(SiteNumber))

  invert_comms1 <- invert_comms1 %>%
    left_join(.specIDgen %>% group_by(SiteNumber, CollectionYear, CollectionDayOfYear) %>% slice(1) %>% ungroup() %>% filter(SiteNumber %in% invert_comms1$SiteNumber) %>% dplyr::select(SiteNumber, CollectionYear, CollectionDayOfYear, Gen_ID_Prop), by = dplyr::join_by(SiteNumber == SiteNumber,
                                              CollectionYear == CollectionYear,
                                              CollectionDayOfYear == CollectionDayOfYear))

  invert_comms1 <- invert_comms1  %>%
    dplyr::rename("SampleID" = "SIDNO") %>%
    dplyr::select(any_of(.finalcovarorder), tidyselect::contains("tax_")) %>%
    dplyr::relocate(tidyselect::contains("tax_"), .after = last_col())

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
  cat("\r","finsyncR data synchronization complete                          \n")

  return(data.frame(invert_comms1))

}
