#' Access clean USGS Algae Dataset
#'
#' @param algType Algae sample type, either \code{"peri"} or \code{"phyto"}.
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"AlgalGroup"}, \code{"Phylum"}, \code{"Class"}, \code{"Order"},
#'   \code{"Family"}, \code{"Genus"}, or \code{"Species"}.
#' @param program The program name(s) that should be included in the output
#'   dataset.
#'
#' @return A species by sample data frame with site, stream reach, and
#'   sample information.
#'
#' @details \code{program} refers to the Local, regional, or national program project
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
#' @examples
#' \dontrun{
#' Periphyton <- getFishData(algType = "peri",
#'                           taxonLevel = "Family")
#'
#' Phytoplankton <- getFishData(algType = "phyto",
#'                              taxonLevel = "Family")
#' }
#'
#' @export


getAlgaeData <- function(algType = "peri",
                         dataType = "abun",
                         taxonLevel = "Species",
                         program = "National Water Quality Assessment"){

  if(!(algType %in% c("peri", "phyto"))) {
    stop('algType must be either "peri" or "phyto".')}

  if(!(dataType %in% c("abun", "occur"))) {
    stop('dataType must be either "abun" or "occur".')}

  if(!(taxonLevel %in% .TaxLevCols_Algae$AlgalGroup$taxcols)){
    stop(paste("taxonLevel must be set between ranks 'Phylum' and 'Subspecies'",
               "or 'AlgalGroup'; see 'Details' in ?getAlgaeData."))
  }
  algae = utils::read.csv(unzip(system.file("extdata",
                                            "AlgaeResults.zip",
                                            package = "StreamData")),
                   colClasses = c("SiteNumber" = "character"))
  if(colnames(algae)[1] != "SIDNO"){
    colnames(algae)[1] = "SIDNO"
  }

  Project <- utils::read.csv(system.file("extdata",
                               "20201217.0757.Project.csv",
                               package = "StreamData"))

  if(program == "ALL") {
    database <- c("National Water Quality Assessment",
                  "Cooperative Water Program",
                  "Collection of Basic Records",
                  "Other Federal Agencies")
  } else {database <- program }

  algae <- algae %>%
    dplyr::filter(ProjectLabel %in% (Project %>%
                                       dplyr::filter(Program %in% database) %>%
                                       dplyr::distinct(ProjectLabel,
                                                       .keep_all = FALSE))[ , "ProjectLabel"]) %>%
    dplyr::select(-LabRecordID, -LabOrderID, -LabSampleID, -FieldComponent,
                  -ReleaseCategory, -SiteVisitSampleNumber,
                  -StudyReachName,
                  -PublishedSortOrder, -BioDataTaxonName, -BioDataShortName,
                  -BenchTaxonName, -BenchTaxonNameReferenceCode, -BenchTaxonNADEDID,
                  -TaxonRecordSource, -IdentificationEntity, -IdentificationDate,
                  -IdentifiedBy, -VerificationEntity, -VerificationDate,
                  -CurationEntity, -CurationDate, -LabTaxonID,
                  -PublishedTaxonNameAuthority, -TaxonVersionNumber)


  algsamp = utils::read.csv(system.file("extdata",
                                 "20201217.0757.AlgSamp.csv",
                                 package = "StreamData"),
                     colClasses = c("SiteNumber" = "character"))
  if(colnames(algsamp)[1] != "SIDNO"){
    colnames(algsamp)[1] = "SIDNO"
  }
  algsamp = algsamp %>%
    dplyr::select(SIDNO,
                  SiteNumber,
                  PeriphytonHabitatSampled,
                  SamplingMethodOrDevice,
                  SubsurfaceGrabDepth_m,
                  StudyReachName)

  algsampinv = utils::read.csv(system.file("extdata",
                                    "20201217.0757.SampleInv.csv",
                                    package = "StreamData"),
                        colClasses = c("SiteNumber" = "character"))
  if(colnames(algsampinv)[1] != "SIDNO"){
    colnames(algsampinv)[1] = "SIDNO"
  }
  algsampinv = algsampinv %>%
    dplyr::select(SIDNO,
                  ReplicateType)

  algsite = utils::read.csv(system.file("extdata",
                                 "20201217.0757.SiteInfo.csv",
                                 package = "StreamData"),
                     colClasses = c("SiteNumber" = "character"))
  algsite = algsite %>%
    dplyr::select(SiteNumber,
                  Latitude_dd,
                  Longitude_dd,
                  CoordinateDatum,
                  HUCCode,
                  DrainageArea_mi2,
                  SiteTypeName,
                  CountyFIPSCode,
                  StateFIPSCode)

  algsampinfo = dplyr::left_join(dplyr::left_join(algsamp,
                                    algsampinv,
                                    by = "SIDNO"),
                          algsite,
                          by = "SiteNumber") %>%
    dplyr::select(-SiteNumber) %>%
    dplyr::mutate(CountyFIPSCode = sprintf("%03d", CountyFIPSCode),
           StateFIPSCode = sprintf("%02d", StateFIPSCode))

  algae = dplyr::left_join(algae, algsampinfo, by = "SIDNO")
    mutate(CountyFIPSCode = sprintf("%03d", CountyFIPSCode),
           StateFIPSCode = sprintf("%02d", StateFIPSCode))


  mycols = .TaxLevCols_Algae[[which(names(.TaxLevCols_Algae) == taxonLevel)]]$mycols
  taxcols = .TaxLevCols_Algae[[which(names(.TaxLevCols_Algae) == taxonLevel)]]$taxcols


  if(algType == "peri"){
    if(dataType == "abun"){
      perisamp = c("ARTH", "ADTH")
    } else {perisamp = c("ARTH", "ADTH", "AQMH")}

  } else if(algType == "phyto") {
    perisamp = c("APHY")
  }


  if(algType == "peri"){
  algae_comms1 = algae %>%
    dplyr::filter(SampleTypeCode %in% perisamp) %>%
    dplyr::filter(PublishedTaxonNameLevel %in% taxcols) %>%
    dplyr::filter(!(is.na(Cells_cm2))) %>%
    dplyr::select(-Cells_mL, -Biovolume_um3_cm2, -Biovolume_um3_mL,
                  -NumberOfCellsCounted, -NumDiaCellsCounted_LiveOrDead,
                  -ProportionExamined, -NormalizedCellCount, -AdjLabCount,
                  -TotAreaSampled_cm2, -BiovolumePerCell_um3,
                  -BiovolumePerCellMethod, -BenchNotes, -PublishedTaxonNameLevel,
                  -ScientificName, -Group, -Variety, -Form,
                  -TaxonomicResultReviewStatus, -PublishedTaxonName,
                  -SubsurfaceGrabDepth_m) %>%
    dplyr::select(-tidyselect::any_of(mycols)) %>%
    tidyr::unite(UNIQUE,
          c(SIDNO, SampleTypeCode,
            tidyselect::all_of(taxonLevel), PeriphytonHabitatSampled),
          sep = "_", remove = FALSE) %>%
    dplyr::group_by(UNIQUE) %>%
    dplyr::mutate(sumCells_cm2 = sum(Cells_cm2)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(UNIQUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::unite(UNIQUE2,
          c(SiteNumber, StudyReachName, CollectionDate,
            SampleTypeCode, tidyselect::all_of(taxonLevel)),
          sep = "_", remove = FALSE) %>%
    dplyr::group_by(UNIQUE2) %>%
    dplyr::mutate(MsumCells_cm2 = mean(sumCells_cm2)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(UNIQUE2) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::unite(SampleGrouping, c(StudyReachName, CollectionDate,
                            SampleTypeCode),
          sep = "_", remove = FALSE) %>%
    dplyr::select(-SIDNO, -UNIQUE, -UNIQUE2, -Cells_cm2, -sumCells_cm2,
                  -ReplicateType) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(taxonLevel),
                values_from = MsumCells_cm2,
                names_prefix = "tax_",
                values_fill = 0,
                names_sort = TRUE)

  } else {

    algae_comms1 = algae %>%
      dplyr::filter(SampleTypeCode %in% perisamp) %>%
      dplyr::filter(PublishedTaxonNameLevel %in% taxcols)%>%
      dplyr::filter(!(is.na(Cells_mL))) %>%
      dplyr::select(-Cells_cm2, -Biovolume_um3_cm2, -Biovolume_um3_mL,
                    -NumberOfCellsCounted, -NumDiaCellsCounted_LiveOrDead,
                    -ProportionExamined, -NormalizedCellCount, -AdjLabCount,
                    -TotAreaSampled_cm2, -BiovolumePerCell_um3,
                    -BiovolumePerCellMethod, -BenchNotes, -PublishedTaxonNameLevel,
                    -ScientificName, -Group, -Variety, -Form,
                    -TaxonomicResultReviewStatus, -PublishedTaxonName,
                    -PeriphytonHabitatSampled, -Cells_cm2) %>%
      dplyr::select(-tidyselect::any_of(mycols)) %>%
      tidyr::unite(UNIQUE, c(SIDNO, tidyselect::all_of(taxonLevel)), remove = FALSE) %>%
      dplyr::group_by(UNIQUE) %>%
      dplyr::mutate(sumCells_mL = sum(Cells_mL)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(UNIQUE) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      tidyr::unite(UNIQUE2, c(StudyReachName, CollectionDate, all_of(taxonLevel)), sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUE2) %>%
      dplyr::mutate(MsumCells_mL = mean(sumCells_mL)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(UNIQUE2) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      tidyr::unite(SampleGrouping, c(StudyReachName, CollectionDate), sep = "_", remove = FALSE) %>%
      dplyr::select(-SIDNO, -UNIQUE, -UNIQUE2, -Cells_mL, -sumCells_mL,
                    -ReplicateType) %>%

      tidyr::pivot_wider(names_from = tidyselect::all_of(taxonLevel),
                  names_prefix = "tax_",
                  values_from = MsumCells_mL,
                  values_fill = 0)

  }
  algae_comm <- rbind(algae_comms1 %>%
                        dplyr::group_by(SampleGrouping) %>%
                        dplyr::mutate(count = n()) %>%
                        dplyr::filter(count > 1) %>%
                        dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                                      mean)) %>%
                        dplyr::slice(1) %>%
                        dplyr::ungroup() %>%
                        dplyr::select(-count),
                      algae_comms1 %>%
                        dplyr::group_by(SampleGrouping) %>%
                        dplyr::mutate(count = n()) %>%
                        dplyr::filter(count == 1) %>%
                        dplyr::select(-count))

    if(dataType == "occur") {
  algae_comm %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("tax_"),
                  ~replace(., . > 0, 1)))
  }

  algae_comm <- algae_comm %>%
    dplyr::select(-LabProcName,
                  -SampleGrouping) %>%
    dplyr::relocate(tidyselect::any_of(.ReorderUSGSBioDataColNames))




  colnames(algae_comm) = sub("tax_", "", colnames(algae_comm))


  return(data.frame(algae_comm))
  }
