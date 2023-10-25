#' Access harmonized USGS and EPA Fish datasets
#'
#' @param dataType Output data type, either \code{"abun"} or \code{"occur"}.
#' @param taxonLevel Level of taxonomic resolution, must be one of:
#'   \code{"Family"}, \code{"Genus"}, or \code{"Species"}.
#' @param agency The agency name(s) (e.g., "USGS" and "EPA") that should be
#'   included in the output dataset. See \code{Details} below for more information.
#' @param standardize Standardization method to use for calculating fish abundance matrices.
#'   Default is \code{standardize = "none"}, which returns raw fish count values. Other options include
#'   \code{standardize = "CPUE"}, which returns standardized abundances in Catch per Unit Effort.
#'   An alternative standardization method is \code{standardize = "MGMS"}, which calculates
#'   Multigear Mean Standardization (MGMS) values to account for catchability differences between
#'   fish sampling methods (see 'details' and Gibson-Reinemer et al. (2014) for more info on MGMS).
#' @param hybrids logical. Should hybrid individuals be included in the output dataset?
#'   \code{TRUE} or \code{FALSE}.
#' @param sharedTaxa logical. Should Genera be limited to those that appear in
#'   both the EPA and USGS datasets? \code{TRUE} or \code{FALSE}. Must be set to
#'   \code{FALSE} when only one agency is specified.
#' @param boatableStreams logical. Should EPA boatable streams be included in the
#'   output dataset? \code{TRUE} or \code{FALSE}. Note: all USGS streams are wadable;
#'   so \code{boatableStreams} should only be set to \code{TRUE} when gathering
#'   EPA data only.
#'
#' @return A taxa by sample data frame with site, stream reach, and
#'   sample information.
#'
#' @details
#'   To standardize fish abundance data, abundances are divided by minutes
#'   shocked, number of seine hauls, etc. These values are then divided by the
#'   stream length sampled. ~35% of the data have been removed,
#'   as these samples lacked information on either stream length sampled or sampling
#'   effort. Therefore, if you are interested in occurrence (pres/abs) data
#'   only, then set \code{dataType = "occur" and standardize = "none"}. Thus,
#'   be aware that setting \code{dataType = "occur"} will result in a larger
#'   dataset with additional samples/sites than when \code{dataType = "abun"}.
#'
#'   To account for differences in efficacy between shocking, seine netting, and
#'   snorkeling, we included multigear mean standardization
#'   (\code{standardize = "MGMS"}) as a standardization method as an alternative
#'   to catch per unit effort (\code{standardize = "CPUE"}). To calculate MGMS,
#'   CPUE is calculated for each taxa within a sample. Then, mean total CPUE
#'   (summation of CPUEs for all taxa at each site for each collection date) is
#'   calculated for for each sampling method. Finally, each CPUE value is then
#'   divided by the mean total CPUE for their respective sampling methods.
#'   See the supplement of Gibson-Reinemer et al. (2014) for an example of MGMS
#'   computation.
#'
#'   Keep in mind that when setting \code{standardize = "CPUE"} will result in a
#'   larger dataset (more rows) than \code{standardize = "MGMS"} because CPUE data
#'   will have a row for each sampling method used at each time-location, whereas
#'   MGMS will have only one row for each time-location because the function is
#'   standardizing by sampling method in addition to standardizing by time and
#'   area sampled, such that MGMS values for each taxa will be summed for
#'   instances in which a site had different methods used to collect fish.
#'
#'   \code{agency} refers to the federal agency that collected the fish samples. If
#'   you want to use data from both agencies, set \code{agency = c("USGS", "EPA")}.
#'   For the "USGS" dataset, this includes all programs that collected fish data
#'   within the larger USGS database. For the "EPA"  dataset, samples from the
#'   National Stream and River Assessment programs (2018-2019, 2013-2014,
#'   2008-2009) will be included. Note that by default, only moving waters
#'   classified as "wadeable" are included, but setting \code{boatableStreams = TRUE}
#'   will include non-wadeable streams. Some information included in the USGS
#'   dataset are not included in the EPA datasets, and vice-versa, and thus
#'   will appear as "NA" values. \code{agency} can also be set to either
#'   "EPA" or "USGS" individually.
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
                        sharedTaxa = FALSE,
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

  if(!(taxonLevel %in% .TaxLevCols_Fish$Superclass$taxcols)){
    stop(paste('taxonLevel must be set between ranks "Superclass" and',
               '"Subspecies"; see "Details" in ?getFishData.'))
  }

  if(!taxonLevel %in% c("Family","Genus","Species")){
    stop(paste('taxonLevel must be set to Family, Genus, or Species'))
  }

  if(isTRUE(sharedTaxa) && (all(grepl("USGS", agency)) | all(grepl("EPA", agency))) ){
    stop('sharedTaxa can only be set to TRUE when agency is set to c("USGS", "EPA")')
  }

  if(any(grepl("USGS", agency))){
    cat(" Gathering and cleaning USGS raw data                    ")
    fish_info <- acquireData("fishes",
                             "USGS",
                             "streams")

    cat('\r',"Applying taxonomic fixes to USGS data                    ")
    fish_info <- fishTaxFix(fish_info,
                            "USGS")

    mycols = .TaxLevCols_Fish[[which(names(.TaxLevCols_Fish) == taxonLevel)]]$mycols
    taxcols = .TaxLevCols_Fish[[which(names(.TaxLevCols_Fish) == taxonLevel)]]$taxcols



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

    cat('\r',"Applying count standardization to USGS data                    ")
    fish_comm2 <- fishStandardization(fish_comm,
                                      dataType,
                                      standardize,
                                      "USGS")

  }

  if(any(grepl("EPA", agency))){
    cat('\r',"Gathering, joining, and cleaning EPA raw data                    ")
    NRSA_FISH_wSite <- acquireData("fishes",
                                   "EPA",
                                   "streams")

    cat('\r',"Applying taxonomic fixes to EPA data                    ")
    NRSA_FISH_wSite <- fishTaxFix(NRSA_FISH_wSite,
                                  "EPA")

    ##this is where we need to bring in different levels of the fish taxonomy
    ##When "taxonLevel" isn't in all caps (in the function), create a NRSA specific
    ##taxonLevel that is in all caps
    taxonLevel.nrsa <- base::toupper(taxonLevel)
    taxonLevel.nrsa = ifelse(taxonLevel.nrsa == "SPECIES",
                             "SCIENTIFIC",
                             taxonLevel.nrsa)
    ##UPDATE THIS FOR NRSA_MYCOLS
    mycols = c("FAMILY",
               "GENUS",
               "SPECIES",
               "SCIENTIFIC",
               "FINAL_NAME")

    mycols <- mycols[!(mycols %in% (taxonLevel.nrsa))]

    NRSA_FISH_comm <- NRSA_FISH_wSite %>%
      dplyr::filter(!is.na(GENUS) ) %>%
      dplyr::filter(!is.na(SPECIES) & SPECIES != "") %>%
      dplyr::filter(!grepl(" or ", SCIENTIFIC)) %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(taxonLevel.nrsa)), dplyr::any_vars(. != "")) %>%
      tidyr::unite(UNIQUEID, c(UID, SITE_ID, DATE_COL, VISIT_NO, all_of(taxonLevel.nrsa)),
                   sep = "_", remove = FALSE) %>%
      dplyr::group_by(UNIQUEID) %>%
      dplyr::mutate(TOTAL = sum(TOTAL)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-UNIQUEID) %>%
      dplyr::select(-tidyselect::any_of(mycols)) %>%
      pivot_wider(names_from = tidyselect::all_of(taxonLevel.nrsa),
                  names_prefix = "tax_",
                  values_from = TOTAL,
                  values_fn = sum,
                  values_fill = 0)
    ####Need to add if group here for just "abundance"

    cat('\r',"Applying count standardization to EPA data                    ")

    NRSA_FISH_comm2 <- fishStandardization(NRSA_FISH_comm,
                                      dataType,
                                      standardize,
                                      "EPA")
  }


  if(all(grepl("EPA", agency))) {

    full_fish <- NRSA_FISH_comm %>%
      mutate(Agency = "EPA") %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))

  } else if(all(grepl("USGS", agency))){
    full_fish = fish_comm2 %>%
      mutate(SiteNumber = paste("USGS-",SiteNumber,sep = ""),
             # StandardMethod = as.character(StandardMethod),
             WettedWidth = NA,
             Agency = "USGS")%>%
      relocate(Agency, .after = SiteNumber) %>%
      dplyr::select(-StateFIPSCode, -CountyFIPSCode)  %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))

  } else if(any(grepl("EPA", agency)) & any(grepl("USGS", agency))) {
    cat("\r","Syncying USGS and EPA data                                     ")
    full_fish <- bind_rows(fish_comm2 %>%
                             mutate(SiteNumber = paste("USGS-",SiteNumber,sep = ""),
                                    # StandardMethod = as.character(StandardMethod),
                                    WettedWidth = NA,
                                    Agency = "USGS") %>%
                             dplyr::select(-StateFIPSCode, -CountyFIPSCode, -MethodCode),
                           NRSA_FISH_comm2 %>% mutate(Agency = "EPA")) %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))
  } else {}


  if(dataType == "occur") {
    full_fish = full_fish %>%
      dplyr::mutate(dplyr::across(tidyselect::starts_with("tax_"),
                                  ~replace(., . > 0, 1)))
  }
  cat("\r","Finalizing data for output                          ")
  ##Remove those observations with 0s in their StandardMethod
  full_fish <- full_fish %>%
    filter(StandardMethod != 0) %>%
    dplyr::select(-tidyselect::any_of(c("NumberSeineHauls",
                                        "NumberStationarySetsKicks", "NumberSnorkelingTransects",
                                        "MinutesShockTime", "SecondsShockTime")))

  full_fish <- full_fish %>%
    ungroup() %>%
    left_join(.allsitesCOMID %>% filter(SiteNumber %in% full_fish$SiteNumber), by = dplyr::join_by(SiteNumber)) %>%
    mutate(`tax_No fish collected` = ifelse(`tax_No fish collected` == 0,
                                            "Fish Collected",
                                            "No Fish Collected")) %>%
    dplyr::rename("FishCollection" = "tax_No fish collected")


  ##remove 0 column sum fish (not sure why they're even included, but remove anyway)
  full_fish <- full_fish  %>%
    ungroup()  %>%
    dplyr::rename("SampleTypeCode" = "FISH_PROTOCOL") %>%
    dplyr::rename("SampleMethod" = "MethodBasic") %>%
    dplyr::rename("MethodEffort" = "StandardMethod") %>%
    dplyr::select(any_of(.finalcovarorder), tidyselect::contains("tax_")) %>%
    dplyr::relocate(tidyselect::contains("tax_"), .after = last_col()) %>%
    dplyr::select(-any_of(c(names(which((colSums(full_fish %>% dplyr::select(tidyselect::contains("tax_")), na.rm = T)) == 0)))))

  if(!isTRUE(boatableStreams)){
    full_fish <- full_fish %>%
      ##remove boatable sites
      filter(SampleTypeCode != "BOATABLE")
  } else {}

  full_fish <- full_fish %>%
    mutate(SampleTypeCode = ifelse(SampleTypeCode == "SM_WADEABLE",
                                  "Small Wadeable",
                                  ifelse(SampleTypeCode == "BOATABLE",
                                         "Boatable",
                                         "Large Wadeable")))

  if(!isTRUE(hybrids)) {
    full_fish <- full_fish %>%
      dplyr::select(-c(tidyselect::contains(" x ")),
                       -c(tidyselect::contains(".x.")),
                          -c(tidyselect::contains(" x-")))
  }

  ##shared Taxa code
  if(isTRUE(sharedTaxa) & any(grepl("EPA", agency)) & any(grepl("USGS", agency))){
    ##List of NAWQA taxa
    cat('\r',"Removing taxa not in both USGS and EPA datasets                 ")

    notbothfish <- colnames(full_fish %>%
                              dplyr::group_by(Agency) %>%
                              dplyr::summarize(dplyr::across(tidyselect::contains("tax_"), ~sum(., na.rm = T))) %>%
                              dplyr::select(-Agency) %>%
                              dplyr::select(tidyselect::where(~ any(. ==  0, na.rm = T))))

    full_fish <- full_fish %>%
      dplyr::select(-tidyselect::any_of(notbothfish))

  } else {}

  colnames(full_fish) = sub("tax_", "", colnames(full_fish))
  cat("\r","finsyncR data syncronization complete                          \n")
  return(data.frame(full_fish))

}
