#' Access and harmonize fish data
#'
#' @description
#' This function generates an occurrence or abundance community matrix for
#' fish sampled in rivers and streams from the US EPA National Rivers and Streams
#' Assessment and USGS BioData.
#'
#' @param dataType Output data type for the community matrix, either
#'   \code{"abun"} (abundance) or \code{"occur"} (occurrence).
#' @param taxonLevel Level of taxonomic resolution for the community matrix.
#'   Input must be one of: \code{"Family"}, \code{"Genus"}, or \code{"Species"}.
#' @param agency The agency name or names (e.g., "USGS" and "EPA") that are the
#'   source of data for the output community matrix. See \code{Details} below for more information.
#' @param standardize Standardization method to be used for calculating fish abundance matrices.
#'   Default is \code{standardize = "none"}, which returns raw fish count values. Other options include
#'   \code{standardize = "CPUE"}, which returns standardized abundances in Catch per Unit Effort.
#'   An alternative standardization method is \code{standardize = "MGMS"}, which uses
#'   Multigear Mean Standardization (MGMS) values to account for catchability differences between
#'   fish sampling methods. See 'Details' for more information on standardizations.
#' @param hybrids logical. Should hybrid individuals be included in the output dataset?
#'   \code{TRUE} or \code{FALSE}.
#' @param sharedTaxa logical. Should taxa be limited to those organisms that
#'   appear in both the EPA and USGS datasets? \code{TRUE} or \code{FALSE}. Must be set to
#'   \code{FALSE} when only one agency is specified.
#' @param boatableStreams logical. Should EPA boatable streams be included in the
#'   output dataset? \code{TRUE} or \code{FALSE}. Note: most USGS fish samples are
#'   from wadable streams. It is not advisable to include boatable streams when building a dataset
#'   including both EPA and USGS data. Boatable EPA data and wadeable USGS data
#'   are not necessarily comparable.
#'
#' @return A taxa by sample data frame with site, stream reach, and sample information.
#'
#' @details
#'   \code{agency} refers to the federal agency that collected the fish samples. If
#'   you want to use data from both agencies, set \code{agency = c("USGS", "EPA")},
#'   which is the default. Note that by default, only moving waters classified as
#'   "wadeable" are included, but setting \code{boatableStreams = TRUE} will
#'   include non-wadeable streams. Some information included in the EPA dataset
#'   are not included in the USGS datasets, specifically observed wetted width of the stream or river.
#'
#'   \code{taxonLevel} refers to the taxonomic resolution (Species, Genus, Family, etc.)
#'   for the sample by taxa matrix. The input values for this parameter are case
#'   sensitive and must start with a capital letter. All observations taxonomically
#'   coarser than the `taxonLevel` provided are dropped from the output community matrix.
#'   For instance, if `taxonLevel = "Genus"`, then observations identified at
#'   Subfamily, Family, Order, Class, or Phylum levels are dropped. "Species" is
#'   the finest level of taxonomic resolution provided for fish.
#'
#'   To standardize fish abundance data (\code{standardize = "CPUE"}), abundances
#'   are divided by the product of
#'   sampling effort (minutes shocked, number of seine hauls, number of
#'   snorkeling transects) and stream length sampled.
#'   \deqn{CPUE = \frac{taxa~abundance}{(sampling~effort~*~stream~length~fished~(m))}}
#'
#'   When (\code{standardize = "CPUE"}) or (\code{standardize = "none"}), sampling
#'   events that used multiple gear types will have rows of data for each unique
#'   gear type (i.e. sampling event in which
#'   both electroshocking and senining were used will have separate rows of data for
#'   each gear type). To account for differences in efficacy between shocking, seining, and
#'   snorkeling, multigear mean standardization (\code{standardize = "MGMS"}) is
#'   another standardization method provided as an alternative to catch per unit
#'   effort (\code{standardize = "CPUE"}). When (\code{standardize = "MGMS"}), individual
#'   taxa abundances are standardized for each gear type (i.e. electroshock, seine net,
#'   snorkel), as above in `CPUE`. Then, for each gear type, the CPUE
#'   of all *i* taxa in each sample *j* is summed to get Total Catch Per Unit Effort
#'   for sample *j* (TCPUE~*j*~): \deqn{TCPUE_j = \sum_{} CPUE_{ij}}
#'
#'   For each gear type, the mean TCPUE is calculated, \eqn{\overline{TCPUE}}.
#'   Next, to standardize each gear, CPUE for each taxa *i* is divided by
#'   \eqn{\overline{TCPUE}}.
#'   \deqn{MSC_{ij} = \frac{CPUE_{ij}}{\overline{TCPUE}}}
#'   \eqn{MSC_{ij}} is the mean standardized catch of species *i* in observation
#'   *j*. The units of sampling effort are cancelled in the calculation of
#'   \eqn{MSC_{ij}}, but patterns of relative abundance
#'   of species within and across observations are preserved. The function then
#'   sums the \eqn{MSC_{ij}} among gear types, resulting in a single row of data for
#'   each sampling event, regardless of the number of gear types used, such that
#'   setting \code{standardize = "CPUE"} will result in more rows within the output
#'    dataset than \code{standardize = "MGMS"}. See Gibson-Reinemer et al. (2017)
#'    for more information regarding the computation of MGMS
#'
#'
#'   Some of the samples lacked information on either stream length sampled or
#'   sampling effort. Therefore,
#'   if a user is interested in occurrence (pres/abs) data only, then set
#'   \code{dataType = "occur" and standardize = "none"}, which will provide an
#'   occurrence dataset the samples that are otherwise dropped with standardization.
#'   Be aware that setting \code{dataType = "occur"} will result in a larger
#'   dataset with additional samples/sites than when \code{dataType = "abun"}.
#'

#'
#' @author Michael Mahon, Ethan Brown, Samantha Rumschlag, Terry Brown
#'
#' @references Gibson-Reinemer DK, Ickes BS, Chick JH, 2014. Development and assessment of a new method for combining
#' catch per unit effort data from different fish sampling gears: Multigear mean standardization (MGMS).
#' Can. J. Fish. Aquat. Sci. 74:8-14.
#'
#' @examples
#' \dontrun{
#' Fish <- getFishData(taxonLevel = "Species")
#' }
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

    if(taxonLevel.nrsa == "SCIENTIFIC"){
      NRSA_FISH_comm <- NRSA_FISH_comm  %>%
        dplyr::mutate(`tax_No fish collected` = ifelse(`tax_No fish collected` == 0,
                                                "Fish Collected",
                                                "No Fish Collected")) %>%
        dplyr::rename("FishCollection" = "tax_No fish collected") %>%
        dplyr::relocate(tidyselect::contains("tax_"), .after = FishCollection)
    } else{
      NRSA_FISH_comm <- NRSA_FISH_comm  %>%
        dplyr::mutate(`tax_No fish` = ifelse(`tax_No fish` == 0,
                                      "Fish Collected",
                                      "No Fish Collected")) %>%
        dplyr::rename("FishCollection" = "tax_No fish")%>%
        dplyr::relocate(tidyselect::contains("tax_"), .after = FishCollection)
    }



    cat('\r',"Applying count standardization to EPA data                    ")

    NRSA_FISH_comm2 <- fishStandardization(NRSA_FISH_comm,
                                      dataType,
                                      standardize,
                                      "EPA")
  }


  if(all(grepl("EPA", agency))) {

    full_fish <- NRSA_FISH_comm2 %>%
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
      rename("SampleID" = "SIDNO") %>%
      mutate(dplyr::across(tidyselect::starts_with("tax_"), ~tidyr::replace_na(.,0)))

  } else if(any(grepl("EPA", agency)) & any(grepl("USGS", agency))) {
    cat("\r","Harmonizing USGS and EPA data                                     ")
    full_fish <- bind_rows(fish_comm2 %>%
                             mutate(SiteNumber = paste("USGS-",SiteNumber,sep = ""),
                                    # StandardMethod = as.character(StandardMethod),
                                    WettedWidth = NA,
                                    Agency = "USGS",
                                    FishCollection = "Fish Collected") %>%
                             rename("SampleID" = "SIDNO") %>%
                             dplyr::select(-StateFIPSCode, -CountyFIPSCode, -MethodCode),
                           NRSA_FISH_comm2 %>% mutate(Agency = "EPA"))  %>%
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
    left_join(.allsitesCOMID %>% filter(SiteNumber %in% full_fish$SiteNumber), by = dplyr::join_by(SiteNumber))

  ##remove 0 column sum fish (not sure why they're even included, but remove anyway)
  full_fish <- full_fish  %>%
    ungroup()  %>%
    dplyr::rename("SampleTypeCode" = "FISH_PROTOCOL") %>%
    dplyr::rename("SampleMethod" = "MethodBasic") %>%
    dplyr::rename("MethodEffort" = "StandardMethod") %>%
    dplyr::select(any_of(.finalcovarorder), tidyselect::contains("tax_")) %>%
    dplyr::mutate(MethodEffort_units = ifelse(SampleMethod == "Shocking",
                                              "Minutes",
                                              ifelse(SampleMethod == "Seine",
                                                     "Number of Hauls",
                                              "Number of transects"))) %>%
    dplyr::relocate("MethodEffort_units", .after = "MethodEffort") %>%
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
  cat("\r","finsyncR data synchronization complete                          \n")
  return(data.frame(full_fish))

}
