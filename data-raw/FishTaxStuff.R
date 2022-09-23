##Fish data
#pull data from EPA NARS website

##



##Fish trait dataset from EPA - this would be good to look at, with regard to
## a basic 4th corner analysis
##IGNORE FOR NOW
#Includes:
##Habitat preference
##Migratory status
##Reproductive strategy
##Temperature preference
##Tolerance categories (based on WEMAP and USGS region 7)
##Trophic guild
##Velocity preferences

#Looking for:
##Native status
##Game status
##Other things


##Read in datasets directly from EPA website - may want a more stable source
##in the future (github repo?)
NRSA_1819_fishcnt = read.csv("https://www.epa.gov/system/files/other-files/2022-03/nrsa-1819-fish-count-data.csv",
                             colClasses = c("UID" = "character"),
                             stringsAsFactors = FALSE)

NRSA_1819_sites = read.csv("https://www.epa.gov/system/files/other-files/2022-01/nrsa-1819-site-information-data-updated.csv",
                           colClasses = c("UID" = "character"),
                           stringsAsFactors = FALSE)

NRSA_1819_sampinfo <- read.csv("https://www.epa.gov/system/files/other-files/2022-03/nrsa-1819-fish-sampling-information-data.csv",
                               colClasses = c("UID" = "character"),
                               stringsAsFactors = FALSE)



NRSA_1314_fishcnt = read.csv("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_fishcts_04232019.csv",
                             colClasses = c("UID" = "character"),
                             stringsAsFactors = FALSE)

NRSA_1314_fishtax = read.csv("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_fishtaxa_02042019.csv",
                           colClasses = c("UID" = "character"),
                           stringsAsFactors = FALSE)

NRSA_1314_sites = read.csv("https://www.epa.gov/sites/default/files/2019-04/nrsa1314_siteinformation_wide_04292019.csv",
                           colClasses = c("UID" = "character",
                                          "STATECTY" = "character"),
                           stringsAsFactors = FALSE)

NRSA_0809_fishcnts = read.csv("https://www.epa.gov/sites/default/files/2015-09/fishcts.csv",
                             colClasses = c("UID" = "character"),
                             stringsAsFactors = FALSE)

NRSA_0809_sites = read.csv("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                           colClasses = c("UID" = "character"),
                           stringsAsFactors = FALSE)


#Need info on a lot of these
##hybrids need to be split, then extracted from 1314 tax
##Hybrids and unknowns are DONE!
Non_hybrids_unknowns <- NRSA_1819_fishcnt %>%
  filter(IS_DISTINCT == 1) %>%
  group_by(FINAL_NAME) %>%
  slice(1) %>%
  filter(!(FINAL_NAME %in% NRSA_1314_fishtax$FINAL_NAME)) %>%
  select(FINAL_NAME) %>%
  filter(!grepl("X", FINAL_NAME),
         !grepl("UNKNOWN", FINAL_NAME))

Hybrids <- NRSA_1819_fishcnt %>%
  filter(IS_DISTINCT == 1) %>%
  group_by(FINAL_NAME) %>%
  slice(1) %>%
  filter(!(FINAL_NAME %in% NRSA_1314_fishtax$FINAL_NAME)) %>%
  select(FINAL_NAME) %>%
  filter(grepl("X", FINAL_NAME)) %>%
  mutate(Spp1 = sub("\\ X.*", "", FINAL_NAME),
         Spp2 = sub(".*\\X ", "", FINAL_NAME)
)

d1 <- NRSA_1314_fishtax %>%
  filter(FINAL_NAME %in% Hybrids$Spp1 | FINAL_NAME %in% Hybrids$Spp2)

TaxHold <- data.frame(FAM_OR_CLS = "",
                      FAMILY = "",
                      FINAL_NAME = "",
                      GENUS = "",
                      SPECIES = "")

for(i in 1:4){
  Th2 <- d1 %>%
    filter(FINAL_NAME %in% c(Hybrids$Spp1[i], Hybrids$Spp2[i]))

  Th2 <- Th2[match(c(Hybrids$Spp1[i], Hybrids$Spp2[i]), Th2$FINAL_NAME),]

  TaxHold[i,1] <- (Th2 %>%
    group_by(FAM_OR_CLS) %>%
    slice(1))$FAM_OR_CLS
  TaxHold[i,2] <- (Th2 %>%
                     group_by(FAM_OR_CLS) %>%
                     slice(1))$FAM_OR_CLS
  TaxHold[i,3] <- Hybrids$FINAL_NAME[i]
  TaxHold[i,4] <- ifelse(length((Th2 %>%
                     group_by(GENUS) %>%
                     slice(1))$GENUS) == 1,
                     (Th2 %>%
                        group_by(GENUS) %>%
                        slice(1))$GENUS,
                     paste((Th2 %>%
                              group_by(GENUS) %>%
                              slice(1))$GENUS, collapse = " X "))
  TaxHold[i,5] <-      paste((Th2 %>%
                                  group_by(SPECIES) %>%
                                  slice(1))$SPECIES, collapse = " X ")
}


NRSA_1314_fishtax <- (bind_rows(NRSA_1314_fishtax, TaxHold))

##Fixed all unknowns
Unknowns <- NRSA_1819_fishcnt %>%
  filter(IS_DISTINCT == 1) %>%
  group_by(FINAL_NAME) %>%
  slice(1) %>%
  filter(!(FINAL_NAME %in% NRSA_1314_fishtax$FINAL_NAME)) %>%
  select(FINAL_NAME) %>%
  filter(grepl("UNKNOWN", FINAL_NAME))

##convert Unknown Gobiidae to Unknown Goby
##convert Unknown Micropeterus to Unknown Bass
##Convert Unknwon Shad to CLUPEIDAE
##Convert Holston Sculpin to Black Sculpin

NRSA_1819_fishcnt <- NRSA_1819_fishcnt %>%
  mutate(FINAL_NAME = ifelse(FINAL_NAME == "UNKNOWN GOBIIDAE",
                             "UNKNOWN GOBY",
                             ifelse(FINAL_NAME == "UNKNOWN MICROPTERUS",
                                    "UNKNOWN BASS",
                                    ifelse(FINAL_NAME == "HOLSTON SCULPIN",
                                           "BLACK SCULPIN",
                                           FINAL_NAME))))


TaxHold2 <- data.frame(FAM_OR_CLS = "CLUPEIDAE",
                      FAMILY = "CLUPEIDAE",
                      FINAL_NAME = "UNKNOWN SHAD",
                      GENUS = "",
                      SPECIES = "")

NRSA_1314_fishtax <- (bind_rows(NRSA_1314_fishtax, TaxHold2))


##0809

##add "-" between large and scaled in largescaled spinycheek sleeper - in 1314 tax
##change PLAINS/WESTERN SILVERY MINNOW to hybrid - in 1314 tax

NRSA_0809_fishcnts <- NRSA_0809_fishcnts %>%
  mutate(FINAL_NAME = ifelse(FINAL_NAME == "LARGESCALED SPINYCHEEK SLEEPER",
                             "LARGE-SCALED SPINYCHEEK SLEEPER",
                             ifelse(FINAL_NAME == "PLAINS/WESTERN SILVERY MINNOW",
                                    "PLAINS MINNOW X WESTERN SILVERY MINNOW",
                                    FINAL_NAME)))

NRSA_0809_fishcnts %>%
filter(IS_DISTINCT == 1) %>%
  group_by(FINAL_NAME) %>%
  slice(1) %>%
  mutate(FINAL_NAME = toupper(FINAL_NAME)) %>%
  filter(!(FINAL_NAME %in% NRSA_1314_fishtax$FINAL_NAME)) %>%
  select(FINAL_NAME)

##Need info on fat snook


library(taxize)

namesneeded <- c(tolower(Non_hybrids_unknowns$FINAL_NAME), "fat snook")
taxhld <- comm2sci(namesneeded, db = "itis")

TaxHold3 <- data.frame(FAM_OR_CLS = "",
                       FAMILY = tax_name(c(unlist(taxhld[which(unlist(lapply(taxhld,
                                             length)) == 1)])), get = 'family', db = 'itis')$family,
                       FINAL_NAME = toupper(namesneeded[which(unlist(lapply(taxhld, length)) == 1)]),
                       GENUS = sub("\\ .*" ,
                                   "",
                                   c(unlist(taxhld[which(unlist(lapply(taxhld,
                                                                       length)) == 1)])))
,
                       SPECIES = sub(".*\\ " ,
                                     "",
                                     c(unlist(taxhld[which(unlist(lapply(taxhld,
                                                                         length)) == 1)])))
)

TaxHold3 <- TaxHold3 %>%
  mutate(FAMILY = toupper(FAMILY),
         FAM_OR_CLS = FAMILY)


NRSA_1314_fishtax <- (bind_rows(NRSA_1314_fishtax, TaxHold3))


taxhld2 <- taxhld[namesneeded[which(unlist(lapply(taxhld, length)) != 1)]]


TaxHold4 <- data.frame(FAM_OR_CLS = "",
                      FAMILY = tax_name(c(taxhld2[[3]][1], taxhld2[[5]][1], taxhld2[[9]][2], taxhld2[[10]][1],
                                          taxhld2[[11]][1],taxhld2[[12]][1],
                                          taxhld2[[16]][2], taxhld2[[18]][2], taxhld2[[19]][1], taxhld2[[20]][2]),
                                        get = 'family', db = 'itis')$family,
                      FINAL_NAME = toupper(names(taxhld2[c(3, 5, 9, 10, 11, 12, 16, 18, 19, 20)])),
                      GENUS = sub("\\ .*" ,
                                  "",c(taxhld2[[3]][1], taxhld2[[5]][1], taxhld2[[9]][2], taxhld2[[10]][1],
                                        taxhld2[[11]][1],taxhld2[[12]][1],
                                        taxhld2[[16]][2], taxhld2[[18]][2], taxhld2[[19]][1], taxhld2[[20]][2])),
                      SPECIES = sub(".*\\ " ,
                                    "",c(taxhld2[[3]][1], taxhld2[[5]][1], taxhld2[[9]][2], taxhld2[[10]][1],
                                         taxhld2[[11]][1],taxhld2[[12]][1],
                                         taxhld2[[16]][2], taxhld2[[18]][2], taxhld2[[19]][1], taxhld2[[20]][2])))

TaxHold4$FAMILY[c(2, 5, 6, 9)] = c("Cichlidae","Percidae","Ariidae","Percidae")
TaxHold4 <- TaxHold4 %>%
  mutate(FAMILY = toupper(FAMILY),
         FAM_OR_CLS = FAMILY)


NRSA_1314_fishtax <- (bind_rows(NRSA_1314_fishtax, TaxHold4))

TaxHold5 <- data.frame(FAM_OR_CLS = "",
                       FAMILY = c("Cichlidae","Leuciscidae","Synbranchidae","Catostomidae","Centrarchidae","Salmonidae","Leuciscidae","","Moronidae","Percidae"),
                       FINAL_NAME = names(taxhld2[-c(3,5,9,10,11,12,16,18,19,20)]),
                       GENUS = c("Hemichromis","Margariscus","Monopterus","Moxostoma","Micropterus","Coregonus","Campostoma","","Morone","Percina"),
                       SPECIES = c("letourneuxi","margarita","albus","sp cf erythrurum","","haiaka","spadiceum","","saxatilis X spp","Sipsi"))

TaxHold5 <- TaxHold5 %>%
  mutate(FAMILY = toupper(FAMILY),
         FAM_OR_CLS = FAMILY)

NRSA_1314_fishtax <- (bind_rows(NRSA_1314_fishtax, TaxHold5))

NRSA_1314_fishtax <- NRSA_1314_fishtax %>%
  mutate(FINAL_NAME = toupper(FINAL_NAME))
##TAXONOMY IS COMPLETED

write.csv(NRSA_1314_fishtax, "C:/Users/mikem/Documents/R Package Builds/StreamData/inst/extdata/updateNRSAfishtax.csv",
          row.names = F)


#############
##2008/2009
NRSA_0809 <- NRSA_0809_fishcnts %>%
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
  filter(IS_DISTINCT == 1) %>%
  filter(NOTFISH == "") %>%
  select(UID:VISIT_NO, DATE_COL, ELECTROFISH:METHOD, FISH_PROTOCOL, FAMILY:SCIENTIFIC, FINAL_NAME, FINAL_CT) %>%
  mutate(DATE_COL = as.Date(DATE_COL, "%d-%B-%y"))



##2013/2014
NRSA_1314 <- NRSA_1314_fishcnt %>%
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
  filter(IS_DISTINCT == 1) %>%
  dplyr::select(UID, SITE_ID:LON_DD83, FAMILY:SCIENTIFIC, FINAL_NAME, TOTAL)%>%
  mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))


##2018/2019
NRSA_1819 <- NRSA_1819_fishcnt %>%
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
  filter(IS_DISTINCT == 1) %>%
  dplyr::select(UID, SITE_ID:VISIT_NO, STATE, FAMILY:SCIENTIFIC, FINAL_NAME, TOTAL)%>%
  mutate(DATE_COL = as.Date(DATE_COL, "%m/%d/%Y"))


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

##Join NRSA_fish_sites with actual data

NRSA_FISH_wSite <- NRSA_FISH %>%
  left_join(NRSA_fish_sites %>%
              group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
              slice(1)) %>%
  mutate(FISH_PROTOCOL = ifelse(is.na(BOAT_WADE),
                                FISH_PROTOCOL,
                                BOAT_WADE)) %>%
  dplyr::select(-BOAT_WADE)


NRSA_FISH_comm <- NRSA_FISH_wSite %>%
  filter(!is.na(GENUS) & GENUS != "") %>%
  filter(!is.na(SPECIES) & SPECIES != "") %>%
  filter(!grepl(" or ", SCIENTIFIC)) %>%
  dplyr::select(-FAMILY, -GENUS, -SPECIES, -FINAL_NAME) %>%
  pivot_wider(names_from = SCIENTIFIC,
              values_from = TOTAL,
              values_fn = sum,
              values_fill = 0)

##to remove hybrids (41)
NRSA_FISH_comm_noHybrid <- NRSA_FISH_comm %>%
  dplyr::select(-c(tidyselect::contains(" X ")))


