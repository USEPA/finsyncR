.TaxLevCols_Algae = list("AlgalGroup" = list("mycols" = c("Phylum", "Class",
                                                   "Order", "Family",
                                                   "Genus", "Species"),
                                      "taxcols" = c("AlgalGroup",
                                                    "Phylum", "Class",
                                                    "Order", "Family",
                                                    "Genus", "Species",
                                                    "Variety", "Form",
                                                    "Subspecies")),
                  "Phylum" = list("mycols" = c("AlgalGroup", "Class",
                                               "Order", "Family",
                                               "Genus", "Species"),
                                  "taxcols" = c("Phylum", "Class",
                                                "Order", "Family",
                                                "Genus", "Species",
                                                "Variety", "Form",
                                                "Subspecies")),
                  "Class" = list("mycols" = c("AlgalGroup", "Phylum",
                                              "Order", "Family",
                                              "Genus", "Species"),
                                 "taxcols" = c("Class",
                                               "Order", "Family",
                                               "Genus", "Species",
                                               "Variety", "Form",
                                               "Subspecies")),
                  "Order" = list("mycols" = c("AlgalGroup", "Phylum",
                                              "Class",
                                              "Family",
                                              "Genus", "Species"),
                                 "taxcols" = c("Order", "Family",
                                               "Genus", "Species",
                                               "Variety", "Form",
                                               "Subspecies")),
                  "Family" = list("mycols" = c("AlgalGroup", "Phylum",
                                               "Class",
                                               "Order",
                                               "Genus", "Species"),
                                  "taxcols" = c("Family",
                                                "Genus", "Species",
                                                "Variety", "Form",
                                                "Subspecies")),
                  "Genus" = list("mycols" = c("AlgalGroup", "Phylum",
                                              "Class",
                                              "Order", "Family",
                                              "Species"),
                                 "taxcols" = c("Genus", "Species",
                                               "Variety", "Form",
                                               "Subspecies")),
                  "Species" = list("mycols" = c("AlgalGroup", "Phylum",
                                                "Class",
                                                "Order", "Family",
                                                "Genus"),
                                   "taxcols" = c("Species",
                                                 "Variety", "Form",
                                                 "Subspecies"))
)

taxnames = c("Phylum", "Subphylum", "Class", "Subclass", "Infraclass",
             "Superorder", "Order", "Suborder", "Infraorder", "Superfamily",
             "Family", "Subfamily", "Tribe", "Genus", "Subgenus", "Species",
             "Subspecies")

.TaxLevCols_Inverts = list("Phylum" = list("mycols" = taxnames[-(which(taxnames == "Phylum"))],
                                           "taxcols" = taxnames[(which(taxnames == "Phylum")):length(taxnames)]),
                           "Subphylum" = list("mycols" = taxnames[-(which(taxnames == "Subphylum"))],
                                              "taxcols" = taxnames[(which(taxnames == "Subphylum")):length(taxnames)]),
                           "Class" = list("mycols" = taxnames[-(which(taxnames == "Class"))],
                                          "taxcols" = taxnames[(which(taxnames == "Class")):length(taxnames)]),
                           "Subclass" = list("mycols" = taxnames[-(which(taxnames == "Subclass"))],
                                             "taxcols" = taxnames[(which(taxnames == "Subclass")):length(taxnames)]),
                           "Infraclass" = list("mycols" = taxnames[-(which(taxnames == "Infraclass"))],
                                               "taxcols" = taxnames[(which(taxnames == "Infraclass")):length(taxnames)]),
                           "Superorder" = list("mycols" = taxnames[-(which(taxnames == "Superorder"))],
                                               "taxcols" = taxnames[(which(taxnames == "Superorder")):length(taxnames)]),
                           "Order" = list("mycols" = taxnames[-(which(taxnames == "Order"))],
                                          "taxcols" = taxnames[(which(taxnames == "Order")):length(taxnames)]),
                           "Suborder" = list("mycols" = taxnames[-(which(taxnames == "Suborder"))],
                                             "taxcols" = taxnames[(which(taxnames == "Suborder")):length(taxnames)]),
                           "Infraorder" = list("mycols" = taxnames[-(which(taxnames == "Infraorder"))],
                                               "taxcols" = taxnames[(which(taxnames == "Infraorder")):length(taxnames)]),
                           "Superfamily" = list("mycols" = taxnames[-(which(taxnames == "Superfamily"))],
                                                "taxcols" = taxnames[(which(taxnames == "Superfamily")):length(taxnames)]),
                           "Family" = list("mycols" = taxnames[-(which(taxnames == "Family"))],
                                           "taxcols" = taxnames[(which(taxnames == "Family")):length(taxnames)]),
                           "Subfamily" = list("mycols" = taxnames[-(which(taxnames == "Subfamily"))],
                                              "taxcols" = taxnames[(which(taxnames == "Subfamily")):length(taxnames)]),
                           "Tribe" = list("mycols" = taxnames[-(which(taxnames == "Tribe"))],
                                          "taxcols" = taxnames[(which(taxnames == "Tribe")):length(taxnames)]),
                           "Genus" = list("mycols" = taxnames[-(which(taxnames == "Genus"))],
                                          "taxcols" = taxnames[(which(taxnames == "Genus")):length(taxnames)]),
                           "Subgenus" = list("mycols" = taxnames[-(which(taxnames == "Subgenus"))],
                                             "taxcols" = taxnames[(which(taxnames == "Subgenus")):length(taxnames)]),
                           "Species" = list("mycols" = taxnames[-(which(taxnames == "Species"))],
                                            "taxcols" = taxnames[(which(taxnames == "Species")):length(taxnames)]),
                           "Subspecies" = list("mycols" = taxnames[-(which(taxnames == "Subspecies"))],
                                               "taxcols" = taxnames[(which(taxnames == "Subspecies")):length(taxnames)])
)

taxnamesFISH = c("Superclass", "Class", "Subclass",
             "Superorder", "Order", "Superfamily",
             "Family", "Subfamily", "Genus", "Species",
             "Subspecies")

.TaxLevCols_Fish = list("Superclass" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Superclass"))],
                                              "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Superclass")):length(taxnamesFISH)]),
                           "Class" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Class"))],
                                          "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Class")):length(taxnamesFISH)]),
                           "Subclass" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Subclass"))],
                                             "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Subclass")):length(taxnamesFISH)]),
                           "Superorder" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Superorder"))],
                                               "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Superorder")):length(taxnamesFISH)]),
                           "Order" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Order"))],
                                          "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Order")):length(taxnamesFISH)]),
                           "Superfamily" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Superfamily"))],
                                                "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Superfamily")):length(taxnamesFISH)]),
                           "Family" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Family"))],
                                           "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Family")):length(taxnamesFISH)]),
                           "Subfamily" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Subfamily"))],
                                              "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Subfamily")):length(taxnamesFISH)]),
                           "Genus" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Genus"))],
                                          "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Genus")):length(taxnamesFISH)]),
                           "Species" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Species"))],
                                            "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Species")):length(taxnamesFISH)]),
                           "Subspecies" = list("mycols" = taxnamesFISH[-(which(taxnamesFISH == "Subspecies"))],
                                               "taxcols" = taxnamesFISH[(which(taxnamesFISH == "Subspecies")):length(taxnamesFISH)])
)

## Load in 'Proprietary' dataset provided by USGS personnel
##NEED TO GET THIS IN.
# Proprietary <- read.csv("./data/Proprietary.csv")
# Proprietary <- read.csv("./data/Proprietary.csv")

## Create a character ratio to avoid reading in the file and generating decimal ratios
# Proprietary$Ratio <- paste('"', Proprietary$NWQLSubsamplingCode, '"', sep = " ")

## Load in 'Sampler' dataset generated from 'Proprietary' dataset
## 'Sampler' contains all unique "Ratio" entries
## Unique "Ratio" entries were assigned a "SamplerType" of FS (Folsom Sampler)
## or Grid (Gridded Tray) based upon USGS OFR 00-212
## (Moulton et al. 2000 Methods of analysis by the U.S. Geological Survey
## National Water Quality Laboratory -- Processing, taxonomy, and quality
## control of benthic macroinvertebrate samples)
## 'SamplerType' decided by DKJ
# Sampler <- read.csv("./data/Grotheer -- Ratio&SamplerType.csv")
# Sampler <- read.csv("./data/Grotheer -- Ratio&SamplerType.csv")
#
# Sampler$Ratio <- as.character(Sampler$Ratio)

## Combine 'Proprietary' and 'Sampler' using the "Ratio" character
# .SamplingRatio_SamplerType <- left_join(Proprietary, Sampler, by = "Ratio")




.ReorderUSGSBioDataColNames = c("ProjectLabel",
                                "ProjectAssignedSampleLabel",
                                "NAWQA.SMCOD",
                                "NAWQAStudyUnitCode",
                                "CollectionDate",
                                "StartTime",
                                "TimeDatum",
                                "CollectionYear",
                                "CollectionMonth",
                                "CollectionDayOfYear",
                                "SiteVisitSampleNumber",
                                "ProvisionalData",
                                "SiteNumber",
                                "SiteName",
                                "StudyReachName",
                                "SiteTypeName",
                                "StateFIPSCode",
                                "CountyFIPSCode",
                                "Latitude_dd",
                                "Longitude_dd",
                                "CoordinateDatum",
                                "HUCCode",
                                "DrainageArea_mi2",
                                "SampleTypeCode",
                                "IdentificationEntity",
                                "AreaSampTot_m2",
                                "GeomorphicChannelUnit",
                                "ChannelBoundaries",
                                "ChannelFeatures",
                                "ReplicateType",
                                "MethodCode",
                                "MethodBasic",
                                "ReachLengthFished_m",
                                "NumberSeineHauls",
                                "NumberStationarySetsKicks",
                                "NumberSnorkelingTransects",
                                "MinutesShockTime",
                                "StandardMethod",
                                "SamplingMethodOrDevice",
                                "PeriphytonHabitatSampled",
                                "SubsurfaceGrabDepth_m")

# .site.info <- read.csv("/Users/samantharumschlag/Documents/PowellCenter/OnlineMeetingSpring2021/PestCntyFunction/Biodata_Site_List.csv",
#                       colClasses = c("StateFIPSCode" = "character",
#                                      "CountyFIPSCode" = "character"))
.site.info <- read.csv("C:/Users/mikem/Documents/Research/USGS Stream Macros/PestDataForRPackageBuild/Biodata_Site_List.csv",
                       colClasses = c("StateFIPSCode" = "character",
                                      "CountyFIPSCode" = "character"))


# .pest.info <- read.csv("/Users/samantharumschlag/Documents/PowellCenter/OnlineMeetingSpring2021/PestCntyFunction/pestclassPANall_22Feb21.csv") %>%
#   dplyr::mutate(Name = stringr::str_to_lower(Name),
#          class = Class,
#          type = Type)
.pest.info <- read.csv("C:/Users/mikem/Documents/Research/USGS Stream Macros/PestDataForRPackageBuild/pestclassPANall_22Feb21.csv") %>%
  dplyr::mutate(Name = stringr::str_to_lower(Name),
                class = Class,
                type = Type)


# .clust_labels = read.csv("/Users/samantharumschlag/Documents/PowellCenter/Code/MahonRumschlagPowell/clust_labels.csv",
#                         header=T, stringsAsFactors = FALSE)
.clust_labels = read.csv("C:/Users/mikem/Documents/Research/USGS Stream Macros/MahonRumschlagPowell/clust_labels.csv",
                         header=T, stringsAsFactors = FALSE)


.switch1to1 <- data.frame(BenchGenus = c("Anisogammarus",
                                         "Asellus",
                                         "Asheum",
                                         "Corophium",
                                         "Deronectes",
                                         "Desserobdella",
                                         "Dina",
                                         "Mooreobdella",
                                         "Nephelopsis",
                                         "Physella",
                                         "Pristinella",
                                         "Stelechomyia",
                                         "Macronema",
                                         "Gloiobdella",
                                         "Goniobasis",
                                         "Teutonia",
                                         "Trasserkidrilus",
                                         "Galba",
                                         "Hebetancylus"),
                          Genus = c("Ramellogammarus",
                                    "Caecidotea",
                                    "Polypedilum",
                                    "Americorophium",
                                    "Stictotarsus",
                                    "Placobdella",
                                    "Erpobdella",
                                    "Erpobdella",
                                    "Erpobdella",
                                    "Physa",
                                    "Pristina",
                                    "Kribiodorum",
                                    "Macrostemum",
                                    "Helobdella",
                                    "Elimia",
                                    "Amboakis",
                                    "Tasserkidrilus",
                                    "Fossaria",
                                    "Hebetoncylus"))

##Code to get crosswalk NRSA site IDs
NRSA_siteIDs <- read.csv("C:/Users/mikem/Documents/Research/USGS Stream Macros/MahonRumschlagPowell/NARS_AllSurvey_SITE_ID_CROSSWALK.csv")

#Filter out all other EPA water sampling projects, leaving NRSA
#select only the columns that we need: site id (sampling round specific) and
#  unique id (crosses all sampling rounds)
NRSA_siteIDs = NRSA_siteIDs %>%
  filter(SURVEY == "NRSA") %>%
  select(SITE_ID, UNIQUE_ID)

##Read in 0809 sites to match the crosswalk UNIQUE_IDs w/ WSA (MASTER_SITEID)
NRSA_0809_sites = read.csv("https://www.epa.gov/sites/production/files/2015-09/siteinfo_0.csv",
                           colClasses = c("UID" = "character"),
                           stringsAsFactors = FALSE)

#Select only site and master site ids
NRSA_0809_sites <- NRSA_0809_sites %>%
  select(SITE_ID, MASTER_SITEID)

#if the crosswalk site ids are in the 0809 site id dataset, match the site ids and
# pull the master site ids from the 0809 dataset (have to do it this way, instead
# of a left_join, because there was a weird error in the dataset)
NRSA_siteIDs$MASTER_SITEID <- ifelse(NRSA_siteIDs$SITE_ID %in% NRSA_0809_sites$SITE_ID,
                                     NRSA_0809_sites$MASTER_SITEID[match(NRSA_siteIDs$SITE_ID,
                                                                         NRSA_0809_sites$SITE_ID)],
                                     NA)

#If there is no master site id (NA or ""), use the site id
.NRSA_siteIDs = NRSA_siteIDs %>%
  mutate(MASTER_SITEID = ifelse(is.na(MASTER_SITEID),
                                SITE_ID,
                                MASTER_SITEID),
         MASTER_SITEID = ifelse(MASTER_SITEID == "",
                                SITE_ID,
                                MASTER_SITEID))


.InvertIDCols <-  c("ProjectLabel", "ProjectAssignedSampleLabel", "NAWQA.SMCOD",
                    "NAWQAStudyUnitCode", "CollectionDate", "CollectionYear",
                    "CollectionMonth", "CollectionDayOfYear", "SiteVisitSampleNumber",
                    "SiteNumber", "SiteName", "StudyReachName", "SiteTypeName",
                    "Latitude_dd", "Longitude_dd", "CoordinateDatum", "SampleTypeCode",
                    "IdentificationEntity", "GeomorphicChannelUnit", "FieldSplitRatio",
                    "LabSubsamplingRatio", "PropID", "AreaSampTot_m2", "WettedWidth")

.allsitesCOMID = allsitesCOMID


# usethis::use_data(.InvertIDCols,
#                   .allsitesCOMID,
#                   internal = TRUE,
#                   overwrite = FALSE)
# save(list = c(sysdata_filenames[-12], ".allsitesCOMID"), file = "R/sysdata.rda")

# .pest.dat <- utils::read.table(base::unz(base::system.file("extdata",
#                                                              "pestCountyEstYrs.zip",
#                                                              package = "StreamData"),
#                                            "pestCountyEstYrs.txt"),
#                                  sep = "\t", header= T,
#                                  colClasses = c("STATE_FIPS_CODE" = "character",
#                                                 "COUNTY_FIPS_CODE" = "character"))  %>%
#   mutate(compound = stringr::str_to_lower(COMPOUND))
sysdata_filenames <- load("R/sysdata.rda")
# .specIDgen <- read.csv("SpecimenIdToGenusAllSites.csv")
# .specIDgen <- FULLDAT


# save(list = c(sysdata_filenames[-14], ".specIDgen"), file = "R/sysdata.rda",
#      compress = "xz")

# save(list = c(sysdata_filenames[-c(7,13)]), file = "R/sysdata.rda",
#      compress = "xz")


.taxlu <- dplyr::tribble(
  ~Order, ~Family,
  "Neotaenioglossa", "Hydrobiidae",
  "Neotaenioglossa", "Pleuroceridae",
  "Neotaenioglossa", "Pomatiopsidae",
  "Neotaenioglossa", "Thiaridae",
  "Architaenioglossa", "Pilidae",
  "Architaenioglossa", "Ampullariidae",
  "Architaenioglossa", "Viviparidae",
  "Unionoida", "Unionidae",
  "Unionoida", "Margaritiferidae",
  "Tubificida", "Naididae",
  "Tubificida", "Tubificidae",
  "Opisthopora", "Lumbricidae",
  "Heterostropha", "Valvatidae",
  "Hirudinida", "Piscicolidae",
  "Neoophora", "Planariidae",
  "", "Aeolosomatidae",
) %>%
  dplyr::mutate(FAMILY = toupper(Family))

##need to check old code to generate .slashgen_fin
.fishtaxlu <- dplyr::tribble(
  ~Old, ~New,
  "Cottus bairdi", "Cottus bairdii",
  "Macrhybopsis cf. aestivalis", "Macrhybopsis aestivalis",
  "Notropis cf. spectrunculus", "Notropis spectrunculus",
  "Catostomus cf. latipinnis", "Catostomus latipinnis",
  "Cyprinella cf. zanema", "Cyprinella zanema",
  "Hybopsis zanema", "Cyprinella zanema",
  "Noturus sp. c.f. leptacanthus", "Noturus leptacanthus",
  "Moxostoma sp cf erythrurum", "Moxostoma erythrurum",
  "Moxostoma cf. lachneri", "Moxostoma lachneri",
  "Moxostoma cf. poecilurum", "Moxostoma poecilurum",
  "Moxostoma duquesnii", "Moxostoma duquesnei",
  "Etheostoma chlorosomum", "Etheostoma chlorosoma",
  "Fundulus stellifera", "Fundulus stellifer",
  "Chaenobryttus gulosus", "Lepomis gulosus",
  "Hybopsis dorsalis", "Notropis dorsalis",
  "Oncorhynchus mykiss gairdneri", "Oncorhynchus mykiss",
  "Aequidens portalegrensis", "Cichlasoma portalegrensis",
  "Herichthys cyanoguttatum",	"Herichthys cyanoguttatus",
  "Lethenteron japonicum",	"Lethenteron camtschaticum",
  "Tilapia zillii",	"Coptodon zillii",
  "Phoxinus cumberlandensis",	"Chrosomus cumberlandensis",
  "Phoxinus eos",	"Chrosomus eos",
  "Phoxinus erythrogaster",	"Chrosomus erythrogaster",
  "Phoxinus neogaeus",	"Chrosomus neogaeus",
  "Phoxinus oreas",	"Chrosomus oreas",
  "Phoxinus tennesseensis",	"Chrosomus tennesseensis"  )

.fishtaxlu$NewGenus <- gsub(" .*$","",.fishtaxlu$New)

.finalcovarorder <-
  c("Agency",
    "SampleID",
    "ProjectLabel",
    "SiteNumber",
    "StudyReachName",
    "CollectionDate",
    "CollectionYear",
    "CollectionMonth",
    "CollectionDayOfYear",
    "Latitude_dd",
    "Longitude_dd",
    "CoordinateDatum",
    "COMID",
    "StreamOrder",
    "WettedWidth",
    "PredictedWettedWidth_m",
    "NARS_Ecoregion",
    "SampleTypeCode",
        ##inverts
    "AreaSampTot_m2",
    "FieldSplitRatio",
    "LabSubsamplingRatio",
    "PropID",
    "Gen_ID_Prop",
    ##fish
    "FishCollection",
    "ReachLengthFished_m",
    "SampleMethod",
    "MethodEffort"
    )

# .allsitesCOMID = read.csv("C:/Users/mmahon/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Research/USGS Stream Macros/allsitesCOMID.csv")

.slashgen_fin <- c(.slashgen_fin, "Arctopelopia/Conchapelopia/Hayesomyia/Helopelopia/Meropelopia/Rheopelopia/Telopelopia/Thienemannimyia")


.slashgen_fin <- .slashgen_fin[-30]
sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames[-13], ".allsitesCOMID"), file = "R/sysdata.rda",
     compress = "xz")


# usethis::use_data(.TaxLevCols_Algae, .TaxLevCols_Inverts,
#                   .TaxLevCols_Fish, .SamplingRatio_SamplerType,
#                   .ReorderUSGSBioDataColNames,
#                   .site.info,
#                   .pest.info,
#                   .clust_labels,
#                   .switch1to1,
#                   .NRSA_siteIDs,
#                   .InvertIDCols,
#                   .allsitesCOMID,
#                   internal = TRUE,
#                   overwrite = TRUE)
