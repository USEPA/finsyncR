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
Proprietary <- read.csv("/Users/samantharumschlag/Documents/PowellCenter/Code/StreamData/inst/extdata/Proprietary.csv")

## Create a character ratio to avoid reading in the file and generating decimal ratios
Proprietary$Ratio <- paste('"', Proprietary$NWQLSubsamplingCode, '"', sep = " ")

## Load in 'Sampler' dataset generated from 'Proprietary' dataset
## 'Sampler' contains all unique "Ratio" entries
## Unique "Ratio" entries were assigned a "SamplerType" of FS (Folsom Sampler)
## or Grid (Gridded Tray) based upon USGS OFR 00-212
## (Moulton et al. 2000 Methods of analysis by the U.S. Geological Survey
## National Water Quality Laboratory -- Processing, taxonomy, and quality
## control of benthic macroinvertebrate samples)
## 'SamplerType' decided by DKJ
Sampler <- read.csv("/Users/samantharumschlag/Documents/PowellCenter/Code/StreamData/inst/extdata/Grotheer -- Ratio&SamplerType.csv")
Sampler$Ratio <- as.character(Sampler$Ratio)

## Combine 'Proprietary' and 'Sampler' using the "Ratio" character
.SamplingRatio_SamplerType <- left_join(Proprietary, Sampler, by = "Ratio")




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

.site.info <- read.csv("/Users/samantharumschlag/Documents/PowellCenter/OnlineMeetingSpring2021/PestCntyFunction/Biodata_Site_List.csv",
                      colClasses = c("StateFIPSCode" = "character",
                                     "CountyFIPSCode" = "character"))

.pest.info <- read.csv("/Users/samantharumschlag/Documents/PowellCenter/OnlineMeetingSpring2021/PestCntyFunction/pestclassPANall_22Feb21.csv") %>%
  dplyr::mutate(Name = stringr::str_to_lower(Name),
         class = Class,
         type = Type)

.clust_labels = read.csv("/Users/samantharumschlag/Documents/PowellCenter/Code/MahonRumschlagPowell/clust_labels.csv",
                        header=T, stringsAsFactors = FALSE)

usethis::use_data(.TaxLevCols_Algae, .TaxLevCols_Inverts,
                  .TaxLevCols_Fish, .SamplingRatio_SamplerType,
                  .ReorderUSGSBioDataColNames,
                  .site.info,
                  .pest.info,
                  .clust_labels,
                  internal = TRUE)
