##Join NRSA sampling datasets, so that we can just attach to package, instead of
##re-downloading and combining

nrsafsm08091314 <- read.table("C:/Users/mikem/Downloads/nrsa0809_1314_fishInfo_wide.tab",
         colClasses = c("UID" = "character"),sep = "\t", header = T)

nrsafsm1819 <- read.csv("C:/Users/mikem/Downloads/nrsa-1819-fish-sampling-information-data.csv",
         colClasses = c("UID" = "character"))


nrsafsm08091314[grepl(" ", nrsafsm08091314$RCH_LENGTH),"RCH_LENGTH"] = "120"

nrsafsm08091314$TEMPERATURE[which((is.na(as.numeric(ifelse(nrsafsm08091314$TEMPERATURE == "",
       NA,nrsafsm08091314$TEMPERATURE)))) != (is.na(ifelse(nrsafsm08091314$TEMPERATURE == "",
                  NA,nrsafsm08091314$TEMPERATURE))))] = c("12.2", "20.1")



hist(nrsafsm08091314$TEMPERATURE)

##I think this is good to go after this point (broadly)
nrsafsm <- bind_rows(nrsafsm08091314 %>%
  dplyr::select(-tidyselect::contains("_FLAG")) %>%
    mutate(CONDUCTIVITY = as.numeric(CONDUCTIVITY),
           PRIM_FSHTIME = as.numeric(PRIM_FSHTIME),
           PRIM_NET_DIAM = as.numeric(PRIM_NET_DIAM),
           PRIM_VOLTS = as.character(PRIM_VOLTS),
           RCH_LENGTH = as.numeric(RCH_LENGTH),
           TEMPERATURE = as.numeric(TEMPERATURE),
           SEC_VOLTS = as.character(SEC_VOLTS)
           ) %>%
    rename(STATE = PSTL_CODE),
  nrsafsm1819 %>%
    dplyr::select(-tidyselect::contains("_FLAG")) %>%
    mutate(SEC_PULSWID = as.numeric(SEC_PULSWID)))


##Do some cleaning of columns, etc.

##I think that all we actually need are Number of hauls, seconds/minutes, and reach length fished
##in addition to UID, gear (PRIM and SEC for each), METHOD, and SAMPLED_FISH

nrsafsampinfo <- nrsafsm %>%
  dplyr::select(UID, FISH_PROTOCOL, RCH_LENGTH, SAMPLED_FISH, METHOD, PRIM_GEAR, PRIM_FSHTIME, PRIM_SHKTIME,
                PRIM_SEINE, PRIM_HAULS, SEC_GEAR, SEC_SHKTIME,
                SEC_SEINE, SEC_HAULS) %>%
  mutate(PRIM_SHKTIME = PRIM_SHKTIME / 60,
         SEC_SHKTIME = SEC_SHKTIME / 60,
         MethodBasic = ifelse(METHOD != "" & !is.na(METHOD),
                          METHOD,
                          ifelse(PRIM_GEAR %in% c("BACKPACK","BARGE","RAFT","BOAT", "long line"),
                                 "Shocking",
                                 NA)),
         MinutesShockTime = rowSums(across(c("PRIM_SHKTIME","SEC_SHKTIME")), na.rm = T),
         NumberSeineHauls = rowSums(across(c("PRIM_HAULS","SEC_HAULS")), na.rm = T),
         MethodBasic = stringr::str_to_sentence(ifelse(MethodBasic %in% c("ELECTROFISH","MIXED","ELECT-MULT"),
                                                       "Shocking",
                                                       ifelse(is.na(RCH_LENGTH),
                                                              NA,
                                                              ifelse(grepl("SEINING",SAMPLED_FISH),
                                                                     "Seine",
                                                                     ifelse(MinutesShockTime != 0,
                                                                            "Shocking",
                                                                            ifelse(NumberSeineHauls != 0,
                                                                                   "Seine",
                                                                                   MethodBasic))))))) %>%
  dplyr::select(UID:SAMPLED_FISH, PRIM_FSHTIME,MethodBasic:NumberSeineHauls) %>%
  mutate(MinutesShockTime = ifelse(MethodBasic == "Shocking" & MinutesShockTime == 0,
                                   PRIM_FSHTIME,
                                   MinutesShockTime),
         NumberSeineHauls = ifelse(MethodBasic == "Seine" & NumberSeineHauls == 0,
                                   1,
                                   NumberSeineHauls))


##UID 12262, Number of Seine Hauls = 1; change MethodBasic to Seine
nrsafsampinfo[which(nrsafsampinfo$UID == 12262),"MethodBasic"] = "Seine"
nrsafsampinfo[which(nrsafsampinfo$UID == 12262),"NumberSeineHauls"] = 1

#uid 1002258, SHOCKTIME IS 85 MIN
nrsafsampinfo[which(nrsafsampinfo$UID == 1002258),"MinutesShockTime"] = 85

write.csv(nrsafsampinfo, "./inst/extdata/NRSA_Fish_SamplingInfo.csv", row.names = F)


NRSA_fish_sampleinfo <- nrsafsampinfo
