###############################################################################
# This script implements the following steps
# - extract nuSEDS from *.xlsx to *.csv
# - filter out the transboundary CU data
# - select the key variables
# - store the cleaned raw nuseds extract
# - recode the estimate quality to H/M/L/UNK
# - filter out records without estimates
# - summarize the records by FAZ and by CU
###############################################################################

library(tidyverse)
library(readxl)



# Read in nuseds xls and cache it in csv.
# big file, takes a while
# nuseds.raw <- read_excel("DATA_PROCESSING/DATA/1_RawData/nuSEDS_RAW/NuSEDS_data_20240227.xlsx",
                         # sheet = "Export Worksheet")

# 2025 update: Use puhblicly available Yukon and Transboundary file
# https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/4bb25b37-3324-3828-af4a-aff4bfc5c8c0
nuseds.raw <- read_csv("data/1_raw-data/Yukon and Transboundary NuSEDS_20250730.csv")
sort(names(nuseds.raw))

cu.info.df <- read_csv("data/2_clean-data/TBR_nuSEDS_CU_Info.csv")
names(cu.info.df)

site.info.df <- read_csv("data/0_lookup-files/TBR_PSF_CU_Site_LookupFile.csv")

nuseds <- nuseds.raw %>% left_join(site.info.df) 

#################################################################
# DATA CLEANUP
#################################################################


sort(names(nuseds))

# IS_INDICATOR versus INDEX_YN
print(nuseds %>% select(IS_INDICATOR, INDEX_YN), n = 100) # different, which to use?
# use IS_INDICATOR for now, seems more complete

# Calculate MAX_ESTIMATE
# Define variables to include in MAX_ESTIMATE
var_in_MAX_ESTIMATE <- c("NATURAL_ADULT_SPAWNERS", "NATURAL_JACK_SPAWNERS", "NATURAL_SPAWNERS_TOTAL", "ADULT_BROODSTOCK_REMOVALS", "JACK_BROODSTOCK_REMOVALS", "TOTAL_BROODSTOCK_REMOVALS", "OTHER_REMOVALS", "TOTAL_RETURN_TO_RIVER")

# Check that all variables are in the dataset
var_in_MAX_ESTIMATE %in% names(nuseds)

# Calculate MAX_ESTIMATE
nuseds$MAX_ESTIMATE <- apply(nuseds[, var_in_MAX_ESTIMATE], 1, max, na.rm = TRUE)
nuseds$MAX_ESTIMATE[nuseds$MAX_ESTIMATE == -Inf] <- NA

vars.keep <- c("cu_name_pse", "FULL_CU_IN", "cuid", "stream_name_pse", "streamid", "SPECIES", "SPECIES_QUALIFIED", "ANALYSIS_YR","IS_INDICATOR","ESTIMATE_STAGE","ESTIMATE_CLASSIFICATION", "NO_INSPECTIONS_USED", "ESTIMATE_METHOD","POP_ID", "MAX_ESTIMATE","NATURAL_ADULT_SPAWNERS","NATURAL_JACK_SPAWNERS", "NATURAL_SPAWNERS_TOTAL", "ADULT_BROODSTOCK_REMOVALS" , "JACK_BROODSTOCK_REMOVALS"  , "TOTAL_BROODSTOCK_REMOVALS" ,"OTHER_REMOVALS" , "TOTAL_RETURN_TO_RIVER" , "ENUMERATION_METHODS")


nuseds.clean <- nuseds %>% select(all_of(vars.keep)) %>%
                dplyr::filter(FULL_CU_IN %in% cu.info.df$Species_CU_INDEX) %>%
                  dplyr::rename(Year = ANALYSIS_YR)
dim(nuseds.raw)
dim(nuseds.clean)
head(nuseds.clean)

unique(nuseds.clean$ESTIMATE_STAGE)
unique(nuseds.clean$ESTIMATE_CLASSIFICATION)


# Estimate Quality Categories


nuseds.clean <- nuseds.clean %>% mutate(EstQual = recode(ESTIMATE_CLASSIFICATION,
                                                         "TRUE ABUNDANCE (TYPE-1)" = "H",
                                                         "TRUE ABUNDANCE (TYPE-2)" = "H",
                                                         "UNKNOWN" = "UNK",
                                                         "RELATIVE ABUNDANCE (TYPE-3)" = "M",
                                                         "RELATIVE ABUNDANCE (TYPE-4)" = "M",
                                                         "RELATIVE ABUNDANCE (TYPE-5)" = "L",
                                                         "PRESENCE-ABSENCE (TYPE-6)"  = "L",
                                                         "PRESENCE/ABSENCE (TYPE-6)" = "L",
                                                         "NO SURVEY THIS YEAR"  = "NA",
                                                         "RELATIVE: CONSTANT MULTI-YEAR METHODS" = "UNK",
                                                         "RELATIVE: VARYING MULTI-YEAR METHODS"  = "UNK",
                                                          "NO SURVEY" = "NA")) %>%
                  dplyr::filter(!is.na(MAX_ESTIMATE)) # CHECK THIS: keep only records that have an actual number



table(nuseds.clean$EstQual)




#################################################################
# HARDWIRED FIXES TO DEAL WITH POP_ID INCONSISTENCIES/CHANGES

nuseds.tmp <- nuseds.clean

# 45154 	TATSATUA RIVER	CO
# any records of 45152 (TATSAMENIE RIVER) for 1994 or earlier get changed to 45154
# after that, all records of 45152 get removed
# NO DATA AFTER 1991?

nuseds.tmp %>% filter(nuseds.tmp$POP_ID %in% c(45152, 45154)) %>% data.frame()

# Remove 54154 before 1994
nuseds.tmp <- nuseds.tmp[-which(nuseds.tmp$Year <= 1994 & nuseds.tmp$POP_ID == 45154),]

# Change POP_ID
nuseds.tmp$POP_ID[which(nuseds.tmp$Year <= 1994 & nuseds.tmp$POP_ID == 45152)] <- 45154

# Remove all 45154
nuseds.tmp <- nuseds.tmp[-which(nuseds.tmp$POP_ID == 45152),]

dim(nuseds.clean)
dim(nuseds.tmp)


# 45153 	TATSATUA RIVER	SER
# any records of 45151 for 1994 or earlier get changed to 45153
# after that any

nuseds.tmp %>% filter(POP_ID %in% c(45151)) %>% arrange(Year) %>% select(Year, MAX_ESTIMATE) %>% data.frame()

nuseds.tmp %>% filter(POP_ID %in% c(45153)) %>% arrange(Year) %>% select(Year, MAX_ESTIMATE) %>% data.frame()

# check before
nuseds.clean %>% dplyr::filter(SPECIES == "Sockeye", POP_ID == 45153) %>% arrange(Year)
nuseds.clean %>% dplyr::filter(SPECIES == "Sockeye", POP_ID == 45151) %>% arrange(Year)

fix.idx <- nuseds.tmp$SPECIES == "Sockeye" & nuseds.tmp$POP_ID == 45151 & nuseds.tmp$Year <= 1994 # use these as replacement
drop.idx <- nuseds.tmp$SPECIES == "Sockeye" & nuseds.tmp$POP_ID == 45153 & nuseds.tmp$Year <= 1994 # delete these old ones after
repl.info.src <- nuseds.tmp %>% dplyr::filter(SPECIES == "Sockeye", POP_ID == 45153, Year == 1991) # use this as the source of site info
repl.info.src
names(nuseds.tmp)
nuseds.tmp[fix.idx,] %>% arrange(Year)

nuseds.tmp[fix.idx,"POP_ID"] <- repl.info.src$POP_ID
nuseds.tmp[fix.idx,"CU_NAME"] <- repl.info.src$CU_NAME
nuseds.tmp[fix.idx,"Species_CU_INDEX"] <- repl.info.src$Species_CU_INDEX
nuseds.tmp[fix.idx,"GEOGRAPHICAL_EXTNT_OF_ESTIMATE"] <- repl.info.src$GEOGRAPHICAL_EXTNT_OF_ESTIMATE
nuseds.tmp[fix.idx,"DESCR"] <- repl.info.src$DESCR

nuseds.tmp[fix.idx,] %>% arrange(Year)

nuseds.tmp <- nuseds.tmp[!drop.idx,]

# clear out any remaining records of 45151
nuseds.tmp <- nuseds.tmp %>% dplyr::filter(POP_ID !=45151)


nuseds.tmp %>% dplyr::filter(SPECIES == "Sockeye", POP_ID == 45153) %>% arrange(Year)
nuseds.tmp %>% dplyr::filter(SPECIES == "Sockeye", POP_ID == 45151) %>% arrange(Year)

dim(nuseds.clean)
dim(nuseds.tmp)



#  45164 NAHLIN CHINOOK
# There is one record with popID 45165 -> change this to 45164
tmp <- nuseds.tmp %>% dplyr::filter(SPECIES == "Chinook", POP_ID == 45164) %>% arrange(Year) %>% select(Year, MAX_ESTIMATE)
view(tmp) # -> 1999 missing
nuseds.tmp %>% dplyr::filter(SPECIES == "Chinook", POP_ID == 45165) %>% arrange(Year) %>% select(Year, MAX_ESTIMATE)
# -> have 1 record for 1999

fix.idx <- nuseds.tmp$SPECIES == "Chinook" & nuseds.tmp$POP_ID == 45165  # change this record

repl.info.src <- nuseds.tmp %>% dplyr::filter(SPECIES == "Chinook", POP_ID == 45164, Year == 2000) # use this as the source of site info
repl.info.src
names(nuseds.tmp)
nuseds.tmp[fix.idx,] %>% arrange(Year)

nuseds.tmp[fix.idx,"POP_ID"] <- repl.info.src$POP_ID
nuseds.tmp[fix.idx,"CU_NAME"] <- repl.info.src$CU_NAME
nuseds.tmp[fix.idx,"Species_CU_INDEX"] <- repl.info.src$Species_CU_INDEX
nuseds.tmp[fix.idx,"GEOGRAPHICAL_EXTNT_OF_ESTIMATE"] <- repl.info.src$GEOGRAPHICAL_EXTNT_OF_ESTIMATE
nuseds.tmp[fix.idx,"DESCR"] <- repl.info.src$DESCR

nuseds.tmp[fix.idx,] %>% arrange(Year)







# after checking, overwrite the main file
nuseds.clean <- nuseds.tmp




write_csv(nuseds.clean,"data/2_clean-data/TBR_nuSEDS_Clean.csv")





# Create Summaries



nuseds.cu.check <- sort(unique(nuseds.clean$Species_CU_INDEX))
length(nuseds.cu.check)
nuseds.cu.check

cu.info.df$Species_CU_INDEX[duplicated(cu.info.df$Species_CU_INDEX)]

length(unique(cu.info.df$Species_CU_INDEX))
length(cu.info.df$Species_CU_INDEX)

length(setdiff(cu.info.df$Species_CU_INDEX,nuseds.cu.check))
length(setdiff(nuseds.cu.check,cu.info.df$Species_CU_INDEX))




nuseds.summary.by.cu <- cu.info.df %>% select(River,FAZ,SPECIES,PSF_CU_NAME,CU_NAME, Species_CU_INDEX) %>%
  left_join(

nuseds.clean %>% group_by(Species_CU_INDEX) %>%
  summarize(Sites = length(unique(POP_ID)),Records_Total = n(), Max_Est = max(MAX_ESTIMATE,na.rm=TRUE),
            FirstRecord = min(Year), LastRecord = max(Year) ,
            Records_HQ = sum(EstQual == "H"),Records_MQ = sum(EstQual == "M"),Records_LQ = sum(EstQual == "L"),Records_UNK = sum(EstQual == "UNK")
)
,by = "Species_CU_INDEX"
) %>% arrange(River,FAZ)  %>% unique()


write_csv(nuseds.summary.by.cu,"data/2_clean-data/TBR_nuSEDS_Summary_byCU.csv")


table(nuseds.summary.by.cu$FAZ)
sum(table(nuseds.summary.by.cu$FAZ))

# NEED TO FIX: NOT INCLUDING UNMATCHED CU YETNCLU


sum(nuseds.summary.by.cu$Records_Total,na.rm=TRUE)
sum(nuseds.summary.by.cu$Sites   ,na.rm=TRUE)

unique(nuseds.summary.by.cu$Sites)


tmp.faz.summary <- nuseds.summary.by.cu %>% group_by(River,FAZ) %>%
  summarize(Species = length(unique(SPECIES)),
            CU = length(unique(CU_NAME)),
            SitesWithData = sum(Sites,na.rm=TRUE),
            NumRecords = sum(Records_Total,na.rm=TRUE)
            )
tmp.faz.summary

sum(tmp.faz.summary$CU)
sum(tmp.faz.summary$SitesWithData)

nuseds.summary.by.faz <- read_csv("data/2_clean-data/TBR_nuSEDS_CUNumberbyFAZ.csv") %>%
                          left_join(tmp.faz.summary %>% select(River, FAZ,SitesWithData,NumRecords),by=c("River","FAZ"))
nuseds.summary.by.faz


write_csv(nuseds.summary.by.faz,"data/2_clean-data/TBR_nuSEDS_Summary_byFAZ.csv")




rm(nuseds.raw)


#############################

# nuSEDS summary by site

nuseds.summary.by.site <-
    nuseds.clean %>% group_by(Species_CU_INDEX, POP_ID, GEOGRAPHICAL_EXTNT_OF_ESTIMATE) %>%
      summarize(Records_Total = n(), Max_Est = max(MAX_ESTIMATE,na.rm=TRUE),
                FirstRecord = min(Year), LastRecord = max(Year) ,
                Records_HQ = sum(EstQual == "H"),Records_MQ = sum(EstQual == "M"),Records_LQ = sum(EstQual == "L"),Records_UNK = sum(EstQual == "UNK")
      ) %>%
    left_join(cu.info.df %>% select(River,FAZ,SPECIES,PSF_CU_NAME,CU_NAME, Species_CU_INDEX)
    ,by = "Species_CU_INDEX"
  ) %>% arrange(River,FAZ,CU_NAME,POP_ID)


nuseds.summary.by.site

write_csv(nuseds.summary.by.site,"data/2_clean-data/TBR_nuSEDS_Summary_bySite.csv")





####################
# Annual Sum of Site Records by CU
names(nuseds.clean)

table(nuseds.clean$EstQual)






nuseds.sum.cu.yr <- nuseds.summary.by.cu <- cu.info.df %>% select(River,FAZ,SPECIES,PSF_CU_NAME,CU_NAME, Species_CU_INDEX) %>%
  left_join(

    left_join(
      # all sites
      nuseds.clean %>% group_by(Species_CU_INDEX,Year) %>%
        summarize(SumMaxEstAll = sum(MAX_ESTIMATE,na.rm=TRUE), NumSitesAll = n()),
      # HQ sites
      nuseds.clean %>% dplyr::filter(EstQual == "H") %>% group_by(Species_CU_INDEX,Year) %>%
        summarize(SumMaxEstH = sum(MAX_ESTIMATE,na.rm=TRUE), NumSitesH = n()),
      by = c("Species_CU_INDEX","Year")) %>%
      # HQ & MQ Sites
      left_join(nuseds.clean %>% dplyr::filter(EstQual %in% c("H","M")) %>% group_by(Species_CU_INDEX,Year) %>%
                  summarize(SumMaxEstHM = sum(MAX_ESTIMATE,na.rm=TRUE), NumSitesHM = n()
                  ), by = c("Species_CU_INDEX","Year")) %>%
    left_join(nuseds.clean %>% dplyr::filter(EstQual %in% c("L")) %>% group_by(Species_CU_INDEX,Year) %>%
                summarize(SumMaxEstL = sum(MAX_ESTIMATE,na.rm=TRUE), NumSitesL = n()
                ), by = c("Species_CU_INDEX","Year")) %>%
      left_join(nuseds.clean %>% dplyr::filter(EstQual %in% c("UNK")) %>% group_by(Species_CU_INDEX,Year) %>%
                  summarize(SumMaxEstUNK = sum(MAX_ESTIMATE,na.rm=TRUE), NumSitesUNK = n()
                  ), by = c("Species_CU_INDEX","Year"))


    ,by = "Species_CU_INDEX"
  ) %>% arrange(River,FAZ,PSF_CU_NAME,Year) %>% rowwise() %>%
  mutate(PercLUNK =  round(sum(SumMaxEstL,SumMaxEstUNK,na.rm=TRUE) / SumMaxEstAll *100,1))


head(nuseds.sum.cu.yr)

write_csv(nuseds.sum.cu.yr,"data/2_clean-data/TBR_nuSEDS_AnnualSumOfMaxEst.csv")

