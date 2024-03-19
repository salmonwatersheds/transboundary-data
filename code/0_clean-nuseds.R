# This script implements the following steps
# - extract nuSEDS from *.xlsx to *.csv
# - filter out the transboundary CU data
# - select the key variables
# - store the cleaned raw nuseds extract
# - recode the estimate quality to H/M/L/UNK
# - filter out records without estimates
# - summarize the records by FAZ and by CU

library(tidyverse)
library(readxl)



# Read in nuseds xls and cache it in csv.
# big file, takes a while
nuseds.raw <- read_excel("DATA_PROCESSING/DATA/1_RawData/nuSEDS_RAW/NuSEDS_data_20240227.xlsx",
                         sheet = "Export Worksheet")

nuseds.raw <- nuseds.raw %>% mutate(Species_CU_INDEX = paste0(SPECIES_QUALIFIED,"-",CU_INDEX ))

nuseds.raw$Species_CU_INDEX <- gsub("L-L","L",nuseds.raw$Species_CU_INDEX)
nuseds.raw$Species_CU_INDEX <- gsub("R-R","R",nuseds.raw$Species_CU_INDEX)


dim(nuseds.raw)
names(nuseds.raw)

sort(unique(nuseds.raw$AREA))
head(sort(unique(nuseds.raw$WATERBODY)))



cu.info.df <- read_csv("DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_CU_Info.csv")


#################################################################
# DATA CLEANUP
#################################################################

names(nuseds.raw)
vars.keep <- c("CU_NAME", "Species_CU_INDEX", "DESCR", "GEOGRAPHICAL_EXTNT_OF_ESTIMATE",
               "SPECIES","ANALYSIS_YR","INDEX_YN","ESTIMATE_STAGE" ,"ESTIMATE_CLASSIFICATION",
               "NO_INSPECTIONS_USED", "ESTIMATE_METHOD","POP_ID", "MAX_ESTIMATE","NATURAL_ADULT_SPAWNERS","NATURAL_JACK_SPAWNERS", "NATURAL_SPAWNERS_TOTAL", "ADULT_BROODSTOCK_REMOVALS" ,
               "JACK_BROODSTOCK_REMOVALS"  , "TOTAL_BROODSTOCK_REMOVALS" ,"OTHER_REMOVALS" , "TOTAL_RETURN_TO_RIVER" , "UNSPECIFIED_RETURNS" , "ENUMERATION_METHOD1" ,  "ENUMERATION_METHOD2"  ,
                "ENUMERATION_METHOD3","ENUMERATION_METHOD4" ,"ENUMERATION_METHOD5" ,"ENUMERATION_METHOD6")


nuseds.clean <- nuseds.raw %>% select(all_of(vars.keep)) %>%
                dplyr::filter(Species_CU_INDEX %in% cu.info.df$Species_CU_INDEX,CU_NAME %in% cu.info.df$CU_NAME ) %>%
                  dplyr::rename(Year = ANALYSIS_YR )
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



# check before
nuseds.clean %>% dplyr::filter(SPECIES == "Coho", POP_ID == 45152) %>% arrange(Year)
nuseds.clean %>% dplyr::filter(SPECIES == "Coho", POP_ID == 45154) %>% arrange(Year)

fix.idx <- nuseds.tmp$SPECIES == "Coho" & nuseds.tmp$POP_ID == 45152 & nuseds.tmp$Year <= 1994 # use these as replacement
drop.idx <- nuseds.tmp$SPECIES == "Coho" & nuseds.tmp$POP_ID == 45154 & nuseds.tmp$Year <= 1994 # delete these old ones after
repl.info.src <- nuseds.tmp %>% dplyr::filter(SPECIES == "Coho", POP_ID == 45154, Year == 1991) # use this as the source of site info
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

# clear out any remaining records of 45152
nuseds.tmp <- nuseds.tmp %>% dplyr::filter(POP_ID !=45152)


nuseds.tmp %>% dplyr::filter(SPECIES == "Coho", POP_ID == 45152) %>% arrange(Year)
nuseds.tmp %>% dplyr::filter(SPECIES == "Coho", POP_ID == 45154) %>% arrange(Year)

dim(nuseds.clean)
dim(nuseds.tmp)


# 45153 	TATSATUA RIVER	SER
# any records of 45151 for 1994 or earlier get changed to 45153
# after that any


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




write_csv(nuseds.clean,"DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_Clean.csv")





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


write_csv(nuseds.summary.by.cu,"DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_Summary_byCU.csv")


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

nuseds.summary.by.faz <- read_csv("DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_CUNumberbyFAZ.csv") %>%
                          left_join(tmp.faz.summary %>% select(River, FAZ,SitesWithData,NumRecords),by=c("River","FAZ"))
nuseds.summary.by.faz


write_csv(nuseds.summary.by.faz,"DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_Summary_byFAZ.csv")




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

write_csv(nuseds.summary.by.site,"DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_Summary_bySite.csv")





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

write_csv(nuseds.sum.cu.yr,"DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_AnnualSumOfMaxEst.csv")













