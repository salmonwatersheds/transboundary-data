###############################################################################
# This script implements the following steps

# extract CU list, compare to PSF's main CU list
# extract CU data
###############################################################################

library(tidyverse)

# Main CU lookup
psf.cu.lookup <- read_csv("data/0_lookup-files/TBR_PSF_CU_Info_LookupFile.csv")
psf.site.lookup <- read_csv("data/0_lookup-files/TBR_PSF_CU_Site_LookupFile.csv")
tbr.cu.list <- psf.cu.lookup$Species_CU_INDEX
tbr.cu.list
length(tbr.cu.list)

# nuSEDS CU list
cu.sites.raw <- read.csv("data/1_raw-data/conservation_unit_system_sites.csv", header=TRUE) %>% 
  mutate(Species_CU_INDEX = paste0(SPECIES_QUALIFIED,"-",CU_INDEX))

cu.sites.raw$Species_CU_INDEX <- gsub("L-L","L",cu.sites.raw$Species_CU_INDEX)
cu.sites.raw$Species_CU_INDEX <- gsub("R-R","R",cu.sites.raw$Species_CU_INDEX)

head(cu.sites.raw)
names(cu.sites.raw)
sort(unique(cu.sites.raw$FAZ_ACRO))
sort(unique(cu.sites.raw$Species_CU_INDEX))


# check for matches
length(tbr.cu.list  )
match.cu <- intersect(tbr.cu.list ,unique(cu.sites.raw$Species_CU_INDEX))
match.cu
length(intersect(tbr.cu.list ,unique(cu.sites.raw$Species_CU_INDEX)))


cu.sites.tbr <- cu.sites.raw %>% dplyr::filter(Species_CU_INDEX %in% tbr.cu.list)
dim(cu.sites.tbr)

sort(cu.sites.tbr$SYSTEM_SITE)

head(cu.sites.tbr[,1:15])

write_csv(cu.sites.tbr,"data/2_clean-data/TBR_nuSEDS_CU_SiteLookup.csv")

df.part1 <- psf.cu.lookup %>% select(River,FAZ,FAZ_ACRO,CU_NAME,SPECIES, Species_CU_INDEX) %>% dplyr::rename(PSF_CU_NAME = CU_NAME)
dim(df.part1)

df.part2 <- cu.sites.tbr %>%
  select(CU_NAME,CU_ACRO,MAZ_ACRO,CU_TYPE,Species_CU_INDEX, SPECIES_QUALIFIED) %>% unique()  %>%
  dplyr::rename(SPECIES_nuSEDS = SPECIES_QUALIFIED)
dim(df.part2)

length(setdiff(df.part1$Species_CU_INDEX,df.part2$Species_CU_INDEX))
length(setdiff(df.part2$Species_CU_INDEX,df.part1$Species_CU_INDEX))


cu.info.df <-
  left_join(df.part1,  df.part2 ,
  by = "Species_CU_INDEX") %>% arrange(River, FAZ, SPECIES)


sort(df.part1$Species_CU_INDEX)
sort(df.part2$Species_CU_INDEX)
cu.info.df

write_csv(cu.info.df ,"data/2_clean-data/TBR_nuSEDS_CU_Info.csv")




faz.summary.df <- left_join(

  cu.info.df %>% group_by(River,FAZ,FAZ_ACRO) %>%
  summarize(Species = length(unique(SPECIES)),
            CU = length(unique(PSF_CU_NAME)) ),

  cu.sites.tbr %>% group_by(FAZ_ACRO) %>%
  summarize(Sites_nuSEDS = n()),

  by = "FAZ_ACRO")



faz.summary.df


write_csv(faz.summary.df,"data/2_clean-data/TBR_nuSEDS_CUNumberbyFAZ.csv")








