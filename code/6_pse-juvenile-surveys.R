###############################################################################
# This script reads in TTC data to compile juvenile survey data (dataset 88) 
# by Conservation Unit for the Pacific Salmon Explorer
###############################################################################

library(tidyverse)

# #---#
# # Look at current dataset88 in database
# pop_root <- "~/Salmon Watersheds Dropbox/Stephanie Peacock/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/"
# 
# # Source functions for database integration
# source(paste0(pop_root, "code/functions_general.R"))
# 
# dat88_old <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset88_output") %>%
# 	filter(region == "Transboundary")
# 	
#------------------------------------------------------------------------------
# Grab TTC juvenile survey data (Tahltan SEL)
#------------------------------------------------------------------------------

# Read in lookup table pointing to source datasets
juv_src <- read_csv("data/0_lookup-files/TBR_PSF_JuvenileSurvey_LookupFile.csv")

# Read in Stikine sockeye TTC tables
ttc <- read_csv("data/2_clean-data/TTC_MERGED.csv") %>%
	arrange(Stock, SPECIES, Series, Year)

unique(ttc$Series[grep("Smolt", ttc$Series)])
unique(ttc$TableSource[grep("Smolt", ttc$Series)])

# Specify series to include
ttc.series <- unique(juv_src$TTC_RECORD_MATCH, na.rm = TRUE)
ttc.series <- ttc.series[!is.na(ttc.series)]
ttc.series

# Set up PSE output dataframe for dataset 88 (juvenile surveys)
dat88_new <- juv_src %>%
	filter(!is.na(TTC_RECORD_MATCH)) %>%
	rename(Series = "TTC_RECORD_MATCH") %>%
	left_join(ttc %>% select(Year, Series, Value)) %>%
	rename(absolute_abundance = Value, year = Year) %>%
	select(region, cuid, cu_name_pse, species_name, locationid, locationname, pointid, latitude, longitude, year,absolute_abundance, enumeration_method)


# Join old data from other sources not TTC
dat88 <- rbind(
	dat88_new,
	dat88_old %>% 
		select(names(dat88_new)) %>% 
		filter(locationid %in% dat88_new$locationid == FALSE)
)

# Rename Tatsamenie sites
dat88$locationname[dat88$locationname == "Mark Recapture - Natural Origin"] <- "Tatsamenie (natural)"
dat88$locationname[dat88$locationname == "Mark Recapture - Hatchery Origin"] <- "Tatsamenie (hatchery)"

# Write to csv
write_csv(dat88, file = paste0("output/juvenile_surveys_dataset88_", Sys.Date(), ".csv"))
