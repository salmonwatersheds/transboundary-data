###############################################################################
# This script reads in spawner surveys and TTC data to calculate observed
# and estimated spawner abundance by Conservation Unit for the Pacific Salmon
# Explorer
###############################################################################

library(tidyverse)
library(sjmisc) # is_even and is_odd

###############################################################################
# Observed spawner abundance (osa)
###############################################################################

# Sum of spawner surveys (ss) by CU
ss.list <- sort(list.files( # Get list of all files
	path = "output", 
	pattern = "spawner_surveys_dataset_1part2_", 
	full.names = TRUE)) 

tbr.ss <- read_csv(tail(ss.list, 1)) # Source most recent file

# Some spawner surveys need to be removed to avoid double counting 

# 1) Nahlin River sonar
tbr.ss <- tbr.ss %>%
	filter(stream_name_pse != "Nahlin River Sonar")

# 2) Nesketahin Lake
tbr.ss <- tbr.ss %>%
	filter(stream_name_pse != "Nesketahin Lake")

# Sum observed spawner abundance (sa) within a CU by year

osa <- tbr.ss %>%
	group_by(cuid, year) %>%
	summarise(sum(stream_observed_count, na.rm = TRUE)) %>%
	rename(observed_spawners = `sum(stream_observed_count, na.rm = TRUE)`)

###############################################################################
# Estimated spawner abundance (esa)
###############################################################################

cu.info.df <- read_csv("data/0_lookup-files/TBR_PSF_CU_Abundance_LookupFile.csv")

# Loop through each CU and extract relevant data
for(i in 1:length(cu.info.df$cuid)){
	
	if( cu.info.df$TTC_Species[i] == "Chinook"){
		ttc.df <- read_csv("data/1_raw-data/TTC_ManualExtract_Alsek_ChinookE7full.csv") %>%
			filter(!is.na(Value))
		
	} else {
			ttc.df <- read_csv(paste0("data/1_raw-data/TTC_ManualExtract_", cu.info.df$TTC_Stock[i], "_", cu.info.df$TTC_Species[i], ".csv")) %>%
		filter(!is.na(Value))
		}
	
	esa.i <- full_join(ttc.df %>% 
		filter(Series == cu.info.df$estimated_spawners[i]) %>%
		select(Year, Value),
		ttc.df %>% 
			filter(Series == cu.info.df$`estimated_spawners_wild+enhanced`[i]) %>%
			select(Year, Value),
		by = "Year") %>%
		rename(year = "Year", estimated_spawners = "Value.x", estimated_spawners_plus = "Value.y") %>%
		mutate(cu.info.df[i, ] %>% select(cuid)) %>%
		select(cuid, year, estimated_spawners, estimated_spawners_plus)
		
		if(i == 1){
			esa <- esa.i
			} else {
				esa <- bind_rows(esa, esa.i)
			}
	
	rm(ttc.df, esa.i)
}

esa
###############################################################################
# Merge observed and estimated
###############################################################################

cu_list <- read_csv("data/1_raw-data/conservationunits_decoder.csv")

sa <- full_join(osa, esa, by = c("cuid", "year")) %>%
	left_join(cu_list %>%
						 select(species_abbr, cu_name_pse, cuid)) %>%
	select(species_abbr, cuid, cu_name_pse, year, observed_spawners, estimated_spawners, estimated_spawners_plus) %>%
	arrange(species_abbr, cu_name_pse, year)


write_csv(sa, paste0("output/spawner_abundance_dataset_1part1_", Sys.Date(), ".csv"))
		