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
tbr.ss <- read_csv("output/dataset2_spawner_surveys.csv") # Source most recent file

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
	
	# if(cu.info.df$TTC_Species[i] == "Chinook"){
	# 	ttc.df <- read_csv("data/1_raw-data/TTC_ManualExtract_Alsek_ChinookE7full.csv") %>%
	# 		filter(!is.na(Value))
	
	if(cu.info.df$PSE_source[i] == "CTC"){
		# Switch to using CTC data for Alsek Chinook
		ttc.df <- readxl::read_xlsx("data/1_raw-data/TCCHINOOK-24-01-Appendix-B-Escapement-Detailed.xlsx", sheet = "B2", range = "A4:B52", col_names = c("Year", "Value"), col_types = c("numeric", "numeric")) %>% 
			mutate(SPECIES = "Chinook", Stock = "Alsek River", Series = "Alsek River - Esc", TableSource = "B2") %>%
			select(SPECIES, Stock, Series, Year, Value, TableSource) %>%
			filter(!is.na(Value))
		
	} else {
		ttc.df <- read_csv(paste0("data/1_raw-data/TTC_ManualExtract_", cu.info.df$TTC_Stock[i], "_", cu.info.df$TTC_Species[i], ".csv"), col_types = c("c", "c", "c", "d", "d", "c")) %>%
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

cu_list <- read_csv("data/1_raw-data/conservationunits_decoder.csv") %>%
	filter(region == "Northern Transboundary")

# Note: We are not including estimated_spawners_plus (enhanced + wild) as a 
# separate series because in all cases this is equal to observed spawner abundance.
sa <- full_join(osa, esa, by = c("cuid", "year")) %>%
	left_join(cu_list %>%
						 select(species_abbr, cu_name_pse, cuid))  %>%
	mutate(region = "Transboundary") %>%
	select(region, species_abbr, cuid, cu_name_pse, year, observed_spawners, estimated_spawners) %>%
	arrange(region, species_abbr, cu_name_pse, year)

write_csv(sa, paste0("output/archive/dataset1_spawner_abundance_", Sys.Date(), ".csv"))
write_csv(sa, "output/dataset1_spawner_abundance.csv")
