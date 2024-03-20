###############################################################################
# This script reads in TTC data to compile catch, run size, and exploitation
# rate by Conservation Unit for the Pacific Salmon Explorer
###############################################################################

library(tidyverse)

# Note: Catch and run size are only available at the Conservation Unit (CU) scale 
# for Alsek Chinook. All other CUs are data deficicient at this time. 

# Read in complete Chinook run reconstruction (expanded version of TTC Appendix E.7)
ttc <- read_csv("data/1_raw-data/TTC_ManualExtract_Alsek_ChinookE7full.csv") %>%
	arrange(Stock, SPECIES, Series, Year)

# Set up PSE output dataframe
yrs <- sort(unique(ttc$Year))
ny <- length(yrs)

pse <- data.frame(
	region = rep("Transboundary", ny),
	species_abbr = rep("CK", ny),
	cuid = rep(1004, ny),
	year = yrs,
	spawners = ttc$Value[ttc$Series == "Alsek River - Escapement"],
	CDN_catch = ttc$Value[ttc$Series == "Alsek River - Canada harvest"],
	US_catch = ttc$Value[ttc$Series == "Alsek River - Harvest Dry Bay"]
)

# Calculate run size as spawners + harvest
pse$run_size <- apply(pse[, c("spawners", "CDN_catch", "US_catch")], 1, sum, na.rm = TRUE)

# Calculate exploitation rate as percent of total run size caught in fisheries
pse$total_exploitation_rate <- round(apply(pse[, c("CDN_catch", "US_catch")], 1, sum, na.rm = TRUE)/pse$run_size, 3)

# Write to csv
write_csv(pse, file = paste0("output/catch_and_run_size_datasets3+4_", Sys.Date(), ".csv"))
