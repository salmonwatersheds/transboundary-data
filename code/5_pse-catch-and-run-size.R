###############################################################################
# This script reads in TTC data to compile catch, run size, and exploitation
# rate by Conservation Unit for the Pacific Salmon Explorer
###############################################################################

library(tidyverse)

# Note: Catch and run size are only available at the Conservation Unit (CU) scale 
# for Alsek Chinook AND TAHLTAN SEL. All other CUs are data deficicient at this time. 

###############################################################################
# Read in and compile TTC data
###############################################################################
ttc <- read_csv("data/2_clean-data/TTC_MERGED.csv") %>%
	arrange(Stock, SPECIES, Series, Year)

#------------------------------------------------------------------------------
# Alsek Chinook run reconstruction (expanded version of TTC Appendix E.7)
# Revised to use escapement from CTC (2024) that has MR estimates for 2022,2023
#------------------------------------------------------------------------------
ttc1 <- ttc %>%
	filter(TableSource == "E.7_full")

ctc <- readxl::read_xlsx("data/1_raw-data/TCCHINOOK-24-01-Appendix-B-Escapement-Detailed.xlsx", sheet = "B2", range = "A4:B52", col_names = c("Year", "Value"), col_types = c("numeric", "numeric")) %>% 
	mutate(SPECIES = "Chinook", Stock = "Alsek River", Series = "Alsek River - Esc", TableSource = "B2")

# Set up PSE output dataframe
yrs <- sort(unique(ttc1$Year))
ny <- length(yrs)

pse1 <- data.frame(
	region = rep("Transboundary", ny),
	species_abbr = rep("CK", ny),
	cuid = rep(1004, ny),
	year = yrs,
	spawners = ttc1$Value[ttc1$Series == "Alsek River - Escapement"],
	CDN_catch = ttc1$Value[ttc1$Series == "Alsek River - Canada harvest"],
	US_catch = ttc1$Value[ttc1$Series == "Alsek River - Harvest Dry Bay"]
)

# Calculate run size as spawners + harvest
pse1$run_size <- apply(pse1[, c("spawners", "CDN_catch", "US_catch")], 1, sum, na.rm = TRUE)

# Calculate exploitation rate as percent of total run size caught in fisheries
pse1$total_exploitation_rate <- round(apply(pse1[, c("CDN_catch", "US_catch")], 1, sum, na.rm = TRUE)/pse1$run_size, 3)

#------------------------------------------------------------------------------
# Tahltan SEL
#------------------------------------------------------------------------------
unique(ttc$Series[ttc$TableSource == "B.27"])
pse.series <- c(
	"Terminal Run - Tahltan Wild",
	"Escapement - Tahltan Wild",
	"US Terminal Harvest - Tahltan Wild",
	"Cdn Terminal Harvest - Tahltan Wild"
)

ttc2 <- ttc %>%
	filter(
		ttc$TableSource == "B.27",
		Series %in% pse.series)

# Visual checks - Total run = sum of catch and escapement
# plot(ttc2$Year[ttc2$Series == pse.series[1]], ttc2$Value[ttc2$Series == pse.series[1]], lwd = 3, col = grey(0.8), "l", ylim = range(ttc2$Value))
# for(i in 1:3){
# 	lines(ttc2$Year[ttc2$Series ==pse.series[i+1]], ttc2$Value[ttc2$Series == pse.series[i+1]], col = c(1,4,2)[i])
# }
# lines(ttc2$Year[ttc2$Series ==pse.series[i+1]], tapply(ttc2$Value[ttc2$Series %in% pse.series[2:4]], ttc2$Year[ttc2$Series %in% pse.series[2:4]], sum), lty = 2)

# Set up PSE output dataframe
yrs <- sort(unique(ttc2$Year))
ny <- length(yrs)

pse2 <- data.frame(
	region = rep("Transboundary", ny),
	species_abbr = rep("SEL", ny),
	cuid = rep(1026, ny),
	year = yrs,
	spawners = ttc2$Value[ttc2$Series == "Escapement - Tahltan Wild"],
	CDN_catch = ttc2$Value[ttc2$Series == "Cdn Terminal Harvest - Tahltan Wild"],
	US_catch = ttc2$Value[ttc2$Series == "US Terminal Harvest - Tahltan Wild"],
	run_size = ttc2$Value[ttc2$Series == "Terminal Run - Tahltan Wild"]
)

# Check: run size as spawners + harvest
if(sum(pse2$run_size - apply(pse2[, c("spawners", "CDN_catch", "US_catch")], 1, sum, na.rm = TRUE)) != 0){
	warning("Run size != (spawners + CDN_catch + US_catch)")
} # Off by 1 or -1; OK rounding errors

# Calculate exploitation rate as percent of total run size caught in fisheries
pse2$total_exploitation_rate <- round(apply(pse2[, c("CDN_catch", "US_catch")], 1, sum, na.rm = TRUE)/pse2$run_size, 3)
###############################################################################
# Write to csv
###############################################################################
pse <- rbind(pse1, pse2)

write_csv(pse, file = paste0("output/catch_and_run_size_datasets3+4_", Sys.Date(), ".csv"))
