library(tidyverse)
library(sjmisc) # is_even and is_odd

# read in cleaned data
site.info.df <- read_csv("DATA_PROCESSING/DATA/0_LookupFiles/TBR_PSF_CU_Site_LookupFile_revised.csv")
nuseds.src <- read_csv("DATA_PROCESSING/DATA/2_ExtractedData/TBR_nuSEDS_Clean.csv") %>%
                mutate(POP_ID = as.numeric(POP_ID))
ttc.src <- read_csv("DATA_PROCESSING/DATA/2_ExtractedData/TTC_MERGED.csv")

# Bring in two additional sources of data : Stonehouse and Nahlin river Sonar
# Arrange by year
pahlke.src <- read_csv("DATA_PROCESSING/DATA/1_RawData/Pahlke_ChinookStonehouseChilkat.csv") %>%
	arrange(Year)

nahlin.src <- read_csv("DATA_PROCESSING/DATA/1_RawData/nahlin_river_sonar.csv") %>%
	rename(Year = "year") %>%
	arrange(Year)

years.include <- 1920:2023

# generate template for flat file (all combos of year and pop ID)
# streamid is the unique identifier for each spawner survey stream; not POP_ID
combined.data.df <- expand.grid(site.info.df$streamid, years.include) %>%
                    dplyr::rename(streamid = "Var1",Year = "Var2") %>%
                    arrange(streamid)
head(combined.data.df )

str(combined.data.df)

# add in CU info
names(site.info.df)

combined.data.df <- combined.data.df %>% left_join(site.info.df %>%
                      select(SPECIES_QUALIFIED,cu_name_pse,cuid,stream_name_pse,streamid,PSE_source,TTC_Species, TTC_STOCK_MATCH,TTC_RECORD_MATCH,TTC_METHOD, TTC_QUALITY, POP_ID, IS_INDICATOR, latitude, longitude),
                      by = "streamid")

head(combined.data.df)


# merge in nuseds data **for those streams that have it**
combined.data.df  <- combined.data.df %>% left_join(nuseds.src %>% select(POP_ID,Year,MAX_ESTIMATE, ESTIMATE_METHOD,ESTIMATE_CLASSIFICATION), by=c("POP_ID","Year")) %>%
                        dplyr::rename(Spn_nuSEDS = "MAX_ESTIMATE")

head(combined.data.df )


# ** Remove even/odd data that aren't applicable for pink CUs at the end *


# merge in TTC data
ttc.src

combined.data.df <- combined.data.df %>% left_join(ttc.src %>%
                            select(SPECIES,Stock,Series,Year, Value) %>%
                              dplyr::rename(Spn_TTC = "Value",TTC_Species = "SPECIES", TTC_STOCK_MATCH = "Stock",TTC_RECORD_MATCH = "Series"),
                            by=c("TTC_Species", "TTC_STOCK_MATCH", "TTC_RECORD_MATCH", "Year"))


head(combined.data.df )

# Method changed for Village Creek counter from resistor to video
combined.data.df$TTC_METHOD[combined.data.df$TTC_RECORD_MATCH == "Village Creek Counter" & combined.data.df$Year <= 2013 & !is.na(combined.data.df$Spn_TTC)] <- "Conductivity counter"
combined.data.df$TTC_METHOD[combined.data.df$TTC_RECORD_MATCH == "Village Creek Counter" & combined.data.df$Year > 2013 & !is.na(combined.data.df$Spn_TTC)] <- "Video counter"

# Method for Alsek SER Spawning Escapement changed
combined.data.df$TTC_METHOD[combined.data.df$TTC_RECORD_MATCH == "Spawning Escapement" & combined.data.df$SPECIES_QUALIFIED == "SER" & combined.data.df$Year <= 2004 & !is.na(combined.data.df$Spn_TTC)] <- "Mark-recapture"
combined.data.df$TTC_METHOD[combined.data.df$TTC_RECORD_MATCH == "Spawning Escapement" & combined.data.df$SPECIES_QUALIFIED == "SER" & combined.data.df$Year > 2004 & !is.na(combined.data.df$Spn_TTC)] <- "GSI + Fence expansion"

# calculate difference diagnostics
combined.data.df <- combined.data.df %>%
                        mutate(Diff = Spn_TTC - Spn_nuSEDS) %>%
                        mutate(PercDiff = round(100* Diff/Spn_TTC,2))

head(combined.data.df %>%filter(!is.na(PercDiff)))

head(combined.data.df %>% filter(Year > 2020))

# generate combined series (approach changed March 2024)
# - use TTC if there are ANY TTC data
# - use NuSEDS if no TTC data
combined.data.df$Spn_Combined <- NA
combined.data.df$Spn_Combined[combined.data.df$PSE_source == "TTC"] <- combined.data.df$Spn_TTC[combined.data.df$PSE_source == "TTC"]
combined.data.df$Spn_Combined[combined.data.df$PSE_source == "NuSEDS"] <- combined.data.df$Spn_nuSEDS[combined.data.df$PSE_source == "NuSEDS"]

head(combined.data.df %>% filter(!is.na(Spn_Combined)))

#------------------------------------------------------------------------------
# PSE additions/changes; made to Spn_Combined
#------------------------------------------------------------------------------

# Merge in Stonehouse data (One Chinook stream from Pahlke (1992))
# ENsure both ordered by increasing year - check
combined.data.df[which(combined.data.df$stream_name_pse == "STONEHOUSE" & combined.data.df$Year %in% pahlke.src$Year), "Spn_Combined"] <- pahlke.src$Value

# Merge in Nahlin Sonar data
combined.data.df[which(combined.data.df$stream_name_pse == "NAHLIN RIVER SONAR" & combined.data.df$Year %in% nahlin.src$Year), "Spn_Combined"] <- nahlin.src$Value

# Remove pink even/odd data that aren't applicable
combined.data.df[which(combined.data.df$SPECIES_QUALIFIED == "PKO" & is_even(combined.data.df$Year)), c("Spn_Combined", "Spn_nuSEDS", "Spn_TTC")] <- NA
combined.data.df[which(combined.data.df$SPECIES_QUALIFIED == "PKE" & is_odd(combined.data.df$Year)), c("Spn_Combined", "Spn_nuSEDS", "Spn_TTC")] <- NA

#-----
# Subtract SEL from Taku River SER
#-----
combined.data.df[which(combined.data.df$SPECIES_QUALIFIED == "SER" & combined.data.df$TTC_RECORD_MATCH == "Natural Spawning Escapement" & combined.data.df$TTC_STOCK_MATCH == "Taku River"), c("Year", "Spn_TTC", "Spn_Combined")] 

# Calculate sum of relevant SEL populations
SEL_sum <- combined.data.df %>%
	filter(SPECIES_QUALIFIED == "SEL" & TTC_STOCK_MATCH == "Taku River") %>%
	group_by(Year) %>%
	summarise(sum(Spn_TTC)) %>%
	filter(Year >= 2004)

# Set Years < 2004 to NA in Taku River SER
combined.data.df <- combined.data.df %>%
	mutate(Spn_Combined = replace(Spn_Combined, 
																SPECIES_QUALIFIED == "SER" & TTC_STOCK_MATCH == "Taku River" & Year < 2004 & TTC_RECORD_MATCH == "Natural Spawning Escapement", NA))

# Substract SEL from SER for 2004-2023
ind.TakuSER <- which(combined.data.df$SPECIES_QUALIFIED == "SER" & combined.data.df$TTC_STOCK_MATCH == "Taku River" & combined.data.df$Year >= 2004 & combined.data.df$TTC_RECORD_MATCH == "Natural Spawning Escapement")

combined.data.df$Spn_Combined[ind.TakuSER] <- combined.data.df$Spn_Combined[ind.TakuSER] - SEL_sum$`sum(Spn_TTC)`


###############################################################################
# Write to CSV
###############################################################################

write_csv(combined.data.df,"DATA_PROCESSING/DATA/2_ExtractedData/1_Combined_Data_Long_WithDetails.csv")

names(combined.data.df)

# Change from arrange by TTC_Species to SPECIES_QUALIFIED
record.summary.byyear.byspecies <- combined.data.df %>% group_by(Year,SPECIES_QUALIFIED) %>%
    summarize(Num_nuSEDS = sum(!is.na(Spn_nuSEDS)),
              Num_TTC = sum(!is.na(Spn_TTC)),
    					Num_Combined = sum(!is.na(Spn_Combined))) %>%
    arrange(SPECIES_QUALIFIED)

write_csv(record.summary.byyear.byspecies,"DATA_PROCESSING/DATA/2_ExtractedData/3_RecordSummary_ByYear_BySpecies.csv")


# create "wide" file for explorer input


wide.data.df <- combined.data.df %>% select(streamid, Year, Spn_Combined) %>%
                #mutate(Year = paste0("X.",Year)) %>%
                # mutate(Spn_Combined = replace_na(as.character(Spn_Combined),"")) %>%
                pivot_wider(id_cols = streamid,names_from = Year, names_prefix = "X.",values_from = Spn_Combined)  #, values_fn = length)


wide.data.df <- left_join(site.info.df,wide.data.df, by="streamid")

head(wide.data.df)

write_csv(wide.data.df,"DATA_PROCESSING/DATA/2_ExtractedData/2_Combined_Data_Wide.csv")

###############################################################################
# Output data for PSE spawner surveys
###############################################################################

# Re-level nuSEDS ESTIMATE_CLASSIFICATION to be Low, Medium-Low, Medium, Medium-High, High, Unknown
unique(combined.data.df$ESTIMATE_CLASSIFICATION)
unique(combined.data.df$TTC_QUALITY)

qualityScores <- data.frame(
	ESTIMATE_CLASSIFICATION = c("TRUE ABUNDANCE (TYPE-1)", "TRUE ABUNDANCE (TYPE-2)", "RELATIVE ABUNDANCE (TYPE-3)", "RELATIVE ABUNDANCE (TYPE-4)", "RELATIVE ABUNDANCE (TYPE-5)", "PRESENCE/ABSENCE (TYPE-6)", "UNKNOWN"),
	DQ = c("High", "Medium-High", "Medium", "Medium-Low", "Low", "Low", "Unknown"),
	TTC_QUALITY = c(1, 2, 3, 4, 5, 5, NA)
)


combined.data.df$DQ <- NA
combined.data.df$DQ[combined.data.df$PSE_source != "NuSEDS"] <- qualityScores$DQ[match(combined.data.df$TTC_QUALITY[combined.data.df$PSE_source != "NuSEDS"], qualityScores$TTC_QUALITY)]
combined.data.df$DQ[combined.data.df$PSE_source == "NuSEDS"] <- qualityScores$DQ[match(combined.data.df$ESTIMATE_CLASSIFICATION[combined.data.df$PSE_source == "NuSEDS"], qualityScores$ESTIMATE_CLASSIFICATION)]

# Include species_name field
speciesLookup <- data.frame(
	SPECIES_QUALIFIED = sort(unique(combined.data.df$SPECIES_QUALIFIED)),
	species_name = c("Chinook", "Chum", "Coho", "Pink (even)", "Pink (odd)", "Lake sockeye", "River sockeye", "Steelhead")
)

combined.data.df$species_name <- speciesLookup$species_name[match(combined.data.df$SPECIES_QUALIFIED, speciesLookup$SPECIES_QUALIFIED)]

pse.data <- data.frame(
	region = rep("Transboundary", dim(combined.data.df)[1]),
	species_name = combined.data.df$species_name,
	cuid = combined.data.df$cuid,
	cu_name_pse = combined.data.df$cu_name_pse,
	streamid = combined.data.df$streamid,
	stream_name_pse = str_to_title(combined.data.df$stream_name_pse),
	indicator = combined.data.df$IS_INDICATOR,
	latitude = combined.data.df$latitude,
	longitude = combined.data.df$longitude,
	year = combined.data.df$Year,
	stream_observed_count = combined.data.df$Spn_Combined, # !! Draws on Spn_Combined field
	stream_survey_method = ifelse(combined.data.df$PSE_source == "NuSEDS", combined.data.df$ESTIMATE_METHOD, combined.data.df$TTC_METHOD),
	stream_survey_quality = combined.data.df$DQ
)

# Remove years with no data
pse.data <- pse.data %>%
	filter(!is.na(stream_observed_count))

# Assume if there's no IS_INDICATOR that it's a non-indicator
pse.data$indicator[is.na(pse.data$indicator)] <- "N"

# Check zeroes (historically we set zeros to NAs...)
pse.data %>% filter(stream_observed_count == 0) %>%
	data.frame() # Aerial surveys -> NA

# pse.data <- pse.data %>% 
# 	mutate(stream_observed_count = replace(
# 		stream_observed_count, 
# 		stream_observed_count == 0 & stream_survey_method == "Aerial survey", 
# 		NA))
# Decision: Do NOT set these aerial surveys to zero, because they are targetting the CU of interest at the relevant time and are thus meaningful observations.

# Write csv
write_csv(pse.data, paste0("DATA_PROCESSING/DATA/2_ExtractedData/dataset_1part2.", strftime(Sys.Date(), format = "%b%d%Y"),  ".csv"))

