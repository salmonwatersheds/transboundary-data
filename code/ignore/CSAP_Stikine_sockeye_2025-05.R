# Read in TBR spawner survey data
library(dplyr)

source("../../population-indicators/code/functions_general.R")

datasetsNames_database_fun()
ss_all <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_streamspawnersurveys_output")

ss <- ss_all %>% filter(species_name %in% c("Lake sockeye", "River sockeye"), region == "Northern Transboundary")

head(ss)

sort(unique(ss$stream_name_pse))

names_Stikine <- c("Tahltan Lake", "Tuya River", "Chutine River", "Christina Creek", "Scud River", "Porcupine River", "Verrett River", "Verrett Slough", "Bronson Creek", "Craig River", "Stikine River")

ss2 <- ss %>% filter(stream_name_pse %in% names_Stikine)

ss2$cu_name_pse <- factor(ss2$cu_name_pse)
n <- length(names_Stikine)

n

plot(range(ss2$year), range(ss2$stream_observed_count, na.rm = TRUE)*10^-3, "n")

for(i in 1:n){
	ss2.i <- ss2 %>% filter(stream_name_pse == names_Stikine[i])
	lines(ss2.i$year, ss2.i$stream_observed_count*10^-3, "o", col = PNWColors::pnw_palette("Bay", n = length(unique(ss2$cuid)))[as.numeric(ss2.i$cu_name_pse)])
}

legend("topleft", lwd = 1, pch = 1, col = PNWColors::pnw_palette("Bay", n = length(unique(ss2$cuid))), levels(ss2$cu_name_pse))

plot(range(ss2$year), c(0, 1), "n")

for(i in 1:n){
	ss2.i <- ss2 %>% filter(stream_name_pse == names_Stikine[i])
	lines(ss2.i$year, ss2.i$stream_observed_count*10^-3, "o", col = PNWColors::pnw_palette("Bay", n = length(unique(ss2$cuid)))[as.numeric(ss2.i$cu_name_pse)])
}

legend("topleft", lwd = 1, pch = 1, col = PNWColors::pnw_palette("Bay", n = length(unique(ss2$cuid))), levels(ss2$cu_name_pse))


# Correlation
chutine <- ss2 %>% filter(stream_name_pse == "Chutine River") %>% select(year, stream_observed_count)

stikine <- ss2 %>% filter(stream_name_pse == "Stikine River") %>% select(year, stream_observed_count)

plot(stikine$stream_observed_count[match(chutine$year,stikine$year)], chutine$stream_observed_count, col = PNWColors::pnw_palette("Bay", n = 50)[chutine$year - min(chutine$year) + 1], pch = 19)

cor.test(stikine$stream_observed_count[match(chutine$year,stikine$year)], chutine$stream_observed_count)

# No "significant" correlation...
# Correlation
christina <- ss2 %>% filter(stream_name_pse == "Christina Creek") %>% select(year, stream_observed_count)

plot(stikine$stream_observed_count[match(christina$year,stikine$year)], christina$stream_observed_count, col = PNWColors::pnw_palette("Bay", n = 50)[chutine$year - min(chutine$year) + 1], pch = 19)

cor.test(stikine$stream_observed_count[match(christina$year,stikine$year)], christina$stream_observed_count)


###############

ttc <- read.csv('data/1_raw-data/TTC_ManualExtract_Stikine_Sockeye_updated2025.csv')

unique(ttc$Series)

ttc.i <- ttc %>% filter(Series == "Escapement - Mainstem")

plot(ttc.i$Year, ttc.i$Value, "o")
points(ss2$year[ss2$stream_name_pse == "Stikine River"], ss2$stream_observed_count[ss2$stream_name_pse == "Stikine River"], col = 2, pch = 19, cex = 0.8)


ttc.chr <- ttc %>% filter(Series == "Christina Creek Aerial Index")
plot(ttc.chr$Year, ttc.chr$Value, "o")

ttc.chu <- ttc %>% filter(Series == "Chutine River Aerial Index")
mean.chu <- exp(rollmean(log(ttc.chu$Value + 1), k = 5, align = "right", na.pad = TRUE))
plot(ttc.chu$Year, ttc.chu$Value, "o")
lines(ttc.chu$Year, mean.chu, lwd = 2)
abline(h = quantile(ttc.chu$Value, c(0.25, 0.75), na.rm = TRUE), col = c(2,3), lty = 2)

#####################
# Cu list

cu_list <- retrieve_data_from_PSF_databse_fun(name_dataset = appdata.vwdl_conservationunits_decoder) %>% filter(region == "Northern Transboundary")


gsi_stat <- data.frame(
	year = rep(c(2020:2024), 3),
	CU = rep(c("NTFjords", "Christina", "Chutine"), each = 5),
	percGSI = c(82, 86, 80, 85, 87, 2, 5, 3, 1, 2, 16, 9, 17, 14, 11)
)

tapply(gsi_stat$percGSI, gsi_stat$year, sum)

est_spwn <- data.frame(year = c(2020:2024),
					 NTFjords = ttc$Value[ttc$Series == "Escapement - Mainstem" & ttc$Year %in% c(2020:2024)]*gsi_stat$percGSI[gsi_stat$CU == "NTFjords"]/100,
					 Christina = ttc$Value[ttc$Series == "Escapement - Mainstem" & ttc$Year %in% c(2020:2024)]*gsi_stat$percGSI[gsi_stat$CU == "Christina"]/100,
					 Chutine = ttc$Value[ttc$Series == "Escapement - Mainstem" & ttc$Year %in% c(2020:2024)]*gsi_stat$percGSI[gsi_stat$CU == "Chutine"]/100
)

apply(est_spwn[, 2:4], 2, function(x){exp(mean(log(x)))})
