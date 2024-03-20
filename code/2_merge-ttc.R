###############################################################################
# This script implements the following steps
# - Read in TTC_ManualExtract... files
# - Merge relevant surveys into single TTC_MERGED.csv output
###############################################################################

library(tidyverse)

# get list of manual TTC extracts



ttc.src.list <- list.files(path = "data/1_raw-data/", pattern = "TTC_ManualExtract_",full.names = TRUE)

ttc.src.list



if(exists("ttc.merged.df")){rm(ttc.merged.df)}

for(i in 1:length(ttc.src.list)){


if(i == 1){ ttc.merged.df <- read_csv(ttc.src.list[i]) %>%
	select(SPECIES, Stock, Series, Year, Value, TableSource)
	}

if(i > 1){ ttc.merged.df <- rbind(ttc.merged.df , read_csv(ttc.src.list[i]) %>%
																		select(SPECIES, Stock, Series, Year, Value, TableSource)) }


}


head(ttc.merged.df)
dim(ttc.merged.df)


write_csv(ttc.merged.df,"data/2_clean-data/TTC_MERGED.csv")



