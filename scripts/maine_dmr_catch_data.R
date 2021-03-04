# Catch Time-Series from Maine #
# website: https://www.maine.gov/dmr/commercial-fishing/landings/historical-data.html
# EKB; March 2, 2021

# packages
library(tabulizer)
library(tidyverse)

### READ IN DATA FROM PDFs, CLEAN, and WRITE TO CSV ###

# one-pagers
crab <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/crab.table.pdf",
                       output = "data.frame")
crab <- crab[[1]]
crab$YEAR[70] <- "2019"
crab <- crab %>% mutate_all(na_if, "")
#write_csv(crab, "prepped_data/crab_catch_ts.csv")
  
urchins <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/urchin.table.pdf",
                          output = "data.frame")
urchins <- urchins[[1]]
urchins$SPECIES[1] <- "SEA URCHIN"
urchins <- urchins %>% 
  mutate_all(na_if, "")
urchins$YEAR <- as.character(urchins$YEAR)
urchins$METRIC.TONS <- as.numeric(urchins$METRIC.TONS)
#write_csv(urchins, "prepped_data/urchin_catch_ts.csv")

# cod data
cod <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/cod.table.pdf",
                      output = "data.frame")
cod <- cod[[1]]
cod$YEAR[70] <- "2019"
cod <- cod %>% mutate_all(na_if, "")


# all lobster data
lobster <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/lobster.table.pdf",
                          output = "data.frame")
lobster <- do.call(rbind, lobster)
lobster <- lobster[-c(1,53,100),]
colnames(lobster) <- c("YEAR", "SPECIES", "METRIC.TONS", "POUNDS", 
                       "POUNDS.millions.", "VALUE", "VALUE.millions.", 
                       "PRICEperLB", "NUM_LICENSE_HOLDERS", "NUM_TRAPS_MILLIONS", 
                       "WATER_TEMP_BOOTHBAY_HARBOR_C")
lobster$YEAR[140] <- "2019"
lobster <- lobster %>% mutate_all(na_if, "")
lobster$POUNDS <- as.numeric(lobster$POUNDS)
lobster <- lobster %>% 
  mutate(METRIC.TONS = POUNDS * 0.00045359237)
lobster$METRIC.TONS <- as.numeric(lobster$METRIC.TONS)
lobster$SPECIES <- "LOBSTER, AMERICAN"
#write_csv(lobster, "prepped_data/lobster_catch_ts.csv")

# seaweed data
seaweeds <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/seaweeds.table.pdf",
                           output = "data.frame")
seaweeds <- do.call(rbind, seaweeds)
seaweeds$YEAR[48] <- "2019"
seaweeds <- seaweeds %>% mutate_all(na_if, "")
#write_csv(seaweeds, "prepped_data/seaweed_catch_ts.csv")

# different structure
lobster_by_county <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/lobster.county.pdf")
lobster_by_county <- do.call(rbind, lobster_by_county) %>% 
  as.data.frame()
colnames(lobster_by_county) <- c("YEAR", "COUNTY", "POUNDS", "VALUE")
#write_csv(lobster_by_county, "prepped_data/lobster_catch_by_county_ts.csv")

### COLLATE INTO ONE DATAFRAME ###

catch_data <- full_join(select(cod, YEAR, SPECIES, METRIC.TONS),
  select(urchins, YEAR, SPECIES, METRIC.TONS)) %>% 
  full_join(., select(crab, YEAR, SPECIES, METRIC.TONS)) %>% 
  full_join(., select(lobster, YEAR, SPECIES, METRIC.TONS)) %>% 
  full_join(., select(seaweeds, YEAR, SPECIES, METRIC.TONS))
catch_data$YEAR <- as.numeric(catch_data$YEAR)
catch_data$METRIC.TONS <- as.numeric(catch_data$METRIC.TONS)

ggplot(catch_data, aes(x = YEAR, y = METRIC.TONS, group = SPECIES, color = SPECIES)) +
  geom_line() +
  theme_bw()
