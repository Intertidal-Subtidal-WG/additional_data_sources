# PCatch Time-Series from Maine #
# website: https://www.maine.gov/dmr/commercial-fishing/landings/historical-data.html
# EKB; March 2, 2021

# packages
library(tabulizer)
library(tidyverse)

# one-pagers
crab <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/crab.table.pdf",
                       output = "data.frame") %>% 
  
urchins <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/urchin.table.pdf",
                          output = "data.frame")
cod <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/cod.table.pdf",
                      output = "data.frame")

urchins <- urchins[[1]]
write_csv(urchins, "prepped_data/urchin_catch_ts.csv")

crab <- crab[[1]]
write_csv(crab, "prepped_data/crab_catch_ts.csv")

cod <- cod[[1]]
write_csv(cod, "prepped_data/cod_catch_ts.csv")

# multiple pages
lobster <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/lobster.table.pdf",
                          output = "data.frame")
lobster <- do.call(rbind, lobster)
lobster <- lobster[-c(1,53,100),]
colnames(lobster) <- c("YEAR", "SPECIES", "METRIC.TONS", "POUNDS", 
                       "POUNDS.millions.", "VALUE", "VALUE.millions.", 
                       "PRICEperLB", "NUM_LICENSE_HOLDERS", "NUM_TRAPS_MILLIONS", 
                       "WATER_TEMP_BOOTHBAY_HARBOR_C")
write_csv(lobster, "prepped_data/lobster_catch_ts.csv")

seaweeds <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/seaweeds.table.pdf",
                           output = "data.frame")
seaweeds <- do.call(rbind, seaweeds)
write_csv(seaweeds, "prepped_data/seaweed_catch_ts.csv")

# different structure
lobster_by_county <- extract_tables("https://www.maine.gov/dmr/commercial-fishing/landings/documents/lobster.county.pdf")
lobster_by_county <- do.call(rbind, lobster_by_county) %>% 
  as.data.frame()
colnames(lobster_by_county) <- c("YEAR", "COUNTY", "POUNDS", "VALUE")
write_csv(lobster_by_county, "prepped_data/lobster_catch_by_county_ts.csv")
