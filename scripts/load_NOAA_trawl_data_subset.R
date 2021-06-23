
## load required packages
library(tidyverse)
library(stringr)

## import data 
URL <- "https://raw.githubusercontent.com/Intertidal-Subtidal-WG/additional_data_sources/master/data/seasonal_NOAA_trawl_data.csv"
per_season <- read_csv(URL)


# data sourcing and preparation -------------------------------------------

## This data set has been filtered from the seasonal 
## Bottom Trawl Surveys data, available from:
## https://www.fisheries.noaa.gov/inport/item/22557
## To generate this dataset, we pulled the season-specific (cumulative)
## catch records from the UNION_FSCS_SVCAT tables on NOAA database:
## https://www.fisheries.noaa.gov/inport/item/33622
## We used the supplementary table (from Distribution 2) 
## SVDBS_SVMSTRATA.csv to identify the midpoint coordinates for each
## STRATUM, where strata represent individual trawling 'transects'.
## we filtered the list of strata to those within 100 km of Appledore
## (42.98683, -70.60899) using a geographic distance matrix approach.
## This identified 13 strata within 100 km of Appledore, of which 
## 7 strata were found in the datasets:
# unique(per_strata$STRATUM)
# [1] "01260" "01270" "01400" "03630" "03640" "03650" "03660"
## After subsetting to these 7 strata, we summed the total number of 
## individuals caught of each species per stratum per season per year. 
## We similarly calculated total mass of caught individuals. Both 
## these values were standardized for effort by dividing by the number 
## or samples (TOW or STATION) taken along each transect. Finally, we
## averaged these effort-standardized values from all strata to calculate
## season-by-year averages of catch and biomass.


# filter and calculate annual averages ------------------------------------

## We think it would be sensible to average the spring and autumn
## values to get an 'integrated measure' across the period of sampling
## on Appledore.

## calculate annual average number of individuals and biomass caught
## for each species 
avg_aut_spr <- per_season %>% 
  filter(SEASON %in% c("autumn", "spring")) %>% 
  group_by(SVSPP, SCINAME, COMNAME, YEAR) %>% 
  summarise(across(.cols = avg_individ_PUE:scaled_avg_tot_mass_PUE, 
                   ~ mean(., na.rm = TRUE)))


# explanation of data -----------------------------------------------------

## summary information
summary(avg_aut_spr)
names(avg_aut_spr)

## avg_individ_PUE = the average per-unit-effort number of individuals
## caught (per species) across all transects within 100 km of Appledore
#
## avg_tot_mass_PUE = the average per-unit-effort biomass of all 
## individuals caught (per species) across all transects within 100 km 
## of Appledore
#
## scaled_avg_individ_PUE = scaled (but not centered) avg_individ_PUE
#
## scaled_avg_tot_mass_PUE = scaled (but not centered) avg_tot_mass_PUE

## Unique species
n_distinct(avg_aut_spr$COMNAME) ## [1] 150
unique(avg_aut_spr$COMNAME)


# filter to species of interest -------------------------------------------

avg_aut_spr2 <- avg_aut_spr %>% 
  filter(COMNAME %in% c("ATLANTIC COD", "HADDOCK", "CUNNER", 
                        "JONAH CRAB", "ATLANTIC ROCK CRAB", 
                        "RED DEEPSEA CRAB", 
                        "SNOW CRAB", "NORTHERN STONE CRAB", 
                        "AMERICAN LOBSTER"))

## plot the time series
## average catch per unit effort
ggplot(data = avg_aut_spr2, 
       mapping = aes(x = YEAR, y = scaled_avg_individ_PUE, 
                     group = COMNAME)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ COMNAME)

## average total biomass
ggplot(data = avg_aut_spr2, 
       mapping = aes(x = YEAR, y = scaled_avg_tot_mass_PUE, 
                     group = COMNAME)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ COMNAME)

## -- ## -- ## -- ## -- ## END OF SCRIPT ## -- ## -- ## -- ## -- ##
