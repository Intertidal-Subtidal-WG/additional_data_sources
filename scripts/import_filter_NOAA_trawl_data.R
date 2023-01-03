
## where am I working?
getwd()
setwd(here::here("data/raw_data/NOAA_trawl_data"))
list.files()

## load required packages
library(tidyverse)
library(stringr)
# library(vctrs)
library(ggthemes)

## set plotting theme
theme_set(theme_few())

## import seasonal trawl catch data
f.imp <- list.files(pattern = "SVCAT.csv")
map(f.imp, read_csv) %>% 
  set_names(str_remove(f.imp, pattern = "_2256[0123]_SVCAT.csv")) %>% 
  list2env(.GlobalEnv)
rm(f.imp)

## import stratum information
strata <- read_csv("stratum_information_SVDBS.csv")

## import species list
species <- read_csv("species_list_SVDBS.csv")


# subset strata -----------------------------------------------------------

## subset to strata within the Gulf of Maine
strata <- strata %>% 
  filter(str_detect(stratum_name, "GME"))

## custom function to insert a string after the i-th position
str_insert <- function(x, pos, insert) {
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

## format longitude and latitude columns
strata <- strata %>% 
  mutate(across(.cols = midlat:maxlon, 
                ~ str_insert(x = ., pos = 2, insert = ".")), 
         across(.cols = c("midlon", "minlon", "maxlon"), 
                ~ ifelse(is.na(.), NA, paste0("-", .))), 
         across(.cols = midlat:maxlon, ~ as.numeric(.)))


## filter summarized data to include only strata with a midpoint 
## within 100 km of Appledore

## Appledore coordinates
isl_lat <- 42.98683 ## Appledore latitude (decimal degrees)
isl_lon <- -70.60899 ## Appledore longitude (decimal degrees)
station <- cbind(isl_lon, isl_lat)

## midpoint coordinates for strata
sample_locs <- strata[c("midlon", "midlat")]

## calculate the geographic distance matrix from all points to Appledore
dmat <- as.matrix(raster::pointDistance(sample_locs, station, 
                                        lonlat = TRUE))

## how many points are within 100 km?
near_100 <- apply(dmat < 100000, 2, which)
length(near_100) # [1] 13

## add a column to the subset that identifies if the point is within the radius
strata <- strata %>% 
  mutate(near = as.factor(
    if_else(row_number() %in% near_100, 1, 0)))

## Check if that worked
table(strata$near)

## plot strata
ggplot(data = strata, 
       mapping = aes(x = midlon, y = midlat, 
                     group = stratum, colour = near)) + 
  geom_point() + 
  geom_segment(mapping = aes(x = minlon, xend = maxlon, 
                             y = minlat, yend = maxlat)) + 
  scale_x_continuous(limits = c(-71,-65))

write_csv(strata |> filter(near == "1"), "../../strata_used_noaa.csv")
# bind seasonal data together ---------------------------------------------

## bind the seasonal data together in a single table
# autumn <- mutate(autumn, season = "autumn")
# spring <- mutate(spring, season = "spring")
# summer <- mutate(summer, season = "summer")
# winter <- mutate(winter, season = "winter")

## fix issues with column classes (don't match)
summer <- mutate(summer, CATCHSEX = as.character(CATCHSEX))
spring <- mutate(spring, CATCHSEX = as.character(CATCHSEX))

## bind subsets together
all_seasons <- bind_rows(autumn, winter, spring, summer, 
                         .id = "SEASON") %>% 
  mutate(SEASON = recode_factor(SEASON, 
                                "1" = "autumn", "2" = "winter", 
                                "3" = "spring", "4" = "summer"))

## get the year from CRUISE6 
## (= 6 digits, first 4 indicate year, last 2 indicate cruise number)
all_seasons <- all_seasons %>% 
  mutate(YEAR = as.integer(substr(CRUISE6, start = 1, stop = 4)))


# summarise data ----------------------------------------------------------

## calculate the total seasonal species-specific catch and biomass 
## values by year and stratum
per_strata <- all_seasons %>% 
  group_by(STRATUM, YEAR, SEASON, SVSPP) %>% 
  summarise(n_individ = sum(EXPCATCHNUM), 
            tot_mass = sum(EXPCATCHWT), 
            n_station = n_distinct(STATION), 
            n_tow = n_distinct(TOW), 
            n_individ_PUE = n_individ / n_tow, 
            tot_mass_PUE = tot_mass / n_tow)

## filter summarized data to include only strata within near Appledore
per_strata <- per_strata %>% 
  filter(STRATUM %in% unique(strata$stratum[strata$near == 1])) %>% 
  left_join(., species, by = "SVSPP")

## calculate the season-specific averages (across all strata)
per_season <- per_strata %>% 
  group_by(YEAR, SEASON, SVSPP, SCINAME, COMNAME) %>% 
  summarise(avg_individ_PUE = mean(n_individ_PUE, na.rm = TRUE), 
            avg_tot_mass_PUE = mean(tot_mass_PUE, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(SVSPP, SEASON) %>% 
  mutate(scaled_avg_individ_PUE = scale(avg_individ_PUE, 
                                        scale = TRUE, center = FALSE), 
         scaled_avg_tot_mass_PUE = scale(avg_tot_mass_PUE, 
                                         scale = TRUE, center = FALSE))
  
## plot time series of average per-stratum counts (PUE) for AUTUMN
ggplot(data = per_season,
       mapping = aes(x = YEAR, y = scaled_avg_individ_PUE, 
                     colour = SVSPP)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ SEASON) + 
  labs(x = NULL, y = "Scaled number of individuals\nacross all strata") + 
  theme(legend.position = "none")

## plot time series of species of interest in SPRING
ggplot(data = per_season %>% 
         filter(SEASON == "spring", 
                COMNAME %in% c("ATLANTIC COD", "HADDOCK", "CUNNER", 
                               "JONAH CRAB", "ATLANTIC ROCK CRAB", 
                               "RED DEEPSEA CRAB", 
                               "SNOW CRAB", "NORTHERN STONE CRAB", 
                               "AMERICAN LOBSTER")), 
       mapping = aes(x = YEAR, y = scaled_avg_individ_PUE, 
                     colour = COMNAME)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ COMNAME) + 
  theme(legend.position = "none")

## biomasses (total weight of all captured individuals)
ggplot(data = per_season %>% 
         filter(SEASON == "spring", 
                COMNAME %in% c("ATLANTIC COD", "HADDOCK", "CUNNER", 
                               "JONAH CRAB", "ATLANTIC ROCK CRAB", 
                               "RED DEEPSEA CRAB", 
                               "SNOW CRAB", "NORTHERN STONE CRAB", 
                               "AMERICAN LOBSTER")), 
       mapping = aes(x = YEAR, y = scaled_avg_tot_mass_PUE, 
                     colour = COMNAME)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ COMNAME) + 
  theme(legend.position = "none")
