# initial set up ----------------------------------------------------------

## where am I working?
here::here()
# getwd()

## load required packages
library(tidyverse)
library(lubridate)
library(ggthemes)

## set a plotting theme
theme_set(theme_few())


# import data -------------------------------------------------------------

## read in files
list.files("data/GoM_catch_data")

## these are not annualized datasets
nutrient <- read_csv("data/GoM_catch_data/nutrients_ts.txt")

plankton <- read_csv("data/GoM_catch_data/plankton_data.txt")

## these are annualized (can be combined)
urchin <- read_csv("data/GoM_catch_data/urchin_catch_ts.txt", 
                   col_types = cols(.default = col_character())) %>% 
  mutate(YEAR = as.character(YEAR), 
         SPECIES = if_else(is.na(SPECIES), "SEA URCHIN", SPECIES))

seaweed <- read_csv("data/GoM_catch_data/seaweed_catch_ts.txt", 
                    col_types = cols(.default = col_character()))

lobster <- read_csv("data/GoM_catch_data/lobster_catch_ts.txt", 
                    col_types = cols(.default = col_character())) %>% 
  rename(PRICE.LB = PRICEperLB) %>% 
  mutate(SPECIES = "AMERICAN LOBSTER") %>% 
  select(-c("NUM_LICENSE_HOLDERS", "NUM_TRAPS_MILLIONS", "WATER_TEMP_BOOTHBAY_HARBOR_C"))

crab <- read_csv("data/GoM_catch_data/crab_catch_ts.txt", 
                 col_types = cols(.default = col_character()))

cod <- read_csv("data/GoM_catch_data/cod_catch_ts.txt", 
                col_types = cols(.default = col_character())) %>% 
  rename(POUNDS = LIVE.POUNDS)

## COMMENT:
## treat the default column type as 'character', otherwise there are
## issues when we go to merge the data
## will correct the column types after the merge (see below)


# merge annualized datasets -----------------------------------------------

## merge the catch datasets together
catch_data <- bind_rows(list(urchin, seaweed, lobster, crab, cod))

## remove all the commas and dollar signs from values
## (who the hell puts commas between numbers in a data frame!?!)
suppressWarnings(
  catch_data <- catch_data %>% 
  mutate(across(.cols = METRIC.TONS:PRICE.LB, ~ gsub(",", "", .))) %>% 
  mutate(across(.cols = METRIC.TONS:PRICE.LB, ~ gsub("$", "", .))) %>% 
  mutate(YEAR = as.numeric(YEAR), 
         across(.cols = METRIC.TONS:PRICE.LB, ~ as.double(.)))
  )

## plot the data
ggplot(data = catch_data, 
       mapping = aes(x = as.numeric(YEAR), 
                     y = as.numeric(METRIC.TONS), colour = SPECIES)) + 
  geom_point() + 
  geom_line() + 
  scale_colour_brewer(palette = "Dark2") + 
  facet_wrap(~SPECIES, scales = "free_y") + 
  labs(y = "Catch (metric tons)", x = "Year") + 
  theme(legend.position = "none")

## export merged data
write_csv(catch_data, "data/clean_data/merged_catch_data.csv")
