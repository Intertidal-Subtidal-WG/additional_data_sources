# initial set up ----------------------------------------------------------

## where am I working?
here::here()

## load required packages
library(tidyverse)
library(lubridate)
library(geosphere)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)

## set a plotting theme
theme_set(theme_few())


# import full GoM nutrients dataset ---------------------------------------

## Citation: Rebuck et al. (2012). Gulf of Maine Region Nutrient and
## Hydrographic Database. School of Marine Sciences, University of Maine.

## specify data location (URL)
URL <- "http://grampus.umeoce.maine.edu/nutrients/RebuckGoMaineNutrients.txt"

## import data
nutr <- read_tsv(URL, 
                 col_names = c("month", "day", "year", 
                               "dec_long", "dec_lat", "bottom_depth", 
                               "sample_depth", "temperature", "salinity", 
                               "NO3_NO2", "Si_OH", "PO4", "chloro", 
                               "PO4_quality", "Si_OH_quality", 
                               "NO3_NO2_quality", "BLANK"), 
                 na = c("NA", "NaN", "")) %>% 
  ## drop the last column (it's empty)
  select(-BLANK) %>%
  ## drop 1 row where day is missing and 9 rows where year is missing
  filter(!is.na(day), !is.na(year)) %>% 
  ## drop 8 rows from July 2010 where day = "309"
  ## what day this is supposed to be...
  filter(day != 309) %>% 
  ## create a date column
  unite(date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  mutate(date = ymd(date))

## COMMENT:
## there is one row [133337] that is missing all but one value, and so
## cannot be parsed properly. This row is dropped during the filtering
## steps above.

## create a new column, where data are grouped into years that
## start September 1st and End August 31st (rather than calendar year)
nutr <- nutr %>% 
  mutate(year2 = if_else(month(date) > 8, 
                         year + 1, year))


# exploration -------------------------------------------------------------

names(nutr)

## what is the temporal range of nutrient sampling?
range(nutr$year) # [1] 1925 2010

## what are the spatial dimensions of sampling?
range(nutr$dec_lat) # [1] 40.00 45.99
range(nutr$dec_long) # [1] -74.26 -60.01

## and the vertical range of sampling depths?
range(nutr$sample_depth, na.rm = TRUE)

## COMMENT:
## Appledore Island is located at (42.98683, -70.60899)


# points within a radius --------------------------------------------------

isl_lat <- 42.98683 ## Appledore latitude (decimal degrees)
isl_lon <- -70.60899 ## Appledore longitude (decimal degrees)
station <- cbind(isl_lon, isl_lat)

## nead to constrain the dataset in order to create a distance matrix
## (WAYYYYY too many points currently)
## let's limit our data to points within +/- 1 degree in of Appledore

## define bounds
diff <- 1
min_lat <- isl_lat - diff
max_lat <- isl_lat + diff
min_lon <- isl_lon - diff
max_lon <- isl_lon + diff

## subset data
nutr_sub <- nutr %>% 
  ## drop data before 1982 (start of intertidal sampling)
  filter(year2 > 1981) %>% 
  ## restrict to samples above 100 m
  filter(sample_depth < 100) %>% 
  ## constrain to data within bounds
  filter(dec_long <= max_lon, dec_long >= min_lon, 
         dec_lat <= max_lat, dec_lat >= min_lat)
nrow(nutr_sub2) # [1] 74010
  
## pull out coordinates
sample_locs <- nutr_sub[c("dec_long", "dec_lat")]

## calculate the geographic distance matrix from all points to Appledore
# dmat <- geosphere::distm(sample_locs, station, fun = distGeo)
dmat <- as.matrix(raster::pointDistance(sample_locs, station, 
                                        lonlat = TRUE))

## how many points are within 50 km?
near_50 <- apply(dmat < 50000, 2, which)
length(near_50) # [1] 5228

## add a column to the subset that identifies if the point is within the radius
nutr_sub <- nutr_sub %>% 
  mutate(near = as.factor(
    if_else(row_number() %in% near_50, 1, 0)))

## Check if that worked
table(nutr_sub$near)


# plot sampling points ----------------------------------------------------

## import map of the world with country borders
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

## map of sampling locations in 2005
ggplot() + 
  ## map
  geom_sf(data = world, 
          fill = "grey80", colour = "black") + 
  coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) + 
  ## Appledore Island
  geom_point(aes(x = isl_lon, y = isl_lat), 
             size = 2, colour = "black", shape = 19) + 
  ## sampling points
  geom_jitter(data = nutr_sub %>% filter(year == 2005), 
             mapping = aes(x = dec_long, y = dec_lat, 
                           shape = near, fill = near), 
             alpha = 0.5, 
             position = position_jitter(width = 0.01, height = 0.01)) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(22, 21)) + 
  annotate("segment", x = -71, xend = isl_lon-0.05, y = 43, yend = isl_lat, 
           colour = "black", lwd = 0.8, arrow = arrow(length = unit(2, "mm"))) + 
  annotate("text", x = -71.05, y = 43, label = "Appledore", 
           colour = "black", hjust = 1, size = 4) + 
  labs(x = NULL, y = NULL, 
       caption = "Source: Gulf of Maine Nutrient and Hydrographic Database") + 
  theme(legend.position = "none", 
        title = element_text(size = 10))


# calculate mean 'annual' nutrient values ---------------------------------

## calculate 'annual' values for each of the nutrient measurements
(nutr_means <- nutr_sub %>% 
   group_by(year2) %>% 
   summarise(across(.cols = temperature:chloro, 
                    ~ mean(., na.rm = TRUE))))

## COMMENT:
## Not all years between 1925 and 2010 have data; would need to first
## interpolate averages to fill in missing years
## Also need to find data for 2011 onward (Kylla may have already?)

## plot time series of environmental measurements
nutr_means %>% 
  pivot_longer(cols = temperature:chloro, names_to = "measure", 
               values_to = "value") %>% 
  drop_na() %>% 
  ggplot(data = ., 
         mapping = aes(x = year2, y = value, colour = measure)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ measure, scales = "free_y") + 
  scale_colour_brewer(palette = "Dark2") + 
  labs(x = "Year", y = "Value") + 
  theme(legend.position = "none")

