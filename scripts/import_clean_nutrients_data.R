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

## example plot
## heat map of sampling locations and intensity for 1981-2010
ggplot(data = nutr %>% filter(year > 1980), 
       mapping = aes(x = dec_long, y = dec_lat)) + 
  geom_hex() + 
  facet_wrap(~year, ncol = 5) + 
  theme(legend.position = "bottom")

## what are the spatial dimensions of the nutrient sampling?
range(nutr$dec_lat) # [1] 40.00 45.99
range(nutr$dec_long) # [1] -74.26 -60.01

## COMMENT:
## Appledore Island is located at (42.98683, -70.60899)

## define a bounding box using a 20 km 'search radius'
# https://gis.stackexchange.com/questions/80809/calculating-bounding-box-coordinates-based-on-center-and-radius
isl_lat <- 42.98683 ## Appledore latitude (decimal degrees)
isl_lon <- -70.60899 ## Appledore longitude (decimal degrees)
r <- 20 ## search radius
R <- 6371 ## radius of Earth
dlon <- 360 * r / R
dlat <- dlon * sin(circular::rad (isl_lon))

min_lon <- isl_lon - dlon
max_lon <- isl_lon + dlon
max_lon - min_lon # 2.260242

min_lat <- isl_lat + dlat
max_lat <- isl_lat - dlat
max_lat - min_lat # 2.132029

ggplot(data = nutr %>% filter(year == 2001), 
       mapping = aes(x = dec_long, y = dec_lat)) + 
  geom_hex() + 
  geom_rect(aes(xmin = min_lon, xmax = max_lon, 
                ymin = min_lat, ymax = max_lat), 
            colour = "red", fill = NA) + 
  coord_fixed()

## subset to data sampled within the bounding box
nutr_sub <- nutr %>% 
  filter(dec_long >= min_lon, dec_long <= max_lon, 
         dec_lat >= min_lat, dec_lat <= max_lat) 

## plot
ggplot(data = nutr_sub %>% 
         filter(year > 1980), 
       mapping = aes(x = dec_long, y = dec_lat)) + 
  geom_hex() + 
  geom_point(aes(x = isl_lon, y = isl_lat), 
             size = 2, colour = "red") + 
  geom_rect(aes(xmin = min_lon, xmax = max_lon, 
                ymin = min_lat, ymax = max_lat), 
            colour = "red", fill = NA) + 
  facet_wrap(~year, ncol = 5) + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_fixed()

## create a new column, where data are grouped into years that
## start September 1st and End August 31st (rather than calendar year)
nutr_sub <- nutr %>% 
  mutate(year2 = if_else(month(date) > 8, 
                         year + 1, year))

## calculate 'annual' values for each of the nutrient measurements
nutr_means <- nutr_sub %>% 
  group_by(year) %>% 
  summarise(across(.cols = temperature:chloro, 
                   ~ mean(., na.rm = TRUE)))


