
#Gulf of Maine region nutrients 
#from: http://grampus.umeoce.maine.edu/nutrients/#Data
#JB March 02 2021

#Packages
library(tidyverse)

#load data and rename columns accordingly to the source

myfile <- read.delim2("RebuckGoMaineNutrients.txt")

data <- myfile %>% 
  rename(
    month = X1,
    day = X2,
    year = X2001,
    dec_long = X.66.85, #decimal longitude
    dec_lat = X44.93, # decimal latitude
    bottom_depth = NaN.,
    sample_depth = X1.1,
    temperature = NaN..1,
    salinity = NaN..2,
    NO3_NO2 = X7.54, #NO3+NO2
    Si_OH = X6.61, #Si(OH)4
    PO4 = X0.92,
    chl = X0.55,
    PO4_quality = X0,
    Si_OH_quality = X0.1,
    NO3_NO2_quality = X0.2
  )

# select years

data_year <- filter(data, (year > 1981) & (year < 2019))


# remove bad quality nutrients data 
# QA/QC flags: 0-passed all criteria; 
# 1-outside UMaine range check; 
# 2-WOD questionable cruise results; 
# 3-WOD questionable cast results; 
# 4-fail extreme criteria listed in text; 
# 9-NaN, on land, zeros for all nutrients

nutrients <- filter(data_year, (PO4_quality = 0) | (PO4_quality = 1) | (Si_OH_quality = 0) | (Si_OH_quality = 1) |
                 (NO3_NO2_quality = 0) | (NO3_NO2_quality = 1))

# select GPS locations


#select depth

nutrients <- filter(nutrients, sample_depth < 100)

# write csv
write_csv(nutrients, "prepped_data/nutrients_ts.csv")


# Plot trends
nutrients$NO3_NO2 <- as.numeric(nutrients$NO3_NO2)
nutrients$sample_depth <- as.numeric(nutrients$sample_depth)

plot(nutrients$NO3_NO2 ~ nutrients$year)


