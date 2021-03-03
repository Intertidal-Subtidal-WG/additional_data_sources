
#Gulf of Maine region nutrients 
#from: http://grampus.umeoce.maine.edu/nutrients/#Data
#JB March 02 2021

#Packages
library(tidyverse)

#load data

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

