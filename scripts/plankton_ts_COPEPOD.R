# Plankton Time-Series from Gulf of Maine #
# from COPEPOD dataset
# website: https://www.st.nmfs.noaa.gov/copepod/data/us-05103/html_src/data.html
# EKB; March 2, 2021

# packages
library(tidyverse)

# make list of file names
myfiles <- list.files("plankton_ts_GOM/data_src/short-format/", 
                      full.names = TRUE, pattern = "*.csv")

# empty list to put each df into
df_list <- list()

# read in each file, remove first 15 lines and blank row after column names
# then place in df_list
for (i in 1:length(myfiles)) {
  file <- read.csv("plankton_ts_GOM/data_src/short-format/zzzz-610716_us-05103.csv", 
                   skip = 15)
  df_list[[i]] <- file[-1,]
}

# bind all of the dataframes together
plankton_data <- do.call(rbind, df_list) 

# write big df to csv file
write_csv(plankton_data, "prepped_data/plankton_data.csv")
