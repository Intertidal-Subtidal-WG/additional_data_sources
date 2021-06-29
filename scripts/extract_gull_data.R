# Extract Gull Data
# EKB; June 2021

library(tidyverse)
library(tabulizer)

# extract gull data
gull_data <- extract_tables("data/raw_data/GullCensusSummary.pdf",
                            output = "data.frame")
gull_data <- gull_data[[1]] # extract dataframe

# first row of data is getting read in as column names
# separate it, remove X (aka just keep numbers), then rbind back onto dataframe
first_row <- colnames(gull_data) %>% 
  str_extract(., "[0-9]+")
gull_data <- rbind(first_row, gull_data)
# rename columns
colnames(gull_data) <- c("Year", "Herring_Gull", "Great_BlackBacked_Gull", "Total", "Notes")

# write to csv
write_csv(gull_data, "data/cleaned_data/gull_data.csv")
