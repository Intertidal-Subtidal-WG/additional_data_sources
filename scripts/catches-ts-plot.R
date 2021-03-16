
#Time series of shifts in subtidal from 1980 to 2018

rm(list=ls())

library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)

#load hobo all data

data <- read.delim2("data/cleaned_data/Maine_DMR_catch_timeseries.csv", sep = ",", dec = ".")

data$YEAR <- as.numeric(data$YEAR)
data <- filter(data,YEAR > 1979)

p1 <- ggplot(data = data, aes(y = METRIC.TONS, x = YEAR, colour = SPECIES))+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17)
  )

p1
