library(tidyverse)
library(cowplot)

env1= read.csv("env_data_annual_2sep20.csv")

#SST!!#

p1= ggplot(data=env1, aes(ï..Year, sst_yr))+
  geom_point()+
  ggtitle("Annual SST")+
  theme(legend.position = "none")
p1

p2= ggplot(data=env1, aes(ï..Year, sst_summer))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Summer SST")+
  theme(legend.position = "none")
p3= ggplot(data=env1, aes(ï..Year, sst_winter))+
  geom_point()+
  ggtitle("Winter SST")+
  theme(legend.position = "none")
plot_grid(p2,p3)

#ATMP!!#
p4= ggplot(data=env1, aes(ï..Year, atmp_yr))+
  geom_point()+
  ggtitle("Annual ATMP")+
  theme(legend.position = "none")
p4

p5= ggplot(data=env1, aes(ï..Year, atmp_summer))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Summer ATMP")+
  theme(legend.position = "none")
p6= ggplot(data=env1, aes(ï..Year, atmp_winter))+
  geom_point()+
  ggtitle("Winter ATMP")+
  theme(legend.position = "none")
plot_grid(p5,p6)


#Precipitation#
p7= ggplot(data=env1, aes(ï..Year, prcp_sum))+
  geom_point()+
  ggtitle("Precipitation-summer")+
  theme(legend.position = "none")
p7

#NAO#
p8= ggplot(data=env1, aes(ï..Year, nao_summer))+
  geom_point()+
  ggtitle("Summer NAO")+
  theme(legend.position = "none")
p9= ggplot(data=env1, aes(ï..Year, nao_winter))+
  geom_point()+
  ggtitle("Winter NAO")+
  theme(legend.position = "none")
plot_grid(p8,p9)

#pH!!#
p10= ggplot(data=env1, aes(ï..Year, ph_global))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("pH")+
  theme(legend.position = "none")
p10



#Wave Height#
p11= ggplot(data=env1, aes(ï..Year, wave_height))+
  geom_point()+
  ggtitle("Wave Height (m)")+
  theme(legend.position = "none")
p11

#Chlorophyll#
p12= ggplot(data=env1, aes(ï..Year, chlorophyll))+
  geom_point()+
  ggtitle("Chlorophyll")+
  theme(legend.position = "none")
p12



