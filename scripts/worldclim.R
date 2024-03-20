

# CLIMATIC CHARACTERISATION OF HOSPITAL

library(tidyverse)
library(terra)

hospital <- data.frame(site='Hospital', y=40.654678, x=-4.001029) %>% vect(geom=c('x','y'))
crs(hospital) <- 'EPSG:4326'

wc <- rast(list.files('C:/Users/user/Desktop/worldclim/wc2.1_30s_bio', full.names=T))


plot(wc$wc2.1_30s_bio_1)
points(hospital)


wc %>% extract(hospital) %>% t()

# BIO1 = Annual Mean Temperature
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
