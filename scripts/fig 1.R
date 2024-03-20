

# FIGURE 1

library(mapSpain)
library(terra)
library(tidyverse)
library(maps)


df_points <- data.frame(site=c('Ciudad Universitaria', 'Atocha', 'Collado Villalba', 'VILL', 'ARAN', 'FACF', 'ROZA'),
                        type=c('Flowering phenology', 'Flowering phenology', 'Study site', 'PALINOCAM', 'PALINOCAM', 'PALINOCAM', 'PALINOCAM'),
                        pch=c(1,1,17,4,4,4,4),
                        lat=c(40.445446, 40.406152, 40.653837, 40.653837, 40.02955, 40.44547, 40.49726),
                        lon=c(-3.726073, -3.686379, -4.001314, -4.001314, -3.60690, -3.72643, -3.88043))

pts_points <- vect(df_points, geom=c('lon','lat'))


v_madrid <- esp_get_prov(prov='Madrid') %>% vect() %>% project('epsg:4326')


plot(v_madrid)
points(pts_points[1:3], pch=df_points$pch[1:3], cex=1.5)
points(pts_points[4:7], pch=df_points$pch[4:7], cex=1.5, col='blue')
text(df_points$lon[1:3], df_points$lat[1:3], labels=df_points$site[1:3], cex=0.8, pos=2)
text(df_points$lon[4:7], df_points$lat[4:7], labels=df_points$site[4:7], cex=0.8, pos=3, col='blue')
map.scale(x=-4.5, y=41.1, metric=T, relwidth=0.3, ratio=F, cex=0.65)

