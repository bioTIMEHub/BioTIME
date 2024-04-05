### Biotime v1 fix of Thorn fungi
## Curator: VB
## datasets split in amend but remerged using treatment info
## Ago 2023

# load libraries
library(sf)
library(clipr)
library(clipr)
library(readxl)
library(tidyverse)
library(ggplot2)

rm(list=ls())

setwd("~/Documents/BioTIME/amend")
dt_ul <- read_excel("BioTIMETemplate_fungi_unlogged.xlsx", sheet = "rawData") 
dt_l <- read_excel("BioTIMETemplate_fungi_logged.xlsx", sheet = "rawData") 
length(unique(dt_l$Plot))
length(unique(dt_l$Latitude))
length(unique(dt_l$Longitude))
length(unique(dt_ul$Plot))
length(unique(dt_ul$Latitude))
length(unique(dt_ul$Longitude))

unique(dt_l$Plot) %in% unique(dt_ul$Plot)
dt_l$treat <- "logged"
dt_ul$treat <- "unlogged"

dt <- rbind (dt_l,dt_ul)

dt$SampleDescription<- paste(dt$treat,dt$Plot,dt$Year, sep = "_")

dt$Plot <- NA
dt <- dt[,1:14]
summary(dt)

write.csv(dt,"Thorn_fungi_recurate_merged_VB.csv")
write_clip(dt)


### Spatial Geometry Calculations

dt_coord <- dt %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# get centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

# Plot the geometries

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()



