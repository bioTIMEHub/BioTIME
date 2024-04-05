setwd("D:/sfuvault/SFU/MooersLab/Ch1_MacroPD/BioTIME/Additional data/OpWall/Hoga")
rm(list=ls())

#### LOAD PACKAGES ####
library(dplyr)
library(tidyverse)
library(readxl)
library(measurements)

library(maps)
library(sp)
library(rgeos)
library(geosphere)
library(clipr)

#### LOAD DATA ####
hoga <- t(read_excel("Edited_Hoga_UVC_2002-2012.xlsx", col_names = FALSE))
meta <- data.frame(species = hoga[2,6:length(hoga[1,])],
                   family  = hoga[1,6:length(hoga[1,])])
coord <- read.csv("hoga_sitescoord.csv")

#### SAMPLING TERMS ####

# Summary

## SITE: The survey site. See "coord" data for abbreviation, full name and GPS locations of each site.

## ZONE: Flat (F), Crest (C) or Slope (S). Flat is typically 0-3 meters deep, Crest is 3-8 meters, Slope is 8-15 meters.

## TRANSECT: The transect (replicate) that the individual fish was recorded on. Each Site has 3 ZONE levels and 
#            each ZONE level has multiple replicated transects.


#### WRANGLE DATA ####
# Check data
colnames(hoga) #error
sort(unique(meta$species)) # some IDs are to genus level only

# Retrieve column names
colnames(hoga) <- hoga[2,]

# keep only cells with data
hoga <- hoga[3:length(hoga[,1]),-5]

# remove row names
rownames(hoga) <- NULL

# Convert species by transect (LARGE FORMAT) to individual observations (LONG FORMAT)
hoga <- as.data.frame(hoga) %>%
  gather("species", "abundance", 5:dim(hoga)[2])

hoga <- hoga[-which(is.na(hoga$abundance)),] # remove zeros (not observed)

# remove/replace characters in abundance column
sort(unique(hoga$abundance))
hoga$abundance[hoga$abundance == "1?"] <- NA    # conservative
hoga$abundance[hoga$abundance == "`"] <- NA     # typo?
hoga$abundance[hoga$abundance == "1(jv)"] <- 1  # observed one individual
hoga$abundance[hoga$abundance == "1`"] <- 1     # typo

# Replace degree by decimal coordinates
coord$coordinates.mod <- gsub('°', ' ', coord$coordinates)
lat <- unlist(lapply(strsplit(coord$coordinates.mod, "  "), function(x) x[1]))
lon <- unlist(lapply(strsplit(coord$coordinates.mod, "  "), function(x) x[2]))

ewns1 <- ifelse(str_extract(lat, "\\(?[EWNS,]+\\)?") %in% c("E","N"),"+","-")
dms1 <- str_sub(lat,1,str_length(lat)-1)
lat <- paste0(ewns1,dms1)

coord$lat.dec <- as.numeric(measurements::conv_unit(lat, from = 'deg_dec_min', to = 'dec_deg'))

ewns2 <- ifelse(str_extract(lon, "\\(?[EWNS,]+\\)?") %in% c("E","N"),"+","-")
dms2 <- str_sub(lon,1,str_length(lon)-1)
lon <- paste0(ewns2,dms2)

coord$lon.dec <- as.numeric(measurements::conv_unit(lon, from = 'deg_dec_min', to = 'dec_deg'))

#### CREATE DATA ####
# Create empty data frame
dt <- data.frame(Abundance = rep('', dim(hoga)[1]),	
                 Biomass = rep(NA, dim(hoga)[1]), # not available
                 Family = rep(NA, dim(hoga)[1]), 
                 Genus = rep('', dim(hoga)[1]),
                 Species = rep('', dim(hoga)[1]),
                 SampleDescription = rep('', dim(hoga)[1]), # leave blank
                 Plot = rep('', dim(hoga)[1]),
                 Latitude = rep('', dim(hoga)[1]),
                 Longitude = rep('', dim(hoga)[1]),
                 DepthElevation = rep(NA, dim(hoga)[1]), # not available
                 Day = rep('', dim(hoga)[1]), # not available
                 Month = rep('', dim(hoga)[1]), # not available
                 Year = rep('', dim(hoga)[1]),
                 StudyID = rep('', dim(hoga)[1]) # leave blank
)


## Fill in the easy ones
dt$Abundance <- hoga$abundance
dt$Family <- meta$family[match(hoga$species, meta$species)]
dt$Genus <- word(hoga$species, 1)
dt$Species <- word(hoga$species, 2, -1) # from second word to end of string

## Plot
# Plot as a concatenation of "Site" + "Zone" + "Transect" (as per Vivi's suggestion)
# each plot is a unique ID of transect per depth per site
dt$Plot <- paste(hoga$SITE, hoga$ZONE, hoga$TRANSECT, sep = "_")

## Latitude, Longitude
# check for outliers (also see map below)
range(coord$lon.dec)
range(coord$lat.dec)

# retrieve lat/lon for each Site, matching hoga data
dt$Latitude <-  coord$lat.dec[match(hoga$SITE, coord$site.code)]
dt$Longitude <- coord$lon.dec[match(hoga$SITE, coord$site.code)]

# Any NAs? 
length(which(is.na(dt$Latitude)))  # Nope.
length(which(is.na(dt$Longitude))) # Nope.

## Year
dt$Year <- hoga$YEAR

# Order by Site (through Plot)
dt <- dt %>%
  arrange(Plot)


#### SAVE ####
write.csv(dt, "fish_OpWall_Hoga_rawData.csv", row.names = FALSE)


#### MAP DATA ####

# Convert data points into point spatial object
points.study <- SpatialPoints(cbind(dt$Longitude, dt$Latitude))
proj4string(points.study) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Calculate convex hull
convhull.study <- gConvexHull(points.study)

# Get area (km2)
geosphere::areaPolygon(convhull.study)/1000000

# Get central coordinates
(centroid.study <- as.data.frame(gCentroid(convhull.study)@coords)) 

# Map it to check
rx <- range(coord$lon.dec, na.rm = TRUE)
ry <- range(coord$lat.dec, na.rm = TRUE)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='grey90') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

world + 
  geom_point(data=coord, aes(x = lon.dec, y = lat.dec), shape=19, alpha = 0.8, size = 3, color = "grey40") + # sampling sites
  geom_point(data = centroid.study, aes(x = x, y = y), shape = 18, color = "red3", size = 6) + # centroid
  coord_fixed(xlim=c(rx[1],rx[2]), ylim=c(ry[1],ry[2]))

