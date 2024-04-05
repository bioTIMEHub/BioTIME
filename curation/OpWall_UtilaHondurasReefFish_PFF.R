setwd("D:/sfuvault/SFU/MooersLab/Ch1_MacroPD/BioTIME/Additional data/OpWall/Honduras")
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
hondu <- read_excel("Honduras_SVS_MASTER_no_formulae.xlsx", col_names = TRUE)

meta <- data.frame(genus = hondu$Genus,
                   species = hondu$Species,
                   fullname = hondu$Full_name,
                   family  = hondu$Family)
sites <- read.csv("honduras_sitecodes.csv", stringsAsFactors = FALSE) # made by PFF (BioTIME curator) in Excel (it was messy)
species <- read.csv("honduras_speciesmeta.csv", stringsAsFactors = FALSE)


#### SAMPLING TERMS ####

# Summary

## LOCATION: Broad geographical area. Either Utila (island) or Tela (mainland coastline)

## SITE: The survey site. See "sites" data for abbreviation, full name and GPS locations of each site.

## DEPTH: Depth of transect. Either 5m, 10m or 15m.

## TRANSECT: The transect (replicate) that the individual fish was recorded on. Each Site has 3 depth levels and 
#            each depth level has multiple replicated transects.


#### WRANGLE DATA ####
# Check data

# Are all species in metadata in the raw data?
all(species$Full_name %in% hondu$Full_name)

# Are all species in raw data in the metadata?
unique(hondu$Full_name[which(!hondu$Full_name %in% species$Full_name)]) # some typos

hondu$Full_name[hondu$Full_name == "abudefduf saxatilis"] <- "Abudefduf saxatilis"
hondu$Full_name[hondu$Full_name == "Canthidermis Sufflamen"] <- "Canthidermis sufflamen"
# All good now

#Species names 
sort(unique(hondu$Full_name)) # some IDs are to genus level only

# Modify genus-level entries to match BioTIME taxonomy
hondu$Full_name <- gsub(" spp", " sp", hondu$Full_name)

# Checking Abundance column
class(hondu$Number)
sort(unique(hondu$Number))

# Checking Biomass column
class(hondu$`Biomass(g)`)
range(hondu$`Biomass(g)`) # 15716g seems pretty high
hondu[hondu$`Biomass(g)` == max(hondu$`Biomass(g)`),] # Looks like it's a school of 50 1m-long barracudas...

# Coordinates
sites$coordinates # Missing coordinates for some sites were added by PFF (See sites$Note)

# Split into two columns
sites$lat <- as.numeric(unlist(lapply(strsplit(sites$coordinates, " "), function(x) x[2]))) # Positive because North
sites$lon <- as.numeric(unlist(lapply(strsplit(sites$coordinates, " "), function(x) paste0("-", x[4]))))# Negative because West

# Add lat/lon to raw data
hondu$lat <- sites$lat[match(hondu$Site, sites$sitecode)]
hondu$lon <- sites$lon[match(hondu$Site, sites$sitecode)]

# REMOVE ROWS WITHOUT COORDINATES :(
# how many rows to remove?
length(which(is.na(hondu$lat)))

hondu <- hondu[!is.na(hondu$lat),] 

## PLOT
# Plot as a concatenation of "Site" + "Depth" + "Transect" (as per Vivi's suggestion)
# each plot is a unique ID of transect per depth per site
hondu$plot <- paste(hondu$Site, hondu$Depth, hondu$Transect, sep = "_")

# COMBINE/SUM MULTIPLE ENTRIES IN SAME TRANSECT
hondu.comb <- hondu %>%
  group_by(Family, Full_name, Site, plot, Year, Depth, lat, lon) %>%
  dplyr::summarise(abund = sum(Number),
                   biomass = sum(`Biomass(g)`)
                   ) %>%
  ungroup()


#### CREATE DATA ####
# Create empty data frame
dt <- data.frame(Abundance = rep('', dim(hondu.comb)[1]),	
                 Biomass = rep(NA, dim(hondu.comb)[1]),
                 Family = rep(NA, dim(hondu.comb)[1]), 
                 Genus = rep('', dim(hondu.comb)[1]),
                 Species = rep('', dim(hondu.comb)[1]),
                 SampleDescription = rep('', dim(hondu.comb)[1]), # leave blank
                 Plot = rep('', dim(hondu.comb)[1]),
                 Latitude = rep('', dim(hondu.comb)[1]),
                 Longitude = rep('', dim(hondu.comb)[1]),
                 DepthElevation = rep(NA, dim(hondu.comb)[1]),
                 Day = rep('', dim(hondu.comb)[1]), # not available
                 Month = rep('', dim(hondu.comb)[1]), # not available
                 Year = rep('', dim(hondu.comb)[1]),
                 StudyID = rep('', dim(hondu.comb)[1]) # leave blank
)


## Fill in the easy ones
dt$Abundance <- hondu.comb$abund
dt$Biomass <- hondu.comb$biomass
dt$Family <- hondu.comb$Family
dt$Genus <- word(hondu.comb$Full_name, 1)
dt$Species <- word(hondu.comb$Full_name, 2, -1) # from second word to end of string
dt$Plot <- hondu.comb$plot

## Latitude, Longitude
# check for outliers (also see map below)
range(sites$lon, na.rm = TRUE)
range(sites$lat, na.rm = TRUE)

# retrieve lat/lon for each Site, matching hondu data
dt$Latitude <-  sites$lat[match(hondu.comb$Site, sites$sitecode)]
dt$Longitude <- sites$lon[match(hondu.comb$Site, sites$sitecode)]

# Any NAs? 
length(which(is.na(dt$Latitude)))  # Nope.
length(which(is.na(dt$Longitude))) # Nope.

## DepthElevation (Depth)
dt$DepthElevation <- hondu.comb$Depth

## Year
dt$Year <- hondu.comb$Year

#### SAVE ####
write.csv(dt, "fish_OpWall_honduras_rawData.csv", row.names = FALSE)


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
rx <- range(sites$lon, na.rm = TRUE)
ry <- range(sites$lat, na.rm = TRUE)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='grey90') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

world + 
  geom_point(data=sites, aes(x = lon, y = lat), shape=19, alpha = 0.8, size = 3, color = "grey40") + # sampling sites
  geom_point(data = centroid.study, aes(x = x, y = y), shape = 18, color = "red3", size = 6) + # centroid
  coord_fixed(xlim=c(rx[1],rx[2]), ylim=c(ry[1],ry[2]))

