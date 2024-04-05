# Curation Script ---------------------------------------------------------

# Dataset: USGS amphibians dataset
# Curator: PF, check by VB
# Date: July 2022

# Set up ------------------------------------------------------------------

## packages	
library(dplyr)
library(tidyverse)
library(maps)
library(readxl)
library(sp)
library(rgeos)
library(clipr)

# clean slate
rm(list =ls())
setwd("~/Documents/BioTIME/Re_ USGS amphibians dataset")

## data
coord <- read.csv("Coordinates.csv")
counts <- read.csv("Counts.csv")
protocol <- read.csv("Protocol.csv")
runs <- read.csv("Runs.csv")
species <- read.csv("Species.csv")
stops <- read.csv("Stops.csv")

# Create empty data frame
dt <- data.frame(Abundance = rep('', dim(counts)[1]),	
                 Biomass = rep(NA, dim(counts)[1]), # not available
                 Family = rep(NA, dim(counts)[1]), # not available
                 Genus = rep('', dim(counts)[1]),
                 Species = rep('', dim(counts)[1]),
                 SampleDescription = rep('', dim(counts)[1]), # leave blank
                 Plot = rep('', dim(counts)[1]),
                 Latitude = rep('', dim(counts)[1]),
                 Longitude = rep('', dim(counts)[1]),
                 DepthElevation = rep(NA, dim(counts)[1]), # not available
                 Day = rep('', dim(counts)[1]),
                 Month = rep('', dim(counts)[1]),
                 Year = rep('', dim(counts)[1]),
                 StudyID = rep('', dim(counts)[1]) # leave blank
)

# Check data
colnames(counts)
sort(unique(counts$Species)) # some IDs as part of a species complex

#### Sampling terms ####

# Summary
# Data were collected from 10 roadside wetlands ('stops' separated by at least 0.8km) 
# along routes at least 8km long from across the 26 collaborating states, 
# predominantly occurring on unprotected areas (i.e. very few stops were located on federal lands).

# RunID
# Unique auto-generated integer assigned to each survey occasion (i.e., datasheet). 
# Use to relate all tables together. Each Run ID appears only once in the Runs table (primary key)
# PFF: This is a sampling event, i.e. there can be multiple RunID for a given RouteNumber in a year.

# RouteNumber
# A unique numeric identifier for each route (i.e. survey path)

# StopNumber
# Integer value, 1-10, identifying which stop along the route survey data are associated with. 
# Each route has 10 stops during any given survey and a site's StopNumber can change from year to year 
# and even from run to run, if route alterations are made, but a site's SiteID remains constant (see SiteID attribute).

# SiteID
# A unique numeric identifier for each stop (i.e. survey site) of each route (i.e. survey path)

#### Abundance term ####

# CallingIndex
# Integer value (1-3) rating the calling intensity for each species heard at a listening location.
# 1 = Individuals can be counted; there is space between calls
# 2 = Calls of individuals can be distinguished but there is some overlapping of calls
# 3 = Full chorus, calls are constant, continuous and overlapping

#### Map of sites ####
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
(points <- world + geom_point(data=coord, aes(x=lon, y=lat), shape=21)) # There's an outlier in China

# Zoomed in to check. Multiple coordinates (stops) per RouteNumber
(points_zoom <- points + coord_fixed(xlim=c(-80,-75), ylim=c(35,40)))


#### Fill in the easy ones ####
dt$Abundance <- counts$CallingIndex
dt$Genus <- word(counts$Species, 1)
dt$Species <- word(counts$Species, 2, -1) # from second word to end of string

#### Plot ####
# SiteID matching the counts data
counts.stop <- paste(counts$RunID, counts$StopNumber, sep = "_")
stops.stop <- paste(stops$RunID, stops$StopNumber, sep = "_")

n_distinct(counts.stop)
n_distinct(stops.stop) # many more (never added to counts df?)

plot1 <- stops$SiteID[match(counts.stop, stops.stop)]

# Plot as a concatenation of "Run" + "stopID" (as per Cher's suggestion)
dt$Plot <- paste(counts$RunID, plot1, sep = "_")

#### Date ####
# retrieve date for each RunID, matching counts data
date1 <- runs$SurveyDate[match(counts$RunID, runs$RunID)]

dt$Day <- unlist(lapply(str_split(date1, pattern = "/"), function(x) x[2]))
dt$Month <- unlist(lapply(str_split(date1, pattern = "/"), function(x) x[1]))
dt$Year <- unlist(lapply(str_split(date1, pattern = "/"), function(x) x[3]))

#### Latitude, Longitude ####
# check for outliers (also see map)
range(coord$lon)
range(coord$lat)

# correct outlier coordinates (all longitudes should be negative, i.e. in North America)
coord[coord$lon > 0, "lon"] <- -coord[coord$lon > 0, "lon"] # all good now

# retrieve lat/lon for each SiteID, matching counts data
dt$Latitude <- coord$lat[match(plot1, coord$SiteID)]
dt$Longitude <- coord$lon[match(plot1, coord$SiteID)]

# Solve NAs
length(which(is.na(dt$Latitude)))

# inspect specific case: RunID = 158, SiteIDE = 9592, RouteNumber = 420516
counts[counts$RunID == 158,]
stops[stops$RunID == 158,]
runs[runs$RunID ==158,]

# no coordinates for this Run...
coord[coord$SiteID == 9592,]
which(coord$RouteNumber == 420516) 

# Remove rows without coordinates
dt <- dt[!is.na(dt$Latitude),]

#### SAVE ####
write.csv(dt, "USGS_NAAMP_Amphibians_PFF_rawData.csv", row.names = FALSE)


#### META DATA ####

# Convert data points into point spatial object
points.study <- SpatialPoints(cbind(dt$Longitude, dt$Latitude))
proj4string(points.study) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Calculate convex hull, area and centroid
convhull.study <- gConvexHull(points.study)

# Get area (km2)
(geosphere::areaPolygon(convhull.study)/1000000)

# Get central coordinates
(centroid.study <- as.data.frame(gCentroid(convhull.study)@coords)) 

# Map it to check
rx <- range(coord$lon)
ry <- range(coord$lat)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='grey90') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

world + 
  geom_point(data=coord, aes(x = lon, y = lat), shape=19, alpha = 0.1, color = "grey40") + # sampling sites
  geom_point(data = centroid.study, aes(x = x, y = y), shape = 18, color = "red3", size = 6) + # centroid
  coord_fixed(xlim=c(rx[1],rx[2]), ylim=c(ry[1],ry[2]))

# Looks good


## VB checks ##


dt <- read.csv("USGS_NAAMP_Amphibians_PFF_rawData.csv")
dt$SampleDescription <- paste0(dt$Plot,"_",dt$Year)
dt$Plot <- NA

#aggregate abundance records that are same species, survey, day and site.

sort(unique(dt$Species))
dt$Species <- word(dt$Species, 1) # from second word to end of string
sort(unique(dt$Species))

dt$Species[dt$Species == "chrysoscelis/versicolor"] <- "sp1"
dt$Species[dt$Species == "americanus/terrestris"] <- "sp2"
dt$Species[dt$Species == "crepitans/gryllus"] <- "sp3"
dt$Species[dt$Species == "feriarum/fouquettei"] <- "sp4"
dt$Species[dt$Species == "feriarum/kalmi"] <- "sp5"
dt$Species[dt$Species == "feriarum/triseriata"] <- "sp6"
dt$Species[dt$Species == "fowleri/woodhousii"] <- "sp7"
dt$Species[dt$Species == "maculata/feriarum"] <- "sp8"
dt$Species[dt$Species == "maculata/triseriata"] <- "sp9"


dt_merged <- dt %>% group_by(Abundance, Biomass, Family, Genus,
                             Species, SampleDescription, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dt_merged$StudyID <- ""

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?


dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
write_clip(dt_merged)

write.csv(dt_merged, "USGS_NAAMP_Amphibians_PFF_VB_rawData.csv", row.names = FALSE)

# load libraries
library(sf)
library(clipr)
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


