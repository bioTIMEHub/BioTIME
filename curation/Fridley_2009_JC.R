# Curation Script ---------------------------------------------------------

# Dataset: Fridley_2009 (Point Quadrat Vegetation Data, Buxton Climate Change Experiment, UK, 1994-2009)
# Location: Buxton, UK
# Curator: James Cant
# Date: 27-Jun-2023

# ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(stringr)
require(dplyr)
require(sf)
require(clipr)

# clear workspace
rm(list=ls())

# assign book-keeping details.
data.name <- 'Fridley_2009'
curator <- 'JC'
# identify the folder that the data is/will be located in
mypath <- 'E:/BioTime/Fridley 2009/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'

### DOWNLOAD DATA ---------------------------
# Identify file pathway 
infile1csv <- paste0(mypath, 'doi 10.5063_AA_fridley.5.1.data.csv')
if(!dir.exists('E:/BioTime/Fridley 2009/') || !file.exists(infile1csv)){
  dir.create('E:/BioTime/Fridley 2009/', showWarnings = FALSE)
  infile1 <- paste0(mypath, 'doi 10.5063_AA_fridley.5.1.data')
  inURL1 <- 'https://knb.ecoinformatics.org/knb/d1/mn/v2/object/doi%3A10.5063%2FAA%2Ffridley.5.1'
  download.file(inURL1, infile1, method="curl")
  file.append(infile1csv, infile1)
}

# READ IN DATA ---------------------------------------------------

# read in data
dt <- read.csv(paste0(mypath, 'doi 10.5063_AA_fridley.5.1.data'))
View(dt)
# This file contains annual estimates of biomass of approximately 225 taxa of reef algae, invertebrates and fish in permanent transects at 11 kelp forest sites in the Santa Barbara Channel.

#### REMOVE ARTIFICIAL TREATMENTS -----------------------------------
dt <- dt[dt$treatment == 'control',]
#treatment variable can now be dropped
dt$treatment <- NULL

### REFORMAT VARIABLES ------------------------------------
# Locations sampled continuously (i.e. they are permanent plots)
names(dt)[2] <- 'Plot'; dt$block <- NULL # without the different treatments block and plot are repetitive
dt$Plot <- as.factor(dt$Plot)

# Sort date information
dt$Year <- as.factor(dt$year); dt$year <- NULL
# Add in missing information
dt$Month <- rep(NA, dim(dt)[1])
dt$Day <- rep(NA, dim(dt)[1])

# Sort abundance data
names(dt)[3] <- 'Abundance'
dt$Abundance <- as.numeric(dt$Abundance) # ensure data is numeric
min(dt$Abundance) > 0 # no zeroes?
sum(dt$Abundance == "" | dt$Abundance == ' ') == 0 # no blanks?

# REFORMAT TAXONIMIC DETAILS ---------------------------------------------------
# Split genus and species
dt$Genus <- word(dt$species, 1)
dt$Species <- word(dt$species, 2)
sort(unique(dt$Genus))
sort(unique(dt$Species)) # Manually inspect classifications
# reassign missing species names
dt[which(is.na(dt$Species) & !(is.na(dt$Genus))),]$Species <- 'sp'
dt$Genus <- as.factor(dt$Genus); dt$Species <- as.factor(dt$Species)
dt$species <- NULL # Remove old variable

# ADD SPATIAL INFORMATION ---------------------------------------------------
# No elevation data provided
dt$DepthElevation <- NA
# The study provides a boundary box for the study location so it is nessecary to identify a central GPS location
dt_coords <- data.frame(Longitude = c(-1.92, -1.91808, -1.92, -1.91808),
                        Latitude = c(53.25, 53.25, 53.25, 53.25))
# Convert data points into point spatial object
dt_coord <- dt_coords %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
# add latitude and longitude to data
dt$Longitude <- rep(centroid[1], dim(dt)[1])
dt$Latitude <- rep(centroid[2], dim(dt)[1])

# Check the coordinates match with the expected location of the data (Buxton)
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-5,0), ylim=c(50,55))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# PREPARE CURATED RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, Site, plot, and survey day.
dt_merged <- dt %>% group_by(Latitude, Longitude, DepthElevation, Plot, Year, Month, Day, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Plot, Year, Month, Day, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 0 records condensed
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Family <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(data.name, Latitude, Longitude, Plot, Year, sep='_')))
length(levels(dt_merged$SampleDescription))

# reorder columns to match BioTIME format
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
                         'StudyID')] %>% arrange(Year, Month, Day, Plot, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(fileSave, data.name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_merged)

# -------------------------------------------- End of Code ------------