# Curation Script ---------------------------------------------------------

# Dataset: Landis_2018 (Insect Population Dynamics on the Main Cropping System Experiment at the Kellogg Biological Station, Hickory Corners, MI  (1989 to 2017).)
# Location: Michigan
# Contact: Data Manager Kellogg Biological Station  - lter.data.manager@kbs.msu.edu
# Curator: James Cant
# Date: 29-Jun-2023

# ------------------------------------------------------------------

# load the necessary packages
library(readxl)
library(reshape2)
library(stringr)
require(tidyverse)
require(maps)
require(stringr)
require(dplyr)
require(sf)
require(clipr)

###########################
# STEP 1: Download and open dataset
###########################

# clear workspace
rm(list=ls())

# assign book-keeping details.
data.name <- 'Landis_2018'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Landis 2018/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# define file name
infile <- paste0(mypath, 'Landis_2018_OriginalData')
# Run download
if(!dir.exists(mypath) || !file.exists(infile))   {
  dir.create(mypath,  showWarnings = FALSE)
  inUrl <- "https://pasta.lternet.edu/package/data/eml/knb-lter-kbs/23/26/8d33fa9169147f266d20bdcd09a07820"
  download.file(inUrl, infile, method="curl")
}

# Open data file
dat <-read.csv(infile, header=F
               ,skip=29
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "Sample_Date",
                 "Treatment",
                 "Replicate",
                 "Station",
                 "Species",
                 "Family",
                 "Order",
                 "Adults",
                 "utm_easting",
                 "utm_northing",
                 "Year"    ), check.names=TRUE)

###########################
# STEP 2: Clean and reformat data variables
###########################

### Clean Abundance (remove NAs) -----------------------
names(dat)[8] <- 'Abundance'
dat <- dat[!(is.na(dat$Abundance)),]
# check to confirm
min(dat$Abundance) > 0 # no zeroes?
dat <- dat[dat$Abundance > 0,] # remove zero entries
sum(dat$Abundance == "" | dat$Abundance == ' ') == 0 # no blanks?
dat$Abundance <- as.numeric(dat$Abundance)

### Clean site/plot details ---------------------------------
dat$Site <- as.factor(rep('Kellogg Biological Station', nrow(dat)))
dat$Plot <- rep(NA, nrow(dat))
dat$Treatment <- as.factor(dat$Treatment)
dat$Station <- as.factor(dat$Station)
dat$Replicate <- as.factor(dat$Replicate)

### Clean Date info ------------------------------------
dat$Year <- as.factor(dat$Year)
dat$Day <- as.factor(substr(dat$Sample_Date, 9,10))
dat$Month <- as.factor(substr(dat$Sample_Date, 6,7))
dat$Sample_Date <- NULL

### Clean Taxonomy -------------------------------------
# Clean Family
sort(unique(dat$Family))
dat$Family[dat$Family == ''] <- NA
dat$Family[dat$Family == 'Lacewing'] <- NA # Lacewings cover multiple families
# condense order details into family variable in instances where no family information given
dat[is.na(dat$Family),]$Family <- dat[is.na(dat$Family),]$Order
# reconvert missing entries to NA
dat$Family[dat$Family == ''] <- NA
dat$Family <- as.factor(dat$Family)
# remove order variable
dat$Order <- NULL
# Separate species variable into genus and species
# Genus
sort(unique(dat$Species))
dat$Genus <- word(dat$Species, 1)
sort(unique(dat$Genus))
dat$Genus[dat$Genus == 'Lacewing'] <- 'Unknown'
dat$Genus[dat$Genus == 'Other'] <- 'Unknown'
dat$Genus <- as.factor(dat$Genus)
# Species
dat$Species <- word(dat$Species, 2)
sort(unique(dat$Species))
# add in correct terminology for unidentified species
dat$Species[dat$Genus == 'Unknown'] <- 'sp'
dat$Species <- as.factor(dat$Species)

# ADD SPATIAL INFORMATION ---------------------------------------------------
# No elevation data provided
dat$DepthElevation <- NA
# The study provides a boundary box for the study location so it is nessecary to identify a central GPS location
dt_coords <- data.frame(Longitude = c(-85.404699, -85.366857, -85.366857, -85.404699),
                        Latitude = c( 42.420265,  42.420265,  42.391019,  42.391019))
# Convert data points into point spatial object
dt_coord <- dt_coords %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
# add latitude and longitude to data
dat$Longitude <- rep(centroid[1], dim(dat)[1])
dat$Latitude <- rep(centroid[2], dim(dat)[1])
# remove uneeded details
dat$utm_easting <- NULL
dat$utm_northing <- NULL

# Check the coordinates match with the expected location of the data
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dat %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-100,-50), ylim=c(20,50))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# PREPARE RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, site, transect, and survey day.
dt_merged <- dat %>% group_by(Latitude, Longitude, DepthElevation, Site, Treatment, Station, Replicate, Plot, Year, Month, Day, Family, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Treatment, Station, Replicate, Family, Genus, Species)
nrow(dat) - nrow(dt_merged) # any change in aggregating?
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Site, Treatment, Station, Replicate, Year, Month, Day, sep='_')))
length(levels(dt_merged$SampleDescription)) 

# Remove variables not contained within BIOTIME format
dt_merged$Site <- NULL
dt_merged$Treatment <- NULL
dt_merged$Station <- NULL
dt_merged$Replicate <- NULL

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
                         'StudyID')] %>% arrange(Year, Month, Day, Family, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(fileSave, data.name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_merged)

# -------------------------------------------- End of Code ------------