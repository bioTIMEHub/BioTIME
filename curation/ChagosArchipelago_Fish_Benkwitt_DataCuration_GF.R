# Curation Script --------------------------------------------------------------

# Dataset: Chagos Archipelago, Fish Counts - Benkwitt
# Location: Chagos Archipelago
# Curator: Garrett Fundakowski
# Date started: 17-01-2023
# Last updated: 26-06-2023


# Set up -----------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(stringr)

# clear up the environment before starting
rm(list=ls())

# set the working directory via a pop-up window
setwd(file.choose() %>% dirname())

# read in the data from each sheet into separate dfs
dt <- read_excel('./Benkwitt_BioTimeData_Aggregated_by_Transect.xlsx', sheet=1, col_names=T, na='')
coords <- read_excel('./Benkwitt_BioTimeData_Aggregated_by_Transect.xlsx', sheet=2, col_names=T, na='')
fam <- read_excel('./Benkwitt_BioTimeData_Aggregated_by_Transect.xlsx', sheet=3, col_names=T, na='')

# quick fixes
coords$Latitude <- -(coords$South) # Latitude is in reference N, so -S
colnames(coords)[colnames(coords) == 'East'] <- 'Longitude'
colnames(dt)[colnames(dt) == 'Species'] <- 'species' # Capital Species is reserved for just species name in final df
colnames(dt)[colnames(dt) == 'Density (individuals per hectare)'] <- 'Abundance'
colnames(dt)[colnames(dt) == 'Biomass (kg/ha)'] <- 'Biomass'
dt <- dt[!(dt$Island) == "Nelson_Island",] # remove Nelson_Island as it was only sampled in the first year

# remove unnecessary columns
coords$Atoll <- NULL
coords$South <- NULL


# Structure check --------------------------------------------------------------
dim(dt) # 1401 x 13
str(dt) # notice YMD, Transect are num; will fix in a moment
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$Abundance) # TRUE
is.numeric(coords$Latitude) # TRUE
is.numeric(coords$Longitude) # TRUE
is.numeric(dt$Biomass) # TRUE
# Date should be POSIXct? 
# NA
# Year, month and day must be integers or factors?
is.factor(dt$Year) | is.integer(dt$Year) # FALSE
is.factor(dt$Month) | is.integer(dt$Month) # FALSE
is.factor(dt$Day) | is.integer(dt$Day) # FALSE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
is.factor(dt$Transect) | is.integer(dt$Transect) # FALSE
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
# NA
# Taxonomic fields must be characters or factors? 
is.factor(dt$species) | is.character(dt$species) # TRUE


# Structure fix ----------------------------------------------------------------
# convert year, month, day, transect to factor
dt$Year <- as.integer(dt$Year)
dt$Month <- as.integer(dt$Month)
dt$Day <- as.integer(dt$Day)
dt$Transect <- as.factor(paste("T",dt$Transect,sep=""))

str(dt)


# Primary field check ----------------------------------------------------------
# ABUNDANCE
# No negative values, zeroes, or NAs in abundance/biomass fields.
# From a quick look, there are NAs
# Remove NAs
dt <- dt[!is.na(dt$Abundance),]
dt <- dt[!is.na(dt$Biomass),]
#Check
min(dt$Abundance) > 0 # TRUE
sum(dt$Abundance == "") == 0 # TRUE
min(dt$Biomass) > 0 # TRUE
sum(dt$Biomass == "") == 0 # TRUE

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dt$Year) # looks good
summary(dt$Month) # looks good
summary(dt$Day) # looks good

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Join with coords df
dt <- left_join(dt,coords)


#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(60,85), ylim=c(-15,15))
points_zoom # looks good
points1 <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points_zoom1 <- points1 + coord_fixed(xlim=c(71,72.5),ylim=c(-5,-6.5))
points_zoom1 # looks good


# Secondary field --------------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed

# no plot needed as these transects are not permanent


# Taxonomic field check --------------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning
# Make Species in fam df have the same format for joining
fam$Species <- gsub(" ", "_", fam$Species, fixed=TRUE)
# Join fam df to dt by species column
dt <- left_join(dt,fam,by=c("species" = "Species"))
# Separate taxon names - species is 'Genus_species'
dt$Genus <- word(dt$species, 1, sep="_")
dt$Species <- word(dt$species, start=2, sep="_")

# check for mispellings
sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))


# Prepare raw data --------------------------------------------------------
# save the dataset name as an object
dataset.name <- 'ChagosArchipelago_Fish_Benkwitt'
# fill in sampling event with unique info
dt$SampleDescription <- as.factor(with(dt, paste(Year, Island, Transect, sep='_')))
length(levels(dt$SampleDescription)) # 88 samples

# Remove unnecessary columns
dt$Atoll <- NULL
dt$Island <- NULL
dt$Structure <- NULL
dt$species <- NULL
dt$`Coral cover` <- NULL
dt$`Count (individuals per transect)` <- NULL

dt <- dt %>% arrange(Year, Genus, Species)
# now create empty columns needed to fit to template
dt$StudyID <- rep('', dim(dt)[1])
dt$Plot <- rep('', dim(dt)[1])
dt$DepthElevation <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance),Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? no, 0

# reorder columns by BioTIME format
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

# final check
View(dt_merged)
summary(dt_merged)
str(dt_merged)


# Spatial Geometry Calculations for BioTIME datasets --------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

# Plot the geometries
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(71,72.5), ylim = c(-5,-6.5)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()


# Export final ------------------------------------------------------------

setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(dataset.name, '_rawdata_GF.csv'), row.names=F)
