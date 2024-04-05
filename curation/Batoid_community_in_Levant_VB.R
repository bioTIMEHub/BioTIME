# Curation Script ---------------------------------------------------------

# Dataset:  Batoid community in Levant
# Location: Israel
# Curator: Viviana Brambilla
# Note* not a whole lot to clean bc contributor filled out template

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# clear environment
rm(list =ls())

# make sure your working directory is set before running these lines
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/BelmakerLab")
dt <- readxl::read_excel("BioTIMEContributorTemplate_shahar_chaikin_ago22.xlsx", 
                         sheet=2, col_names=T, na='')
dt <- as.data.frame(dt[,1:14])

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

summary(dt$Abundance) # check the minimum (no zeroes)
dt <- dt[dt$Abundance>0,]
summary(dt$Abundance) # check the minimum (no zeroes)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dt[,11:13]) # looks good, only year

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct
# seems fine

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean names

sort(unique(dt$Family))
summary(as.factor(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
dt$Species <- word(dt$Species,2)
sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))


# Prepare raw data --------------------------------------------------------
dt$Abundance
dt$Biomass <- NA

table(dt$Plot, dt$Latitude)
table(dt$Plot, dt$Longitude)

# aggregate abundance records that are same species and sampling event.
dt$SampleDescription <- as.factor(paste(dt$Plot,dt$Year, dt$Month, dt$Day, sep = "_"))
dt_merged <- dt %>% group_by(Genus,Species,SampleDescription) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()
nrow(dt) - nrow(dt_merged) # it was already aggregated fine
dt_merged <- dt[c('Abundance',
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
                  'StudyID')] %>% arrange(Year, Plot, Genus, Species)
dt_merged$Plot <- NA
dt_merged$StudyID <- NA
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Batoid_communities_in_Levant_rawdata_VB.csv', row.names=F)
library(clipr)
write_clip(dt_merged)


library(sf)
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area) # this is just to check against what the author inputted



