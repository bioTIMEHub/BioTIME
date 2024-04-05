# Curation Script ---------------------------------------------------------

# Dataset:  Norvegian Scandes MIREN
# Location: Norway
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
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Clavel")
dt <- readxl::read_excel("BioTIMETemplateJune20 - Jan Clavel.xlsx", 
                          sheet=1, col_names=T, na='')
dt <- as.data.frame(dt[,1:14])

summary(dt)
# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

# convert categories into numeric
unique(dt$Abundance) 
dt$Abundance[is.na(dt$Abundance)] <- 0.5
dt$Abundance[dt$Abundance =="100+"] <- 100
dt$Abundance[dt$Abundance =="10-100"] <- 55
dt$Abundance[dt$Abundance =="1-10"] <- 5
dt$Abundance <- as.numeric(dt$Abundance)  
summary(dt$Abundance) # this is too coarse.

unique(dt$Biomass) 
dt$Biomass[dt$Biomass =="75-95%"] <- 85
dt$Biomass[dt$Biomass =="50-75%"] <- 67.5
dt$Biomass[dt$Biomass =="25-50%"] <- 42.5
dt$Biomass[dt$Biomass =="5-25%"] <- 15
dt$Biomass[dt$Biomass =="1-5%"] <- 3.5
dt$Biomass[dt$Biomass =="0-1%"] <- 0.5

dt$Biomass <- as.numeric(dt$Biomass)  
summary(dt$Biomass) # check the minimum (no zeroes)


summary(dt$Biomass) # check the minimum (no zeroes)
summary(dt$Biomass) # check the minimum (no zeroes)


# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

dt$Day <- as.numeric(dt$Day)
dt$Month <- as.numeric(dt$Month)
dt$Year <- as.numeric(dt$Year)
summary(dt[,11:13]) # looks good

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)
dt <- dt[!is.na(dt$Latitude),]
unique(dt$Longitude)

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
summary(as.factor(dt$Species))
dt[(is.na(dt$Species)),]
dt$Species[dt$Species == "NS2"] <- "sp"
dt$Species[dt$Species == "NS"] <- "sp"
dt$Species[dt$Species == "sect. Oreadea"] <- "Oreadea"
dt$Species[dt$Species == "sect. Alpina"] <- "Alpina"
dt$Species[dt$Species == "sect. Subalpina"] <- "Subalpina"
dt$Species[dt$Species == "sect. Vulgata"] <- "Vulgata"
dt$Species[dt$Species == "multiflora subsp. frigida"] <- "multiflora"
dt$Species[dt$Species == "magellanica subsp. irrigua"] <- "magellanica"
dt$Species[dt$Species == "alpestris subsp. lapponicus"] <- "alpestris"
sort(unique(dt$Species))
summary(as.factor(dt$Species))
#ok

# Prepare raw data --------------------------------------------------------
dt$Abundance <- NA
dt$Biomass

# aggregate abundance records that are same species and sampling event.
dt$SampleDescription <- as.factor(paste(dt$Plot, dt$Year, sep = '_'))
dt$DepthElevation <- dt$Elevation
dt_merged <- dt %>% group_by(Abundance,Family,Genus,Species,
                              SampleDescription,Plot,Latitude,Longitude,
                              DepthElevation,Day,Month,Year,StudyID) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup()
nrow(dt) - nrow(dt_merged) # it' was already aggregated fine's the subspecies that hs been removed
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
                  'StudyID')] %>% arrange(Year, Plot, Genus, Species)
dt_merged$Plot <- NA
dt_merged$StudyID <- NA
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Norvegian_Scandes_MIREN_rawdata_VB.csv', row.names=F)
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



