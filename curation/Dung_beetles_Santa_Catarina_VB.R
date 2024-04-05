# Curation Script ---------------------------------------------------------

# Dataset:  Dung beetles from Santa Catarina Island
# Location: Brasil
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
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Ale-Brazil/Malva_scarab/")
dt <- readxl::read_excel('malva scarab.xlsx', 
                         sheet=2, col_names=T, na='')
dt <- dt[,1:14]

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? 7
unique(dt$Month)
dt$Month[dt$Month == "september"] <- 9
dt$Month[dt$Month == "december"] <- 12
dt$Month[dt$Month == "march"] <- 3
dt$Month[dt$Month == "july"] <- 7
dt$Month[dt$Month == "october"] <- 10
dt$Month[dt$Month == "january"] <- 1
dt$Month[dt$Month == "april"] <- 4
dt$Month<- as.numeric(dt$Month)

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[,11:13]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

dt$Latitude[dt$Latitude == "27°43'6.35\"S"] <- -27.7184305
dt$Longitude[dt$Longitude == "48°30'46.09\"W"] <- -48.5128028
dt$Latitude[dt$Latitude == "27°43'4.57\"S"] <- -27.7179361
dt$Longitude[dt$Longitude == "48°30'49.71\"W"] <- -48.5138083
dt$Latitude[dt$Latitude == "27°43'2.43\"S"] <- -27.7173417
dt$Longitude[dt$Longitude == "48°30'53.53\"W"] <- -48.5148694
dt$Latitude[dt$Latitude == "27°43'1.17\"S"] <- -27.7169917
dt$Longitude[dt$Longitude == "48°30'57.58\"W"] <- -48.5159944
dt$Latitude[dt$Latitude == "27°42'60.00\"S"] <- -27.7166667
dt$Longitude[dt$Longitude == "48°31'1.94\"W"] <- -48.5172056

dt$Latitude[dt$Latitude == "27°31'38.13\"S"] <- -27.5272583
dt$Longitude[dt$Longitude == "48°30'36.84\"W"] <- -48.5102333
dt$Latitude[dt$Latitude == "27°31'34.71\"S"] <- -27.5263083
dt$Longitude[dt$Longitude == "48°30'38.54\"W"] <- -48.5107056
dt$Latitude[dt$Latitude == "27°31'31.12\"S"] <- -27.5253111
dt$Longitude[dt$Longitude == "48°30'39.89\"W"] <- -48.5110806
dt$Latitude[dt$Latitude == "27°31'27.64\"S"] <- -27.5243444
dt$Longitude[dt$Longitude == "48°30'35.22\"W"] <- -48.5097833
dt$Latitude[dt$Latitude == "27°31'23.98\"S"] <- -27.5233278
dt$Longitude[dt$Longitude == "48°30'33.56\"W"] <- -48.5093222

dt$Longitude<- as.numeric(dt$Longitude)
dt$Latitude<- as.numeric(dt$Latitude)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# seems fine
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  coord_fixed(xlim = c(-48.53, -48.50), ylim = c(-27.8,-27.4)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))
sum(is.na(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))

dt[dt$Species == "sp_b",]
dt[dt$Species == "rutilans_cyanescens",]

dt$Species[dt$Species == "sp_b"] <- "sp"
dt$Species[dt$Species == "rutilans_cyanescens"] <- "rutilans"


# Prepare raw data --------------------------------------------------------
dt$Abundance
dt$Biomass <- NA

# aggregate abundance records that are same species and sampling event.
dt$SampleDescription <- as.factor(paste(dt$Plot, dt$Day, dt$Month, dt$Year, sep = "_"))
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
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Dung_beetles_Santa_Catarina_rawdata_VB.csv', row.names=F)
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



