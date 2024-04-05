# Curation Script ---------------------------------------------------------

# Dataset:  Marine Fish Underwater Surveys in The Israeli Mediterranean
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
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/BelmakerLab/MaiLazarus")
dt1 <- readxl::read_excel("BioTIMEContributorTemplate - Biobliz Fished Areas.xlsx", 
                         sheet=2, col_names=T, na='')
dt1 <- as.data.frame(dt1[,1:14])

dt2 <- readxl::read_excel("BioTIMEContributorTemplate - Biobliz Protected Areas.xlsx", 
                          sheet=2, col_names=T, na='')
dt2 <- as.data.frame(dt2[,1:14])
dt2$Latitude <- as.numeric(dt2$Latitude)
dt2$Longitude <- as.numeric(dt2$Longitude)

summary(dt1)
summary(dt2)
d1lat <- unique(dt1$Latitude)
d1lon <- unique(dt1$Longitude)

d2lat <- unique(as.numeric(dt2$Latitude))
d2lon <- unique(as.numeric(dt2$Longitude))

sum(d1lat %in% d2lat)
sum(d1lon %in% d2lon)
d1lon[which(d1lon %in% d2lon)]
d1lat[which(d1lat %in% d2lat)]

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt1, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'red') +
  coord_fixed(xlim = c(34.75, 35.25), ylim = c(32.3,33.15)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

dt1$Plot <- paste0("NP_",dt1$Plot)
dt2$Plot <- paste0("PA_", dt2$Plot)

dt <- rbind(dt1,dt2)

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

summary(dt$Biomass) # check the minimum (no zeroes)
summary(dt$Biomass) # check the minimum (no zeroes)


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
dt$Species <- word(dt$Species,2)
sort(unique(dt$Species))
summary(as.factor(dt$Species))
dt[(is.na(dt$Species)),]
dt$Species[(is.na(dt$Species))] <- "sp"
dt[dt$Species == "forskalii",]
dt[dt$Species == "forsskali",]

#ok

# Prepare raw data --------------------------------------------------------
dt$Abundance
dt$Biomass

dt %>%
  group_by(Latitude,Longitude, DepthElevation, Day, Month, Year) %>%
  count(Plot) %>%
  ungroup()

#multiple transect per sampling event


# aggregate abundance records that are same species and sampling event.
dt$SampleDescription <- as.factor(dt$Plot)
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

write.csv(dt_merged, 'Marine_Fish_Underwater_Survey_Israeli_Mediterranean_rawdata_VB.csv', row.names=F)
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



