
# Curation Script ---------------------------------------------------------

# Dataset: NIES Fish and shrimp monitoring in Lake Kasumigaura
# Version: 2.0
# Location: Japan
# Curator: Cher Chow
# Date: 03-Aug-2021

# Kumiko Totsu (dataset contributor) sent a revised version of the rawData formatted. This is to perform checks on that file.

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(lubridate)
require(maps)
require(CoordinateCleaner)
require(readxl)

rm(list=ls())
data.name <- 'NIES_Fish_LakeKasumigaura'
curator <- 'CC'

dt <- read.csv('Originals/LakeKasumigaura/NIES_FishMonitoring_LakeKasumigaura-Japan_rawData.csv', header=T)
View(dt)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 1523 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# logical?
summary(dt[,c('Year', 'Month', 'Day')]) # yes

summary(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Abundance) & is.na(Biomass))
dt %>% filter(is.na(Biomass)) # blanks in biomass? yes, for 6 records in 2005, 07, 08
min(dt$Abundance, na.rm=T) # no zeroes? Y
min(dt$Biomass, na.rm=T) # no zeroes? Y
dt <- dt %>% filter(!Biomass == 0) # omit zeroes
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y
str_which(dt$Biomass, '\\s') %>% length() # no blanks? Y

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# coordinate cleaner works best against Darwin Core Archive formats
coord.test <- clean_coordinates(dt %>% mutate(countryCode=rep('JP', nrow(dt)), Name=paste(Genus, specificEpithet, sep=' ')),
                                lon='Longitude', lat='Latitude', species='Name', countries='countryCode', verbose=T)
coord.test %>% filter(.summary == F) %>% View()

# WGS84 coordinates? Y
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + 
  geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude)) +
  geom_point(data=coord.test %>% distinct(Longitude, Latitude, .keep_all=T) %>% filter(.summary == F),
             aes(x=Longitude, y=Latitude), shape=21, fill='red')

points_zoom <- points + coord_fixed(xlim=c(130,150), ylim=c(30,40))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))
rm(coord.test, original)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Species) + 0) == 0
sum(str_detect(dt$Species, '^\\s+$') + 0) == 0
sum(is.na(dt$Genus) + 0) == 0

dt %>% filter(str_detect(Genus, '^\\s+$') | str_detect(Species, '^\\s+$'))
dt %>% filter(Genus == '' | Species == '') # two gobiid records certain only to family

# Check the species records
with(dt, paste(Genus, Species, sep=' ')) %>% sort() %>% unique()
sort(unique(dt$Genus))
sort(unique(dt$Species))

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-c(Abundance, Biomass))) %>% 
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Month, Plot, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? no

# now create empty columns needed to fit to template
dt_merged$DepthElevation <- ''
dt_merged$StudyID <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, Plot, sep='_')))
length(levels(dt_merged$SampleDescription)) # 93 samples
dt_merged$Plot <- ''

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
                         'StudyID')]
View(dt_merged) # final check :)


# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(data.name, '_rawdata_', curator, '.csv'), row.names=F)

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# area provided by author