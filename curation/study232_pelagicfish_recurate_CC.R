

# Curation Script ---------------------------------------------------------

# Dataset: Pelagic Fish Observations 1968-1999 
# Recurating to fix depth information
# Curator: Cher Chow
# Date: Apr 28 2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(lubridate)
require(maps)
require(ggplot2)

rm(list=ls())

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read.table('Originals/study232/occurrence.tsv', header=F, sep='\t')[-2]
# field names not included, but in meta.xml with DwCA

# columns to keep
dt <- dt[c(8,61,9,16,17,19,20,25,26,29,33:31)]
colnames(dt) <- c('Abundance', 'Biomass', 'Taxon', 'Family', 'Genus',
                  'Latitude', 'Longitude', 'minDepth', 'maxDepth', 'locality', 'Day', 'Month', 'Year')
View(dt)
# extract only standard lengths for biomass
dt <- dt %>% mutate(Length = str_extract(Biomass, '\\d{1,3}(?=\\smm)'), .after = Biomass)
sum(str_detect(dt$Biomass, '^Standard|^$|^Gonad')+0) == nrow(dt) # no other type of lengths?
dt$Biomass <- NULL
colnames(dt)[2] <- 'Biomass'

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 54k+ records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# logical?
# Year, month and day must be integers or factors. Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt$locality <- as.factor(dt$locality)
levels(dt$locality)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Abundance))
dt <- dt %>% filter(!is.na(Abundance)) # remove those records=
min(dt$Abundance, na.rm=T) # no zeroes? Y
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y

dt$Biomass <- as.numeric(dt$Biomass)
dt %>% filter(is.na(Biomass)) %>% View
min(dt$Biomass, na.rm=T) # no zeroes? Y
str_which(dt$Biomass, '\\s') %>% length() # no blanks? Y

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?
dt %>% filter(is.na(Latitude) | is.na(Longitude)) %>% View # all Prydz Bay
dt %>% filter(locality == 'Prydz Bay') %>% 
  distinct(Latitude, Longitude, .keep_all = T) %>% 
  arrange(Year, Month, Day) %>% View # only in one year
dt %>% filter(locality == 'Prydz Bay', Year == 1988) %>% 
  arrange(Year, Month, Day) %>% View

# calculate a centroid for the Prydz Bay cruises
# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt %>% filter(locality == 'Prydz Bay', !is.na(Latitude)) %>% distinct(Latitude, Longitude) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
# fill in the NA 1988 year with the centroid for Prydz Bay
dt$Latitude[dt$locality == 'Prydz Bay' & is.na(dt$Latitude)] <- centroid[2]
dt$Longitude[dt$locality == 'Prydz Bay' & is.na(dt$Longitude)] <- centroid[1]
# check
dt %>% filter(is.na(Latitude) | is.na(Longitude)) %>% 
  distinct(Latitude, Longitude, .keep_all = T) %>% View

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
  geom_point(data=dt %>% distinct(Longitude, Latitude), 
             aes(x=Longitude, y=Latitude), 
             shape = 21, color = 'blue')
points

rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# also check Depths
mean(dt$maxDepth - dt$minDepth, na.rm = T) # theyre the same?
sum((dt$maxDepth == dt$minDepth) + 0, na.rm = T) + sum(is.na(dt$maxDepth)+0) # same
dt$minDepth <- NULL
colnames(dt)[8] <- 'DepthElevation'
dt %>% filter(is.na(DepthElevation)) %>% View

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon) + 0) == 0
sum(str_detect(dt$Taxon, '^\\s+$') + 0) == 0
sum(is.na(dt$Genus) + 0) == 0

dt %>% filter(str_detect(Genus, '^\\s+$') | str_detect(Taxon, '^\\s+$'))

sort(unique(dt$Taxon)) # a mix of family, genus, and species level records.
# need to isolate specific epithets out for cleaned records

dt$Taxon[str_which(dt$Taxon, 'idae$|IDAE$')] <- NA # remove family level records since we have a family column
dt$Taxon[str_count(dt$Taxon, '\\s') == 0] %>% unique # uncertain records at genus level
dt$Taxon[str_count(dt$Taxon, '\\s') == 0 & is.na(dt$Taxon) == F] <- paste0(dt$Taxon[str_count(dt$Taxon, '\\s') == 0 & is.na(dt$Taxon) == F], ' sp')
dt$Taxon[str_count(dt$Taxon, '\\s') == 0 & is.na(dt$Taxon) == F] # check
sort(unique(dt$Taxon)) # now check for misspellings

dt$Species <- word(dt$Taxon, 2)
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

dt %>% distinct(Year, Month, Day, Latitude, Longitude, .keep_all = T) %>% arrange(Year, Month, Day) %>% View 
dt$SampleDescription <- with(dt, paste(Latitude, Longitude, Year, Month, Day, DepthElevation, sep = "_")) %>% as.factor
dt$locality <- NULL

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Family, Genus, Species, SampleDescription, Latitude, Longitude, DepthElevation, Day, Month, Year) %>% 
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% 
  ungroup() %>% arrange(Year, Month, Family, Genus, Species) %>% 
  relocate(Abundance, Biomass, .before = Family)
nrow(dt) - nrow(dt_merged) # any change in aggregating? no

# now create empty columns needed to fit to template
dt_merged <- dt_merged %>% mutate(Plot = '', .before = Latitude)

View(dt_merged) # final check :)


# Export final ------------------------------------------------------------

# write.csv(dt_merged, 'study232_recurate_CC_rawdata.csv', row.names=F)

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

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)