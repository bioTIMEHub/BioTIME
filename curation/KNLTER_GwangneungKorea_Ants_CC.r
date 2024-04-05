

# Dataset: Finland LTER Moths
# Location: Finland, national
# Curator: Cher Chow
# Date: 31-Aug-2022


require(lubridate)
require(dplyr)
require(stringr)
require(readxl)

data_string <- 'KNLTER_GwangneungKorea_Ants_CC'
ddata <- read_excel('Originals/Ants_data.xlsx', sheet = 1)[1:4]
ddata$Guild <- NULL

# Structure check ---------------------------------------------------------

dim(ddata)
str(ddata) # check field types
summary(ddata) # check field values

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
ddata <- ddata %>% filter(!Abundance == 0)
# yes
sum((ddata$Abundance == '') + 0) # how many blanks
sum(is.na(ddata$Abundance) + 0) # how many NAs
nrow(ddata)
# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# yes

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# NA

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

sum(is.na(ddata$Species) + 0)
sort(unique(ddata$Species)) # visual check for misspellings
# nothing to replace

# split Genus species
ddata$Genus <- word(ddata$Species, 1)
ddata$Species <- word(ddata$Species, 2)
sort(unique(ddata$Genus))

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- ddata %>% group_by(Genus, Species, Year) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()
nrow(ddata)-nrow(dt_merged) # any change in aggregating? yes. 2430 records merged

dt_merged$DepthElevation <- ''
dt_merged$Latitude <- 37.44
dt_merged$Longitude <- 127.08
dt_merged$Plot <- ''
dt_merged$Biomass <- ''
dt_merged$Family <- ''
dt_merged$Month <- ''
dt_merged$Day <- ''
dt_merged$StudyID <- ''
dt_merged$SampleDescription <- as.factor(dt_merged$Year)
length(levels(dt_merged$SampleDescription)) # 3 samples (annual)

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
                         'Year')] %>% arrange(Year, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)
write.csv(dt_merged, file = paste0(data_string, '_rawdata.csv'), row.names = F)
# clipr::write_clip(dt_merged)

# Convex Hull for centroid ------------------------------------------------

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
