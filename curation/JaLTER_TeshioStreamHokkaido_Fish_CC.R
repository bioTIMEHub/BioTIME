

# Curation Script ---------------------------------------------------------

# Dataset: JaLTER Teshio Hokkaido Stream Fish
# Location: Hokkaido, Japan
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
dt <- read.csv(file='Originals/StreamFishTeshioHokkaido/ERDP-2020-09.4.1-taxa_data.csv', header=T)
sites <- read.csv(file='Originals/StreamFishTeshioHokkaido/sampling_sites.csv',header=T)
sites <- na.omit(sites)
# for this dataset, each row is an individual fish observation
# Curator had to manually create a csv of stream sampling site locations

# single‐pass removal method was used for A1 in autumn of 2005 and B4 in autumn of 2007
# remove these method inconsistencies
dt <- dt[(dt$Year == 2005 & dt$Site.ID == 'A1') == F,]
dt %>% filter(Year == 2005) %>% View # check 
dt <- dt[(dt$Year == 2007 & dt$Site.ID == 'B4') == F,]
dt %>% filter(Year == 2007) %>% View # check 

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 57062 x 14
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
dt$Month <- month(parse_date_time(dt$Month, '%b')) %>% as.integer()
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
dt$Site.ID <- as.factor(dt$Site.ID)

str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
str(sites)
# Date should be POSIXct: Y
# Taxonomic fields must be characters or factors? Y

dt <- dt %>% select(!c(Body.weight, State, Sampling.order, Order))
summary(dt)
colnames(dt)[1] <- 'SiteID'
sites$SiteID <- str_trim(sites$SiteID, side='both')
dt <- left_join(dt, sites, by="SiteID")
dt <- dt %>% select(!starts_with('Stream'))
dt$Abundance <- 1
colnames(dt)[c(9,12)] <- c('Biomass', 'DepthElevation')

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks
# Abundance is the primary field for this dataset, with some that have biomass too, but not all

min(dt$Biomass, na.rm=T) # no zeroes to biomass fields
dt %>% filter(is.na(Biomass)) %>% View

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[c('Year', 'Month', 'Day')]) # all okay
  
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # sensible ranges

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# Coordinates look correct

points_zoom <- points + coord_fixed(xlim=c(min(dt$Longitude)-10, max(dt$Longitude)+10),
                                    ylim=c(min(dt$Latitude)-10, max(dt$Latitude)+10))
points_zoom


# Secondary field check ---------------------------------------------------

# Plot, treatment, and elevation can have 0s but not blanks, NAs, NULLs, etc.
# remove if the error makes it impossible to assign to a group
# Any errors?
dt$SiteID <- as.factor(dt$SiteID)
summary(dt[c('SiteID', 'DepthElevation')])
levels(dt$SiteID)
sum(is.na(dt$SiteID)+0)
sum(is.na(dt$DepthElevation)+0)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Family)+0)
sum(is.na(dt$Genus)+0)
sum(is.na(dt$Species)+0)
# misspellings check, but no need to validate
sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))
# check that genera are genera, not family names (-idae/eae)
dt$Genus <- str_trim(dt$Genus, side='both') # remove any whitespaces at ends
dt$Species <- word(dt$Species, 2, -1)
dt$Species <- str_replace_all(dt$Species, 'sp.$', 'sp') # Change unknown species to BioTIME format
sort(unique(dt$Species)) # ok, all good.

# Prepare raw data --------------------------------------------------------

# no record aggregation as there are individual body sizes
dt$SampleDescription <- as.factor(with(dt, paste(Year, Month, SiteID, sep='_')))
summary(dt$SampleDescription)
length(levels(dt$SampleDescription)) # 547 sampling events
dt$SiteID <- NULL
dt$Type <- NULL
dt$Plot <- ''
# reorder columns by BioTIME format
dt <- dt[c('Abundance',
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
                         'Year')] %>% arrange(SampleDescription, Genus, Species)
View(dt) # final check :)
summary(dt)
str(dt)

# Export final ------------------------------------------------------------

write.csv(dt, 'JaLTER_TeshioHokkaido_StreamFish_rawdata_CC.csv', row.names=F)
library(clipr)
write_clip(dt)

# load libraries
library(sf)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)
