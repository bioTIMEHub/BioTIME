
## STUDY_ID 235
## Hubbard Brook Experimental Forest: Watershed 5 stem origin tracking data
## Recurate for fixes of BioTIME v1
## Curator: CC

require(dplyr)
require(stringr)
require(lubridate)

rm(list = ls())

dataset_id <- 'study331_DEMO_recurate_CC'
dt <- read.csv('Originals/study331/TP10815_v4_overstory.csv', header = T)
# sites and spcodes are extracted and converted from metadata xml
sites <- read.csv('Originals/study331/plotcoords.csv', header = T)
spcodes <- read.csv('Originals/study331/spcodes.csv', header = T)

# prep sites for matching into dt

# View(sites)
colnames(sites)[1:5] <- c('plot', 'W', 'E', 'N', 'S')
sites$block <- str_extract(sites$plot, '(?<=block\\s)[:digit:]+') %>% as.numeric
sites$treatment <- str_extract(sites$plot, '(?<=unit\\s)[:digit:]+') %>% as.numeric
sites$row <- str_extract(sites$plot, '(?<=row\\s)[:upper:]+')
sites$column <- str_extract(sites$plot, '(?<=column\\s)[:digit:]+') %>% as.numeric
summary(sites)

sites <- sites[-1,] # get rid of the all sites row
sites$Longitude <- with(sites, W + (E - W)/2)
sites$Latitude <- with(sites, S + (N - S)/2)
sites$plot <- with(sites, paste0('P', block, treatment, row, column))
sites <- sites %>% select(!W:S)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions, 21468 records originally
str(dt) # check structure

dt <- dt %>% filter(TRT == 1)
summary(dt)
dt <- dt %>% select(YEAR, BLOCK, TRT, PLOT, TREESPP, DBH, SAMPLEDATE)

# match in site info
# Plot ID = block, treatment, row, column
dt <- left_join(dt, sites[-c(2,3)], by=c("PLOT" = 'plot'))
colnames(dt) <- str_to_title(colnames(dt))
colnames(spcodes)[2] <- 'Taxon'
dt <- left_join(dt, spcodes, by=c("Treespp" = "code")) # join in taxonomic names
dt$Treespp <- NULL # remove codes

str(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
dt$Sampledate <- parse_date_time(dt$Sampledate, "Y-m-d")
dt$Month <- month(dt$Sampledate)
dt$Day <- day(dt$Sampledate)
dt$Sampledate <- NULL
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y
# check the number of treatment plots

n_distinct(dt$Plot)
with(dt, paste0(Block, Trt, Row, Column)) %>% n_distinct # equal
# 185 subplots within control treatment
dt$Trt <- NULL # not needed anymore
dt$Row <- NULL
dt$Column <- NULL
dt$Block <- NULL

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
summary(dt)
colnames(dt)[4] <- 'Biomass'
dt$Abundance <- 1 # individual level DBH data, so abundance = 1 for aggregation convenience
sum(dt$Biomass == "", na.rm=T) # no blanks
dt %>% filter(is.na(Biomass))
dt <- dt %>% filter(!is.na(Biomass))
# eliminate 0 or NA records

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
dt %>% filter(is.na(Month) | is.na(Day)) %>% distinct(Year, Plot)
# dates all good, except for year 2003... no months or days

# LAT LONG
# already checked when matching in

# world_map <- map_data('world') # check whether the GPS coordinates match expectations
# world <- ggplot(world_map) +
#   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#   coord_fixed() +
#   labs(x='Longitude', y='Latitude') +
#   theme_bw() + theme(panel.grid=element_blank())
# points <- world + geom_point(data=sites, aes(x=Longitude, y=Latitude), shape=21)
# points

# Taxonomic field check ---------------------------------------------------

# need to coerce common names to scientific names
# check if there is a single scientific name that fits

dt$Species <- dt$Taxon
sort(unique(dt$Species))
dt$Species <- str_remove_all(dt$Species, '\\svar. [:alnum:]+$')
sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2)
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------
dt %>% distinct(Year, Month, Day)
# confirms annual sampling suggested by methods
# SKIP AGGREGATION. keep individual level body size
dt_merged <- dt %>%  
  mutate(Family = '', DepthElevation = '', StudyID = '',
         SampleDescription = paste(Year, Plot, sep = "_")) %>% 
  arrange(Year, Month, Plot, Genus, Species) %>%
  relocate(Abundance,
           Biomass,
           Family,
           Genus,
           Species,
           SampleDescription,
           Plot,
           Latitude,
           Longitude,
           DepthElevation,
           Day,
           Month,
           Year,
           StudyID)

n_distinct(dt_merged$SampleDescription)
View(dt_merged) # final check :)

# Export final raw data ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset_id, '_rawdata.csv'), row.names=F)
clipr::write_clip(dt_merged)




### Spatial Geometry Calculations for BioTIME datasets
# paste this chunk into the end of your curation script

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

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()