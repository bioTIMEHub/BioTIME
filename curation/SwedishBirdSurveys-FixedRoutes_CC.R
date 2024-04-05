

# Curation Script ---------------------------------------------------------

# Dataset: Swedish Bird Surveys: Fixed routes
# Location: Sweden
# Curator: Cher Chow
# Date: 01-Jul-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(lubridate)

rm(list=ls())
data.name <- 'SwedishBirdSurveys-FixedRoutes'
curator <- 'CC'

# make sure your working directory is set before running these lines
# Darwin Core Archive format
dt <- read.table('./Originals/SwedishBirdSurveys/occurrence.txt', sep='\t', header=T)
sites <- read.table('./Originals/SwedishBirdSurveys/event.txt', sep='\t', header=T)
View(dt)
View(sites)

# Structure organisation --------------------------------------------------

# add the metadata from the sites sheet
dt[,c(1:4,7,9,12)] <- NULL # remove some of the unnecessary fields
dt <- sites %>% select(eventID, eventDate, decimalLatitude, decimalLongitude, locationID) %>% right_join(., dt, by="eventID")

# rename fields
colnames(dt)[3:9] <- c('Latitude', 'Longitude', 'locationID', 'Abundance', 'scientificName', 'Genus', 'Species')

dt %>% select(Latitude,Longitude) %>% distinct() %>% nrow() # how many plots
n_distinct(dt$locationID) # lines up!

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 363961 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# dates need to be coerced
dt$Date <- as.POSIXct(dt$eventDate, tryFormats=('%Y-%m-%d'))
summary(dt$Date)

# Year, month and day must be integers or factors?
dt$Year <- year(dt$Date) %>% as.integer()
dt$Month <- month(dt$Date) %>% as.integer()
dt$Day <- day(dt$Date) %>% as.integer()
dt$Date <- NULL
dt$eventDate <- NULL
dt$eventID <- NULL

# logical?
summary(dt[,c('Year', 'Month', 'Day')]) # yes

# Secondary fields such as trawl, plot, transect etc must be factors or integers?

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)+0)
min(dt$Abundance, na.rm=T) # no zeroes? Y
dt %>% filter(is.na(Abundance)) %>% filter(Abundance == 0)
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# WGS84 coordinates?
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

# world_map <- map_data('world') # check whether the GPS coordinates match expectations
# world <- ggplot(world_map) + 
#   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#   coord_fixed() + 
#   labs(x='Longitude', y='Latitude') +
#   theme_bw() + theme(panel.grid=element_blank())
# points <- world + geom_point(data=coord.test %>% distinct(Longitude, Latitude, .keep_all=T), aes(x=Longitude, y=Latitude), shape=21, fill="orange") +
#   geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)
# 
# points_zoom <- points + coord_fixed(xlim=c(5,30), ylim=c(40,70))
# points_zoom # all looks good
# 
# rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Species) + 0) == 0
sum(str_detect(dt$Species, '^\\s+$') + 0) == 0
sum(is.na(dt$Genus) + 0) == 0
sum(str_detect(dt$Genus, '^\\s+$') + 0) == 0

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)

# Check the species records
sort(unique(dt$scientificName)) # looks good
sort(unique(dt$Genus)) # looks good
dt$Species[str_which(dt$Species, '\\s')] %>% sort(.) %>% unique(.) # view all the subspecies
dt$scientificName[which(str_count(dt$scientificName, '\\s') > 1)] %>% sort(.) %>% unique(.) # view all the subspecies
dt[which(dt$scientificName != paste(dt$Genus, dt$Species, sep=' ')), 5:7] %>% distinct() %>% View()

# create correction vector
sp_correct <- c(
  'f. hornemanni' = 'sp1',
  'corone cornix' = 'cornix',
  'corone corone' = 'corone'
)

dt$Species <- str_replace_all(dt$Species, sp_correct)
# check that all specific epithets are one word
dt$Species[str_which(dt$Species, '\\s')] %>% sort(.) %>% unique(.) # view all the subspecies
rm(sp_correct)

# Needs separating mammals into a different dataset
require(taxize)
mammal <- tax_name(sci = sort(unique(dt$Genus)), get = "class", db = "ncbi")

# create the sp lists
bird <- mammal %>% filter(class == 'Aves') %>% pull(query)
mammal <- mammal %>% filter(class == 'Mammalia') %>% pull(query)

# split the datasets
dt_bird <- dt %>% filter(Genus %in% bird) %>% select(!scientificName)
dt_mam <- dt %>% filter(Genus %in% mammal) %>% select(!scientificName)
rm(sites)

summary(dt_mam)
summary(dt_bird)
# looks great

# Prepare raw data --------------------------------------------------------
dt %>% distinct(locationID, Year, Month, Day) %>% 
  arrange(Year, Month, Day, locationID) %>% View

dt_bird %>% group_by(Year) %>% summarise(nlocations = length(unique(locationID))) %>% View # spatial sampling evenness
dt %>% group_by(locationID) %>% summarise(nyear = length(unique(Year))) %>% View

# aggregate abundance records that are same species, plot, and survey day.
dt_bird_m <- dt_bird %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, locationID, Genus, Species)
nrow(dt_bird) - nrow(dt_bird_m) # any change in aggregating? no

dt_mam_m <- dt_mam %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, locationID, Genus, Species)
nrow(dt_mam) - nrow(dt_mam_m) # any change in aggregating? no

# now create empty columns needed to fit to template
dt_bird_m$Biomass <- ''
dt_bird_m$Family <- ''
dt_bird_m$DepthElevation <- ''
dt_bird_m$StudyID <- ''
dt_bird_m$Plot <- ''
dt_bird_m$SampleDescription <- as.factor(with(dt_bird_m, paste(Year, locationID, sep='_')))
length(levels(dt_bird_m$SampleDescription)) # 9409 samples

# reorder columns by BioTIME format
dt_bird_m <- dt_bird_m[c('Abundance',
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
View(dt_bird_m) # final check :)

dt_mam_m <- dt_mam %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, locationID, Genus, Species)
nrow(dt_mam) - nrow(dt_mam_m) # any change in aggregating? no

# now create empty columns needed to fit to template
dt_mam_m$Biomass <- ''
dt_mam_m$Family <- ''
dt_mam_m$DepthElevation <- ''
dt_mam_m$StudyID <- ''
dt_mam_m$Plot <- ''
dt_mam_m$SampleDescription <- as.factor(with(dt_mam_m, paste(Year, locationID, sep='_')))
length(levels(dt_mam_m$SampleDescription)) # 2455 samples

# reorder columns by BioTIME format
dt_mam_m <- dt_mam_m[c('Abundance',
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
View(dt_mam_m)

# Export final ------------------------------------------------------------

write.csv(dt_bird_m, paste0(data.name, '_Birds', '_rawdata_', curator, '.csv'), row.names=F)
write.csv(dt_mam_m, paste0(data.name, '_Mammals', '_rawdata_', curator, '.csv'), row.names=F)

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