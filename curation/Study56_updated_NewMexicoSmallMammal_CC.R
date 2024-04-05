

# Curation Script ---------------------------------------------------------

# Dataset: STUDY 56 Small Mammal Mark-Recapture Population Dynamics at Core Research Sites
# Location: New Mexico, USA
# Curator: Cher Chow
# Date: 25-Oct-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(lubridate)
require(maps)
require(CoordinateCleaner)
require(readxl)

rm(list=ls())
data.name <- 'Study56_updated_NewMexicoSmallMammal'
curator <- 'CC'

# make sure your working directory is set before running these lines
dt <- read.csv('CurationProgress/sTeTra/Study56/sev008_rodentpopns_qaqcd_final_data.csv', header=T, sep=',')
species <- read.csv('CurationProgress/sTeTra/Study56/SEV_Small_Mammals_Species_List.csv', header=T)
View(dt)
summary(dt)

# Structure organisation --------------------------------------------------

# see if the sampling design described in methods match
dt %>% distinct(location, web) %>% arrange(location, web)
n_distinct(dt$location)

# Add study site/plot coordinates from author provided xml metadata
sites <- data.frame(location=sort(unique(dt$location)), Latitude=NA, Longitude=NA)
# in order of metadata matched with location nicknames
sites[1,3:2] <- c(-106.736,34.3331) #5pgrass, Five Points Black Grama
sites[2,3:2] <- c(-106.736,34.3331) #5plarrea, Five Points Creosote
sites[5,3:2] <- c(-106.927,34.296) #rsgrass, Rio Salado
sites[6,3:2] <- c(-106.927,34.296) #rslarrea, Rio Salado
sites[4,3:2] <- c(-106.523,34.4146) #goatdraw, The Goat Draw Juniper Savanna Core Site
sites[3,3:2] <- c(-106.631,34.3348) #blugrama, The Blue Grama core site
sites[8,3:2] <- c(-107.03,34.418) #two22, foothills of the Sierra Ladrones
sites[7,3:2] <- c(-106.535,34.368) #savanna, Cerro Montosa Pinyon-Juniper site

dt <- left_join(dt, sites, by="location")
# filter out recaptured individuals
dt <- dt %>% filter(recap == 'n')

# just grab the fields we need
dt <- dt[c(1:5,8,12,17:18)]
View(dt) # check the joining and filtering
dt$Plot <- with(dt, paste(location, web, sep="-")) # plot is a combination of study site and the mammal trap web. Did not include the trap number on the web.
dt$location <- NULL
dt$web <- NULL

# rename fields
colnames(dt)[c(1,4,5)] <- c('Year','Code', 'Biomass')

# season and day are proxies for month and day, but vary year to year.
# Curator puts in a value based on author provided ranges for the years sampled in methods.
# season 1, spring
dt$Month[which(dt$season == 1)] <- 5
dt$Day[which(dt$season == 1 & dt$night == 1)] <- rep(15, which(dt$season == 1 & dt$night == 1) %>% length)
dt$Day[which(dt$season == 1 & dt$night == 2)] <- rep(16, which(dt$season == 1 & dt$night == 2) %>% length)
dt$Day[which(dt$season == 1 & dt$night == 3)] <- rep(17, which(dt$season == 1 & dt$night == 3) %>% length)

# season 2, summer, 1989 to 1993
dt$Month[which(dt$season == 2)] <- 7
dt$Day[which(dt$season == 2 & dt$night == 1)] <- rep(15, which(dt$season == 2 & dt$night == 1) %>% length)
dt$Day[which(dt$season == 2 & dt$night == 2)] <- rep(16, which(dt$season == 2 & dt$night == 2) %>% length)
dt$Day[which(dt$season == 2 & dt$night == 3)] <- rep(17, which(dt$season == 2 & dt$night == 3) %>% length)

# season 3, summer. 1994 to present
dt$Month[which(dt$season == 3)] <- 9
dt$Day[which(dt$season == 3 & dt$night == 1)] <- rep(15, which(dt$season == 3 & dt$night == 1) %>% length)
dt$Day[which(dt$season == 3 & dt$night == 2)] <- rep(16, which(dt$season == 3 & dt$night == 2) %>% length)
dt$Day[which(dt$season == 3 & dt$night == 3)] <- rep(17, which(dt$season == 3 & dt$night == 3) %>% length)
str(dt)
dt$season <- NULL
dt$night <- NULL
# not joining species table yet because I'll do the taxonomic checks first before joining.
# do checks before aggregating individual per row to species per row.

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 20331
str(dt) # check structure

# do coercions first
dt$Plot <- as.factor(dt$Plot)
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# lots of biomass NAs
dt %>% filter(is.na(Biomass)) %>% View
# logical? yes
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# individual per row convert to species per row
dt <- dt %>% group_by(across(c(-Biomass))) %>% summarise(Abundance=n(), Biomass=sum(Biomass)) %>% ungroup()

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt %>% filter(is.na(Abundance)) # blanks in Abundance?
min(dt$Abundance, na.rm=T) # no zeroes? Y
min(dt$Biomass, na.rm=T) # no zeroes? Y
str_which(dt$Abundance, '\\s') %>% length() # no blanks? Y

# LAT LONG
str_which(dt$Latitude, '\\s') %>% length() # no blanks?
str_which(dt$Longitude, '\\s') %>% length()
sum(is.na(dt[c('Latitude', 'Longitude')])) == 0 # no NAs?

# WGS84 coordinates? Y
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')])

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + 
  geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude))
points
points_zoom <- points + coord_fixed(xlim=c(-115,-100), ylim=c(30,40))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records Y
# genus level add sp
species$Species <- species$Species %>% str_replace_all(., 'spp.', 'sp')

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
sort(unique(species$Genus))
sort(unique(with(species, paste0(Genus,' ', Species)))) # all good

# good to merge with dt dataframe now
dt <- species %>% select(Code, Genus, Species) %>% left_join(dt, ., by="Code")
View(dt)
dt$Code <- NULL

# Prepare raw data --------------------------------------------------------

# don't need aggregation because I already did that above.
# now create empty columns needed to fit to template
dt$StudyID <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))
dt$SampleDescription <- as.factor(with(dt, paste(data.name, Year, Month, Day, Plot, Latitude, Longitude, sep='_')))
length(levels(dt$SampleDescription)) # 2098 samples

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
                         'Year',
                         'StudyID')] %>% arrange(Year, Month, Day, Plot, Family, Genus, Species)
View(dt) # final check :)


# Export final ------------------------------------------------------------

write.csv(dt, paste0('CurationProgress/', data.name, '_rawdata_', curator, '.csv'), row.names=F)

# Convex Hull for centroid ------------------------------------------------

##load libraries
require(sp)
require(rgeos)
require(clipr)
write_clip(dt)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dataset_coords<- SpatialPoints(dt %>% select(Longitude, Latitude) %>% distinct(), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) 

# 2. Calculate convex hull, area and centroid
centroid <- gConvexHull(dataset_coords) %>% gCentroid() # get centroid
centroid@coords # coordinates
centroid@coords[c(2,1)] %>% write_clip()

# use lake area. two points not sufficient.
