#### Nouioua - baths ####

dataset_name <- "Insectivorous_bats_Maddeira_Island"

require(tidyverse)
require(readxl)
require(maps)



setwd("D:/Dropbox/towards BioTIME v2/Vivi-dataset hunting files/Nouioua - baths")

dt <- read.csv("Insectivorous_bats_Madeira_island_VB.csv")
dim(dt)
names(dt)
dt <- dt[,1:13] #keep only relevant columns

# field checks
is.numeric(dt$Abundance)
summary(dt$Abundance) # there's zeros and NAs
dim(dt)
is.factor(dt$Year)
is.integer(dt$Year) # yes
unique(dt$Year)
is.factor(dt$Family) 
is.character(dt$Family) #yes
unique(dt$Family) #fix capitalized V
is.factor(dt$Species)
is.character(dt$Species) #yes 
unique(dt$Species) # - only retain species
is.factor(dt$Genus)
is.character(dt$Genus) #yes
unique(dt$Genus)


### Fixes

##Abundance
#check NAs
dt[is.na(dt$Abundance),]
summary(dt[is.na(dt$Abundance),])
# all NAs - get rid of these lines
dt<- dt[!is.na(dt$Abundance),]
dim(dt) #348 records

#check zeros
dt[dt$Abundance==0,]
dt <- dt[dt$Abundance>0,]
dim(dt) #257 records
# Abundance is fine

## Family
dt$Family <- "Vespertilionidae"

## Species
sp <- word(dt$Species, start=2)
unique(sp)
dt$Species<- sp

## Check Lon and Lat
# Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points #lon lat are swapped
points <- world + geom_point(data=dt, aes(x=Latitude, y=Longitude), shape=21)
points # this seems right, swap

lat<- dt$Longitude
lon <- dt$Latitude
dt$Longitude <-lon
dt$Latitude <-lat

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points # yep

detach('package:maps')

summary(dt)

## Prepare for export
#put DepthElevation back in
dt$DepthElevation <- rep('', nrow(dt))

##aggegation check
# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # perfect

dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(dataset_name, Latitude, Longitude, Plot, Year, Month, Day, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # 116 succcessful sampling events

#rreformat columns
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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
head(dt_merged) # final check!


## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='')

#Excel to fill out the curation spreadsheet. 

#Central coordinates (`CentralLatitude, CentralLongitude`) and sampling area. 

# load libraries
library(sf)
library(clipr)
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


## snippet if coordinates are ever in degree minutes seconds
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}
# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim=c(6.4,6.7), ylim=c(52.7,52.9)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()               