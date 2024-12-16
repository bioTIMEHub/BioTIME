### Curation script for LightAndShadow_fungi ###
### author: VB ####

###### upload libraries #####
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)

###### read dataset ######
mypath <- getwd()     
ds <- read.csv(file = paste0(mypath,"/6mal6_Biotime_raw_fungi.csv"), h=T)

# transform to long format
str(ds)
ds <- ds[,1:14]
dim(ds)
unique(ds$Year)
#### add and check locations
# coordinates taken from wikipedia and checked with map in Appendix 2
unique(ds$Plot)
table(ds$Plot)
unique(c(ds$Latitude, ds$Longitude))

#map
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=as.data.frame(cbind(lon = ds$Longitude, lat = ds$Latitude)), 
             aes(x=lon, y=lat, alpha=0.01)) 
points

# zoom 
points_zoom <- points +
  ylim(49.79,50)+
  xlim(10.4,10.6)
points_zoom

##### check counts####
unique(ds$Abundance) # inspect values
is.numeric(ds$Abundance)

sum(ds$Abundance <= 0) # there's 136087 cases with 0 count
ds <- ds[ds$Abundance > 0,] # exclude them
sum(is.na(ds$Abundance)) # there's no NA counts
# ds <- ds[!is.na(ds$Abundance),]
sum(is.null(ds$Abundance)) # there's no NULL counts
# ds <- ds[ds$Abundance != "NULL",]
sum(ds$Abundance=="")  # there's no blanks
# ds <- ds[ds$Abundance != "NULL",]

dim(ds)  # 59707     4

##### check taxa ####
length(unique(ds$Species)) # 170

sum(is.na(ds$Species)) # no NAs
sum(ds$Species=="") # no blanks
sum(is.null(ds$Species)) # no NULLs
sort(unique(ds$Species)) # sorting taxa to catch duplications and mispelling

sum(is.na(ds$Genus)) # no NAs
sum(ds$Genus=="") # no blanks
sum(is.null(ds$Genus)) # no NULLs
sort(unique(ds$Genus)) # sorting taxa to catch duplications and mispelling
# seems ok

##### check dates ####
unique(ds$Day)
unique(ds$Month)
ds$Month <- 10
unique(ds$Year)
table(ds$Year)
# seems right

sum(is.na(ds$Year)) # there's no NA counts
# ds <- ds[!is.na(ds$yearcollected),]
sum(ds$Year=="NULL") # there's no NULL counts
# ds <- ds[ds$yearcollected != "NULL",]
sum(ds$Year=="")  # there's no blanks
# ds <- ds[ds$yearcollected != "NULL",]

##### control coordinates ####
summary(ds$Longitude)
summary(ds$Latitude)

## sampleDesc
ds$SampleDescSpec 
ds$SampleDescription <- as.factor(paste0("LightAndShadow_fungi_",
                                      ds$Latitude,"_",
                                      ds$Longitude,"_",
                                      ds$DepthElevation,"_",
                                      ds$Month,"_",
                                      ds$Year,"_",
                                      ds$Plot))

ds %>%
  group_by(Plot) %>%
  count()

dim(ds)
str(ds)

rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = rep(NA,nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDesc =  ds$SampleDescription,
                      Plot = ds$Plot,
                      Latitude = ds$Latitude, 
                      Longitude = ds$Longitude,
                      DepthElevation = ds$DepthElevation,
                      Day = rep("NULL",nrow(ds)),
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))
head(rawdata)

## aggregation check
dt_merged <- rawdata %>% group_by(Biomass,Family,Genus, Species,
                                  Plot,Latitude, Longitude,DepthElevation,
                                  Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- rawdata %>% group_by(Biomass,Family,Genus, Species,
                           Plot,Latitude, Longitude,DepthElevation,
                           Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] #nope!
View(dt[duplicated(dt),])
which(duplicated(dt))
dt[1389:1390,]
dt[1634:1635,]

ds <- dt_merged

rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = rep(NA,nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDesc =  as.factor(paste0("LightAndShadow_fungi_",
                                                     ds$Latitude,"_",
                                                     ds$Longitude,"_",
                                                     ds$DepthElevation,"_",
                                                     ds$Month,"_",
                                                     ds$Year,"_",
                                                     ds$Plot)),
                      Plot = ds$Plot,
                      Latitude = ds$Latitude, 
                      Longitude = ds$Longitude,
                      DepthElevation = ds$DepthElevation,
                      Day = rep("NULL",nrow(ds)),
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))


### save raw data 
write.csv(rawdata, "LightandShadow2021_fungi_RAWDATA_VB.csv", row.names = FALSE)

###code for calculating geographical features of the data

##load libraries
library(sp)
library(rgeos)

#Import the raw data file - has latitude & longitude

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
area
# well it's 2 plots, no point in doing this...

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

