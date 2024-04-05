##########################################################################################
# Study: Stream benthic macroinvertebrates abundances over a 6-year monitoring period of an Italian glacier-fed stream
# Curator: AFE
# Date: 10-6-2020 & revised December 2023
##########################################################################################

# Main sources ===========================================================================
# https://doi.pangaea.de/10.1594/PANGAEA.897915

# Libraries ==============================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(pangaear)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()

# Read raw data files ==========================================================
dt_file <- pg_data(doi="10.1594/PANGAEA.897915") # Download Data from Pangaea
dt <- dt_file[[1]][["data"]]


# Explore Data =================================================================
names(dt)
names(dt)[names(dt)=="Macrof abund [#/m**2]"] <- "Density"


# Check structure ==============================================================
dim(dt) #1901 x 9
str(dt)


# Abundances ===================================================================
range(dt$Density) # OK


# Time =========================================================================

dt$Day <- str_split_fixed(dt$`Date/Time`, "-", 3)[,3]
unique(dt$Day)
dt$Day <- as.integer(dt$Day)
dt$Month <- str_split_fixed(dt$`Date/Time`, "-", 3)[,2]
unique(dt$Month)
dt$Month <- as.integer(dt$Month)
dt$Year <- str_split_fixed(dt$`Date/Time`, "-", 3)[,1]
unique(dt$Year)
dt$Year <- as.integer(dt$Year)


# Coordinates ==================================================================

unique(dt$Latitude) # 3
range(dt$Latitude)  # no NAs

unique(dt$Longitude) # 3
range(dt$Longitude)  # no NAs

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points       

#Zoom:
points_zoom <- points +
  ylim(30,50)+
  xlim(-20,20)
points_zoom   # OK


# Depth/elevation column =======================================================
unique(dt$Event)
dt$DepthElevation <- rep(NA, nrow(dt))

dt$DepthElevation <- ifelse(dt$Event=="Saldur_1", 2030, dt$DepthElevation)
dt$DepthElevation <- ifelse(dt$Event=="Saldur_2A", 2016, dt$DepthElevation)
dt$DepthElevation <- ifelse(dt$Event=="Saldur_3", 1645, dt$DepthElevation)

unique(dt$DepthElevation)


# Taxonomy =====================================================================

sum(is.na(dt$Family) & is.na(dt$Genus)) # 0, OK
sum(dt$Family =="" & dt$Genus=="")      # 0, OK

sort(unique(dt$Genus))
sum(dt$Genus=="") # 867
dt$Genus[dt$Genus==""] <- NA

unique(dt$Family[dt$Genus %in% c("Choloroperla", "Chloroperla")])
unique(dt$Order[dt$Genus %in% c("Choloroperla", "Chloroperla")])
dt$Genus <- plyr::revalue(dt$Genus, c("Choloroperla"="Chloroperla")) # Choloroperla is a misspelling, checked

dt$Species <- rep(NA, nrow(dt))
dt$Species <- ifelse(dt$Genus=="Crenobia alpina", "alpina", dt$Species)
dt$Genus <- ifelse(dt$Genus=="Crenobia alpina", "Crenobia", dt$Genus)

sum(is.na(dt$Genus)) # 867

sort(unique(dt$Family))


# Create rawdata ===============================================================
names(dt)
newdt <- dt %>% group_by(Event, Latitude, Longitude, DepthElevation, Family, Genus, Species, Day, Month, Year) %>%
  summarise("Abundance"=sum(Density))

newdt$Biomass <- rep(NA, nrow(newdt))
newdt <- newdt %>% relocate(c(Abundance, Biomass, Family, Genus, Species), .before=Event)

newdt$Plot <- rep(NA, nrow(newdt))
newdt$Event <- gsub("_", "", newdt$Event)
newdt$SampleDescription <- paste0(newdt$Event, "_", newdt$Day, "_", newdt$Month, "_", newdt$Year)
newdt <- within(newdt, rm(Event))
newdt <- newdt %>% relocate(c(SampleDescription, Plot, Latitude, Longitude, DepthElevation), .after=Species)
newdt$StudyID <- rep(NA, nrow(newdt))

range(newdt$Abundance)

rawdata <- newdt


# Write csv ====================================================================

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/BenthicInvertebrates_SaldurStream_Italy_Scotti&Bottarin2018_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)   


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(10.5,11), ylim = c(46.5,47)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal() # OK, longitudinal transect


# End of script ################################################################