################################################################################
# Study: Freshwater Fish survey in the Northern Range, Trinidad (BioTIME dataset)
# Curator: AFE
# Date: August 2023
################################################################################


# Main sources =================================================================
# https://www.pnas.org/doi/10.1073/pnas.1712594115


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)


rm(list=ls())
getwd()


# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Fish_T&T_Deacon_et_al_2018_AFE"

load(paste0(files_dir, "/aggFish2010_15.RData"))             # fish dataset
dt <- as.data.frame(aggFish2010_15)
load(paste0(files_dir, "/keysamplingdates.RData"))           # timestep to date key
temp <- as.data.frame(keysamplingdates)
coords <- read.csv(paste0(files_dir, "/coords_TT.csv"), h=T) # coordinates


# Abundances ===================================================================
range(dt$Abundance)  # OK
range(dt$Biomass)    # OK

dt$Genus[dt$Abundance==max(dt$Abundance)]  # "Poecilia", OK
dt$Genus[dt$Biomass==max(dt$Biomass)]      # "Astyanax", OK


# Temporal =====================================================================
sort(unique(dt$Day))
sort(unique(dt$Month))
sort(unique(dt$Year))

sum(is.na(dt$Year))
sum(is.na(dt$Month))
sum(is.na(dt$Day))


dt$Day <- as.integer(dt$Day)
dt$Month <- as.integer(dt$Month)
dt$Year <- as.integer(dt$Year)

c1 <- dt %>% group_by(Site, Year, Month) %>% summarise(n=n_distinct(Day)) # always 1


# Coordinates & locations ======================================================
dt$Site <- paste0(substr(dt$Site, 1, nchar(dt$Site) - 1), toupper(substr(dt$Site, nchar(dt$Site), nchar(dt$Site))))
dt$Site <- gsub(" ", "", dt$Site)
coords$Site <- paste0(substr(coords$Site, 1, nchar(coords$Site) - 1), toupper(substr(coords$Site, nchar(coords$Site), nchar(coords$Site))))
coords$Site <- gsub("_", "", coords$Site)

dt$Latitude <- coords$Latitude[match(dt$Site, coords$Site)]
dt$Longitude <- coords$Longitude[match(dt$Site, coords$Site)]

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points       # OK

#Zoom:
points_zoom <- points +
  ylim(5, 15)+
  xlim(-65, -55)
points_zoom # OK

sort(unique(dt$Site))  # OK


# Taxonomy =====================================================================
sort(unique(dt$Genus))
sort(unique(dt$Species))


# SampleDesc & data aggregation ================================================

rawdata <- dt %>% group_by(Genus, Species, Site, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass))


rawdata$Family <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Family), .before=Genus)
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$SampleDescription <- paste0(rawdata$Site, "_",
                                    rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before = Latitude)
rawdata$DepthElevation <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(DepthElevation, .after = Longitude)
rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(StudyID, .after = Year)

range(rawdata$Abundance) 
range(rawdata$Biomass)

rawdata <- within(rawdata, rm(Site))
rawdata <- as.data.frame(rawdata)
str(rawdata)

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Fish_T&T_Deacon_et_al_2018_AFE"
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

## also useful snippet if coordinates are ever in degree minutes seconds

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
  coord_sf(xlim = c(-62,-60), ylim = c(10,12)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################
