################################################################################
# Study: Data from: Spatial and temporal variations of aridity shape dung beetle assemblages towards the Sahara desert
# Curator: AFE
# Date: August 2020 & revised December 2023
################################################################################

# main sources =================================================================
# https://doi.org/10.7717/peerj.5210


# libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)


rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# read data ====================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Dung_bettles_Sahara_east_Morocco_deCastro-Arrazola_etal2018_AFE"
dt <- read.csv(paste0(files_dir, "/rawdata_DungBeetlesSahara.csv"), h=T)


# structure ====================================================================
dim(dt) #1844 x 26 
head(dt, 2L)

unique(dt$Notes.on.capture.method)
unique(dt$Notes.on.sample.preservation)


# abundances ===================================================================
range(dt$Abundance) # 1 to 4438, no 0s, NAs or NULLs
non_dups <- dt %>%
  distinct()        # idem, ok
 

# temporal =====================================================================
unique(dt$Day)   # ok
unique(dt$Month) # ok (wet and dry seasons)
unique(dt$Year)  # ok

c1 <- dt %>% group_by(Plot, Month, Year) %>% summarise(nDay=n_distinct(Day)) # 1


# plot =========================================================================
sort(unique(dt$Plot)) # 21

# NOTE: plot mor.3C, only surveyed in 2014, removed 
dt <- dt[!dt$Plot=="mor.3C",]

c2 <- dt %>% group_by(Plot) %>% summarise(nLat=n_distinct(Latitude), 
                                          nLong=n_distinct(Longitude), 
                                          nElev=n_distinct(DepthElevation)) # a unique val throughout the series for each of the site*variable pair
c3 <- dt %>% group_by(Plot) %>% summarise(nTrap=n_distinct(Trap), nTrapID=n_distinct(TrapID))
# always 5 traps per site transect replicate, ok


# depth elevation ==============================================================
range(dt$DepthElevation) # 1-1347
unique(dt$Plot[dt$DepthElevation==1]) # plot mor10A which is on the coast near Saïdia


# coordinates ==================================================================
range(dt$Latitude)
range(dt$Longitude)

length(unique(dt$Latitude))
length(unique(dt$Longitude))

class(dt$Latitude)
class(dt$Longitude)

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

points_zoom <- points +
  ylim(20,50)+
  xlim(-10,10)
points_zoom    


# taxonomy =====================================================================
unique(dt$Family)  # 3 
unique(dt$Genus)   # 34 
sort(unique(dt$Species)) # 61 

dt$Genus[dt$Species=="nsp"] # Rhyssemus
sum(dt$Species=="nsp")      # 2
#View(dt[dt$Genus=="Rhyssemus",])

dt$Species[dt$Species=="nsp"] <- "sp" # standardised to BT


# aggregate data ===============================================================

dt$Site <- str_sub(dt$Plot, end = -2)
sort(unique(dt$Site)) # 10, OK
c4 <- dt %>% group_by(Site, Month, Year) %>% summarise(n=n_distinct(Day)) # two cases of two different days.

dt$Day[dt$Site=="mor.8" & dt$Month==4 & dt$Year==2013]
dt$Day[dt$Site=="mor.8" & dt$Month==4 & dt$Year==2013] <- 13
dt$Day[dt$Site=="mor.9" & dt$Month==4 & dt$Year==2013]
dt$Day[dt$Site=="mor.9" & dt$Month==4 & dt$Year==2013] <- 13

c5 <- dt %>% group_by(Site) %>% summarise(nDiffLat=max(Latitude)-min(Latitude),
                                          nDiffLong=max(Longitude)-min(Longitude),
                                          nDiffElevation=max(DepthElevation)-min(DepthElevation)) # max 36 m difference
str(dt)

avs <- dt %>% group_by(Site) %>% summarise(Latitude=mean(Latitude),
                                          Longitude=mean(Longitude),
                                          DepthElevation=round(mean(DepthElevation), 2)) # avs Lat, Long & Elevation

dt$Latitude <- avs$Latitude[match(dt$Site, avs$Site)]
dt$Longitude <- avs$Longitude[match(dt$Site, avs$Site)]
dt$DepthElevation <- avs$DepthElevation[match(dt$Site, avs$Site)]

rawdata <- dt %>% group_by(Family, Genus, Species, Site, Latitude, Longitude, DepthElevation, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) # 405


sum(rawdata$Abundance) # 70697 
range(rawdata$Abundance)


# re-check coordinates OK:------------------------------------------------------
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=rawdata, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

points_zoom <- points +
  ylim(20,50)+
  xlim(-10,10)
points_zoom    # OK

# depthelevation: --------------------------------------------------------------
unique(rawdata$DepthElevation)
range(rawdata$DepthElevation)

# rawdata ======================================================================
rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata$SampleDescription <- paste0(rawdata$Site, "_", rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata <- rawdata %>% relocate(c(Plot, SampleDescription), .before=Latitude)
rawdata <-within(rawdata, rm(Site))
rawdata <- rawdata %>% relocate(c(Day, Month, Year), .after=DepthElevation)

rawdata$StudyID <- rep(NA, nrow(rawdata))

rawdata <- as.data.frame(rawdata) 


# inspect final rawdata ========================================================
str(rawdata)
range(rawdata$Abundance) # 1 22722
c6 <- rawdata %>% group_by(Latitude, Longitude) %>% summarise(nSamp=n_distinct(SampleDescription)) # always 4, OK

# write csv ====================================================================

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Dung_bettles_Sahara_east_Morocco_deCastro-Arrazola_etal2018_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# convex hulls =================================================================
# 1. Convert data points into point spatial object
dt_merged <- dt # to obtain the true centroid
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
  coord_sf(xlim = c(-5,5), ylim = c(30, 40)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()  # ok

# End of script ################################################################
