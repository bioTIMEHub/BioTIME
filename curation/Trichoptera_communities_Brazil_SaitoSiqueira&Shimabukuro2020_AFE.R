##################################################################################
#Curation script
#Title: Four-year study of Trichoptera communities in nine close subtropical streams of Brazil
#Curator: Erika Mayumi Shimabukuro and AFE
#Date: 21-1-2021
##################################################################################

# Main sources -------------------------------------------------------------------
# https://doi.org/10.1111/fwb.13738


# Libraries:----------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(data.table)


# Read data:----------------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Trichoptera_communities_Brazil_SaitoSiqueira&Shimabukuro2020_AFE"
dt <- read_excel(paste0(files_dir, "/BioTIMETemplateJune20.xlsx"), sheet="rawData")


# Data structure: ----------------------------------------------------------------
dim(dt) # 743  15
str(dt)

dt <- dt[-c(725:743),] # rm template key

# Check abundances: --------------------------------------------------------------
dt$Abundance <- as.numeric(dt$Abundance)
dt$Biomass <- as.numeric(dt$Biomass)

range(dt$Abundance)  # 0 160
sum(dt$Abundance==0) # 1
dt[dt$Abundance==0,] # one observation of the Genus: Smicridea
dt <- dt[!dt$Abundance==0,] # 723

range(dt$Biomass) 
dt$Biomass <- NA


# Check temporal fields: ---------------------------------------------------------
sort(unique(dt$Day)) 
sort(unique(dt$Month)) 
sort(unique(dt$Year))


# Check number of sampling campaigns: ---------------------------------------------
dplyr::count(unique(dt[,c(11:12)])) # 11 events


# Check DepthElevation: -----------------------------------------------------------
unique(dt$DepthElevation) #8, two have the same elevation


# Check location:------------------------------------------------------------------
length(unique(dt$Latitude)) #9
length(unique(dt$Latitude)) #9 (decimal separation not read)

dt$Latitude <- as.numeric(sub("(\\d{2})(\\d+)", "\\1.\\2", dt$Latitude))
dt$Longitude <- as.numeric(sub("(\\d{2})(\\d+)", "\\1.\\2", dt$Longitude))

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
  ylim(-40,10)+
  xlim(-70,-20)
points_zoom  #Correct


# Taxonomic fields: ---------------------------------------------------------------
length(unique(dt$Family)) # 13
sort(unique(dt$Family))

# View(dt[dt$Family=="Anomalopshyche",])
# https://www.gbif.org/species/7955 (for consultation)

dt$Family[dt$Family=="Anomalopshyche"] <- "Anomalopsychidae"

length(unique(dt$Genus))  # 38
sort(unique(dt$Genus))

#View(dt[dt$Genus %like% "_A",])
#View(dt[dt$Genus %like% "_B",])

unique(dt$Species) 
dt$Species <- rep("sp", nrow(dt))
dt$Species <- ifelse(dt$Genus %like% "_A", "sp1", dt$Species)
dt$Species <- ifelse(dt$Genus %like% "_B", "sp2", dt$Species)

dt$Genus <- ifelse(dt$Genus %like% "_A", NA, dt$Genus)
dt$Genus <- ifelse(dt$Genus %like% "_B", NA, dt$Genus)


# Rawdata: ------------------------------------------------------------------------
dt$Taxa3 <- paste0(dt$Family, "_", dt$Genus, "_", dt$Species)
c1 <- dt %>% group_by(Latitude, Longitude, Day, Month, Year) %>% summarise(nTaxa=n_distinct(Taxa3)) # 3 to 16 taxa per sampling event
c2 <- dt %>% group_by(Year) %>% summarise(nLat=n_distinct(Latitude), nLong=n_distinct(Longitude))   # 9
c3 <- dt %>% group_by(Latitude, Longitude) %>% summarise(nEle=n_distinct(DepthElevation))           # 1
dt <- dt[,!names(dt) %in% "Taxa3"]

# NOTE: data are already aggregated.
rawData <- dt
sum(is.na(rawData[,c(1,3,5,6,8,9,10,11,12,13)])) # 0, OK
str(rawData)


# Save: ---------------------------------------------------------------------------
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Trichoptera_communities_Brazil_SaitoSiqueira&Shimabukuro2020_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# Convex hulls --------------------------------------------------------------------
# 1. Convert data points into point spatial object
dt_merged <- rawData 
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

range(rawData$Latitude)
range(rawData$Longitude)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-48,-41), ylim = c(-30,-20)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################

