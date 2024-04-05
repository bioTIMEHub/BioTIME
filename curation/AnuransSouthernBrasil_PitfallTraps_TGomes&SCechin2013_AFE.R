################################################################################
# Study: Anurans Pitfall Traps Southern Brazil
# Curator: AFE
# Date: June 2020 & January 2024
################################################################################

# Main sources =================================================================
# The role of phytophysiognomies and seasonality on the structure of ground-dwelling anuran (Amphibia) in the Pampa biome, southern Brazil. 


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)
library(measurements)
library(data.table)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")


# Read data: ===================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/AnuransSouthernBrasil_PitfallTraps_TGomes&SCechin2013_AFE"
dt <- read_excel(paste0(files_dir, "/BioTIMETemplate_Anfibios CISM SONIA.xlsx"), sheet="rawData") 


# Structure: ===================================================================
dim(dt) # 1114 x 14
str(dt)


# Abundance: ===================================================================
sum(is.na(dt$Abundance)) # 44
dt[is.na(dt$Abundance),]
dt <- dt[!is.na(dt$Abundance),]
range(dt$Abundance)      # 1 - 29
sum(dt$Abundance)        # 2176
unique(dt$Biomass)


# Taxonomy =====================================================================
sort(unique(dt$Family))
sort(unique(dt$Genus))
dt$Genus <- str_trim(dt$Genus)
sort(unique(dt$Species))
dt$Species <- gsub("sp.", "sp", dt$Species)

dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]
sort(unique(dt$Species))

sum(is.na(dt$Family) | is.na(dt$Genus) | is.na(dt$Species))
  
  
# Sample Event =================================================================
sort(unique(dt$Day))
sort(unique(dt$Month))
sort(unique(dt$Year))

sum(dt$Year==1994)        # 6
dt <- dt[!dt$Year==1994,] # pilot study

c1 <- dt %>%
  group_by(Month, Year) %>%
  summarise(nDay = n_distinct(Day))


# Coordinates ==================================================================
unique(dt$Plot, na.rm=T)
unique(dt$SampleDescription, na.rm=T)
unique(dt$Latitude)
unique(dt$Longitude)

dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)


world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

(points_zoom <- points +
    ylim(-35,-25)+
    xlim(-60,-50)) 


# Create rawData: ==============================================================

rawData <- dt %>% 
  group_by(Family, Genus, Species, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) # 1063 obs
sum(rawData$Abundance)                # 2170 inds

rawData$Biomass <- rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$Latitude, "_",
                                    rawData$Longitude, "_",
                                    rawData$Day, "_",
                                    rawData$Month, "_",
                                    rawData$Year)
rawData$Plot <- rep(NA, nrow(rawData))
rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData$StudyID <- rep(NA, nrow(rawData))


rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Family)
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .before=Latitude)
rawData <- rawData %>% relocate(c(DepthElevation), .before=Day)


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/AnuransSouthernBrasil_PitfallTraps_TGomes&SCechin2013_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# Convex hulls =================================================================
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
area # 0 because it's only one pair of central coords


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
  coord_sf(ylim=c(-30,-29.6), xlim=c(-54,-53.6)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################