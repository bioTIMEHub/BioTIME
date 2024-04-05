################################################################################
# Study: Spatial and temporal shifts in functional and taxonomic diversity of dung
# beetles in a human-modified tropical forest landscape
# Curator: Wallace Beiroz (BioTIME: AFE)
# Date: 21/7/2020 & December 2023
################################################################################

# Main sources =================================================================
# https://doi.org/10.1016/j.ecolind.2018.07.062


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


# Read data ====================================================================
rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Dung_beetles_Brazil_WallaceBeiroz2018_AFE"
dt <- read_excel(paste0(files_dir, "/Beiroz_data.xlsx"), sheet="rawData")


# Check structure ==============================================================
dim(dt) #13125 x 13
head(dt)
str(dt)


# Abundance & biomass ==========================================================
sum(dt$Abundance <= 0) # 10380
sum(dt$Biomass <= 0)   # 10380

dt <- dt[dt$Abundance > 0,]  # 2745 (0s removed)
range(dt$Biomass)   
range(dt$Abundance) 


# Time =========================================================================
sort(unique(dt$Day)) 
sort(unique(dt$Month)) 
sort(unique(dt$Year)) 

c1 <- dt %>% group_by(Plot, Year) %>% summarise(nMonth=n_distinct(Month), nDay=n_distinct(Day)) # ok, always 1


# Latitude & Longitude =========================================================
length(unique(dt$Latitude))  # 35
length(unique(dt$Longitude)) # 35


world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points


# Plot =========================================================================
sort(unique(dt$Plot)) # 35

#C1 to C8: 8 primary forest corridors
#E1 to E15: 15 Eucalyptus plantations separated
#F1 to F12: 12 undisturbed primary forest sites


# DepthElevation ===============================================================
length(unique(dt$DepthElevation)) # 32
range(dt$DepthElevation)          


# Taxonomy =====================================================================
sort(unique(dt$Genus))
sort(unique(dt$Species))

# sp A, sp B, etc changed to sp1, sp2, etc

dt$Species <- plyr::revalue(dt$Species, c("Ateuchus sp. A"="Ateuchus sp1",
                                          "Ateuchus sp. E"="Ateuchus sp2",
                                          "Ateuchus sp. F"="Ateuchus sp3",
                                          "Canthidium sp. A"="Canthidium sp1",
                                          "Canthidium sp. B"="Canthidium sp2",
                                          "Canthidium sp. D"="Canthidium sp3",
                                          "Canthidium sp. F"="Canthidium sp4",
                                          "Canthidium sp. H"="Canthidium sp5",
                                          "Canthidium sp. K"="Canthidium sp6",
                                          "Canthidium sp. L"="Canthidium sp7",
                                          "Deltochilum sp. A"="Deltochilum sp1",
                                          "Deltochilum sp. B"="Deltochilum sp2",
                                          "Uroxys sp. A"="Uroxys sp1",
                                          "Uroxys sp. B"="Uroxys sp2",
                                          "Uroxys sp. C"="Uroxys sp3"))

sort(unique(dt$Species))

dt$Gen <- str_split_fixed(as.character(dt$Species), " ", 2)[, 1]
dt$Species <- str_split_fixed(as.character(dt$Species), " ", 2)[, 2]


sort(unique(dt$Gen))
identical(as.character(dt$Genus), as.character(dt$Gen)) # FALSE
setdiff(as.character(dt$Genus), as.character(dt$Gen)) # typo, Coprophanaues
setdiff(as.character(dt$Gen), as.character(dt$Genus)) 

dt <- within(dt, rm(Genus))
names(dt)[names(dt)=="Gen"] <- "Genus"

sort(unique(dt$Species))


# Create rawdata ===============================================================
names(dt)
rawData <- dt %>% group_by(Genus, Species, Plot, Latitude, Longitude, 
                           DepthElevation, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass))

sum(rawData$Abundance) # 26528 beetles

rawData <- rawData %>% relocate(c(Abundance, Biomass), .before = Genus)
rawData$Family <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Family), .before = Genus)

rawData$SampleDescription <- paste0(rawData$Plot, "_",
                                    rawData$Day, "_",
                                    rawData$Month, "_",
                                    rawData$Year)
rawData$Plot <- rep(NA, nrow(rawData)) # reserved for fixed plots for sessile organisms
rawData <- rawData %>% relocate(c(Plot, SampleDescription), .after = Species)
rawData$StudyID <- rep(NA, nrow(rawData)) 

rawData <- as.data.frame(rawData)
str(rawData)
sum(is.na(rawData[,c(1,2,4,5,7,8,9,10,11,12,13)])) #0


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Dung_beetles_Brazil_WallaceBeiroz2018_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# convex hulls =================================================================
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

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-56,-50), ylim = c(-4,4)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################




