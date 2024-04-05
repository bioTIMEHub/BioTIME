################################################################################
# Study: Slug Numbers - Beckingham 1999 - 2002 (1999-2001) 
# Curator: AFE
# Date: June 2020 & January 2024
################################################################################


# Main sources =================================================================
# Slug Numbers - Beckingham 1999 - 2002, Chris du Feu 2020


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


# Read Data: ===================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/BeckinghamSlugDataTimeSeries1_CduFeu99-01_years1_2_AFE"
dt <- read.csv(file = paste0(files_dir,"/Years 1-2 Fixed-Movable.csv"), h=T, sep=",")


# Structure: ===================================================================
dim(dt) #32 x 103
head(dt)
str(dt)

names(dt)
names(dt) <- gsub("X","",names(dt))  # rm X in front of date name
dt <- gather(dt, key="Date", value="Abundance", -c(Species, Mat)) # 3232 obs, long format
str(dt)


# Abundance: ===================================================================
range(dt$Abundance)
sum(is.na(dt$Abundance))        # 2461 are NA's
dt <- dt[!is.na(dt$Abundance),] # 771 (NAs removed)

sum(dt$Abundance <= 0)          # Tandonia budapestensis Moveable mat 06.11.99 is a 0
dt <- dt[dt$Abundance > 0,]     # 770 (0s removed)

range(dt$Abundance)             # 1-43 per mat*sampling event


# Sampling Event Date: =========================================================
length(unique(dt$Date))         # 101 sampling events (Approx mat checking every two weeks)

dt$Year <- str_split_fixed(as.character(dt$Date), "\\.", 3)[, 3]
dt$Month <- as.integer(str_split_fixed(as.character(dt$Date), "\\.", 3)[, 2])
dt$Day <- as.integer(str_split_fixed(as.character(dt$Date), "\\.", 3)[, 1])

unique(dt$Year) 
dt$Year <- as.integer(plyr::revalue(dt$Year, c("99"="1999",
                                    "00"="2000",
                                    "01"="2001")))
sort(unique(dt$Month))
sort(unique(dt$Day)) 

#View(dt[dt$Year==2001,]) # last event in March 2001


# Sampling Mat =================================================================
unique(dt$Mat)
sum(dt$Abundance[dt$Mat=="Fixed mat"])
sum(dt$Abundance[dt$Mat=="Moveable mat"])


# Coordinates ==================================================================
# Latitude is positive(N) and longitude is negative(W)
# Coords in degree format 53° 24′ 11.37″ N 0° 49′ 46.74″ W

dt$Latitude <- as.numeric(rep(conv_unit("53 24 11.37", "deg_min_sec", "dec_deg"), nrow(dt)))
dt$Longitude <- as.numeric(rep(conv_unit("-0 49 46.74", "deg_min_sec", "dec_deg"), nrow(dt)))


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
  ylim(45,60)+
  xlim(-10,10)
points_zoom    


# Taxonomy =====================================================================
unique(dt$Species)

# GBIF Backbone Taxonomy:
# "Arion (Carinarion) circumscriptus" to Arion circumscriptus
# "Arion (Kobeltia) distinctus" to Arion distinctus
# "Arion (Carinarion) fasciatus" to Arion fasciatus
# "Arion (Mesarion) subfuscus" to Arion subfuscus
# "Arion (Kobeltia) hortensis" to Arion hortensis
# "Arion (Arion) agg." to Arion agg.
# "Arion (Kobeltia) intermedius" to Arion intermedius

dt$Species2 <- gsub("\\(.*?\\)", "", dt$Species)
dt$Species2 <- gsub("  ", " ", dt$Species2)
unique(dt$Species2)

dt$Species2[dt$Species2=="Limacus sp."] <- "Limacus sp"

dt$Genus <- str_split_fixed(dt$Species2, " ", 2)[,1]
unique(dt$Genus)
dt$Species <- str_split_fixed(dt$Species2, " ", 2)[,2]
unique(dt$Species)
 

# RawData ======================================================================
names(dt)
rawData <- dt %>% group_by(Genus, Species, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) # pool data from both mats 
rawData <- as.data.frame(rawData)
str(rawData)

rawData$Biomass <- rep(NA, nrow(rawData))
rawData$Family <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Abundance, Biomass, Family), .before=Genus)

rawData$SampleDescription <- paste0(rawData$Day, "_", rawData$Month, "_", rawData$Year)
rawData$Plot <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .after=Species)

rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(DepthElevation), .after=Longitude)
rawData$StudyID <- rep(NA, nrow(rawData))

names(rawData)
str(rawData)
range(rawData$Abundance) # 1-44 per sampling event


# Write csv ====================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/BeckinghamSlugDataTimeSeries1_CduFeu99-01_years1_2_AFE"
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
area # 0 because it's only one pair of central coords

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
  coord_sf(ylim=c(53,54), xlim=c(-1,1)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################