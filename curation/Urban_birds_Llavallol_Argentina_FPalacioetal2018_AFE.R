################################################################################
#Curation script
#Title: Urbanization as a driver of taxonomic, functional, and phylogenetic diversity losses in bird communities
#Curator: AFE
#Date: 2020 & January 2024
################################################################################


# Main sources -----------------------------------------------------------------
# https://doi.org/10.1139/cjz-2018-0008


# Libraries:--------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)
library(data.table)
library(measurements)


# Read data:--------------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Urban_birds_Llavallol_Argentina_FPalacioetal2018_AFE"
dt0 <- read_excel(paste0(files_dir, "/Bases de datos Palacio et al. 2019 CJZ.xlsx"), sheet="dataset", col_names = T, col_types = NULL)
# NOTE: double heading, thus:
dt <- read_excel(paste0(files_dir, "/Bases de datos Palacio et al. 2019 CJZ.xlsx"), sheet="dataset", col_names = T, skip=1)
sps <- rbind(names(dt0), dt0[1,])
sps <- as.data.frame(t(sps[,-c(1:3)])) # sps Latin names



# Structure: -------------------------------------------------------------------
dim(dt)                                                        
head(dt)
str(dt)

dt <- gather(dt, key="Taxa", value="Abundance", -c(1:3))


# Abundance: -------------------------------------------------------------------
range(dt$Abundance)
sum(dt$Abundance==0)
dt <- dt[dt$Abundance>0,]
range(dt$Abundance)


# Temporal sampling event: -----------------------------------------------------
sort(unique(dt$año)) # designates the two sampling periods (1985-1986 is 1985, 2015-2016 is 2015)

dt$Year <- as.integer(str_split_fixed(as.character(dt$fecha), "-", 3)[, 1])
dt$Month <- as.integer(str_split_fixed(as.character(dt$fecha), "-", 3)[, 2])
dt$Day <- as.integer(str_split_fixed(as.character(dt$fecha), "-", 3)[, 3])

unique(dt$Year)
unique(dt$Month)
unique(dt$Day)


# Coordinates: -----------------------------------------------------------------
# Llavallol (34°48=S, 58°26=W), Lomas de Zamora city
# Both are negative (South & West)

dt$Latitude <- as.numeric(conv_unit("34 48 00", "deg_min_sec", "dec_deg"))*(-1)
dt$Longitude <- as.numeric(conv_unit("58 26 00", "deg_min_sec", "dec_deg"))*(-1)

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points


# Spatial sampling event: ------------------------------------------------------
length(unique(dt$sitio))
c1 <- dt %>% group_by(año) %>% summarise(nSitio=n_distinct(sitio)) # 10
sort(unique(dt$Month[dt$año==1985]))
sort(unique(dt$Month[dt$año==2015]))
c2 <- dt %>% group_by(año) %>% summarise(nEvent=n_distinct(fecha)) # 11 & 12
c3 <- dt %>% group_by(Year, Month, Day) %>% summarise(nSitio=n_distinct(sitio)) # 11 & 12
# 10 except in 2 cases, January 1985 Day 4 (only 1 site)
# April 1985 Day 27 (only 9 sites)
# NOTE: no bird species were observed in the other sampling points in these two events


# Taxonomy: --------------------------------------------------------------------
dt$Taxa2 <- sps$V1[match(dt$Taxa, sps$V2)]
sort(unique(dt$Taxa2))

dt$Genus <- str_split_fixed(dt$Taxa2, " ", 2)[,1]
sort(unique(dt$Genus))
dt$Species <- str_split_fixed(dt$Taxa2, " ", 2)[,2]
sort(unique(dt$Species))


# Rawdata: ---------------------------------------------------------------------
# NOTE: pool abundances for all sites per sampling event

rawData <- dt %>% group_by(Genus, Species, 
                           Latitude, Longitude, 
                           Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance))

rawData$Biomass <- rep(NA, nrow(rawData))
rawData$Family <- rep(NA, nrow(rawData))
rawData$Plot <- rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$Day, "_", 
                                    rawData$Month, "_", 
                                    rawData$Year)
rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData$StudyID <- rep(NA, nrow(rawData))

rawData <- rawData %>% relocate(c(Abundance, Biomass, Family), .before = Genus)
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .before = Latitude)
rawData <- rawData %>% relocate(c(DepthElevation), .before = Day)

rawData <- as.data.frame(rawData)
str(rawData)
range(rawData$Abundance) # 1 250
sum(rawData$Abundance[rawData$Year<2015])
sum(rawData$Abundance[rawData$Year>2014])
 

# Save: ------------------------------------------------------------------------
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Urban_birds_Llavallol_Argentina_FPalacioetal2018_AFE"
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
area # only 1 pair of coordinates

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
  coord_sf(xlim = c(-70,-40), ylim = c(-40,-25)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################


