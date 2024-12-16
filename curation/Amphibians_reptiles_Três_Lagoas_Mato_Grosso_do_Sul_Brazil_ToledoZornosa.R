################################################################################
# Curation script
# Title: Amphibians and reptiles of Três Lagoas, Mato Grosso do Sul, Brazil
# Source title: (Unpublished data)
# Curator: AFE
# Date: 21-6-2020 & January 2024
################################################################################


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
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Amphibians_reptiles_Três_Lagoas_Mato_Grosso_do_Sul_Brazil_Toledo&Zornosa_AFE"
dt <- read_excel(paste0(files_dir, "/dataFelipeT.xlsx"), sheet="rawData")


# Check structure: -------------------------------------------------------------
dim(dt) # 246 x 13
str(dt)
class(dt)

dt <- dt[-c(247:265),] # rm template data key
dt$Abundance <- as.numeric(dt$Abundance)

range(dt$Abundance)    # 1 7


# Temporal sampling event: -----------------------------------------------------

sort(unique(dt$Day))
sort(unique(dt$Month))
sort(unique(dt$Year))


# Plot (sampling unit): --------------------------------------------------------
unique(dt$Plot) 
unique(dt$SampleDescription) 


# Elevation: -------------------------------------------------------------------
unique(dt$DepthElevation) # no data


# Coordinates: -----------------------------------------------------------------

unique(dt$Latitude) 
unique(dt$Longitude) 

dt$Latitude <- gsub("[^0-9.-]", " ", dt$Latitude)
dt$Latitude <- trimws(dt$Latitude)
dt$Longitude <- gsub("[^0-9.-]", " ", dt$Longitude)
dt$Longitude <- trimws(dt$Longitude)

dt$Latitude <- as.numeric(conv_unit(dt$Latitude, "deg_min_sec", "dec_deg")) * (-1)
dt$Longitude <- as.numeric(conv_unit(dt$Longitude, "deg_min_sec", "dec_deg")) * (-1)


world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points 

c1 <- dt %>% group_by(Plot) %>% summarise(nLat=n_distinct(Latitude),
                                          nLong=n_distinct(Longitude))


dt_merged <- dt 
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  
area 

dt$Longitude <- -51.65748
dt$Latitude <- -20.74986


# Taxonomy: --------------------------------------------------------------------
sort(unique(dt$Genus))
sort(unique(dt$Species))

#Keep: "cf. bicolor", "cf. ocellifer".
#Change: "sp. (aff. lineata)" to "aff. lineata"

head(dt[dt$Species=="cf. bicolor",])
head(dt[dt$Species=="cf. ocellifer",])

dt$Species <- plyr::revalue(dt$Species, c("sp. (aff. lineata)"="aff. lineata"))


# RawData: ---------------------------------------------------------------------

c2 <- dt %>% group_by(Year) %>% summarise(nMonth=n_distinct(Month), nDay=n_distinct(Day))

# year 2010
unique(dt$Day[dt$Year==2010])


# year 2006
unique(dt$Day[dt$Year==2006 & dt$Month==7])
unique(dt$Day[dt$Year==2006 & dt$Month==10])


# year 2007
unique(dt$Day[dt$Year==2007 & dt$Month==2])
unique(dt$Day[dt$Year==2007 & dt$Month==4])
unique(dt$Day[dt$Year==2007 & dt$Month==5]) # Month 4 & 5 constitute the same sampling event


# year 2008
unique(dt$Day[dt$Year==2008 & dt$Month==4])
unique(dt$Day[dt$Year==2008 & dt$Month==6])

unique(dt$Day[dt$Year==2008 & dt$Month==8]) 
unique(dt$Day[dt$Year==2008 & dt$Month==9]) # Month 8 & 9 constitute the same sampling event

unique(dt$Day[dt$Year==2008 & dt$Month==11]) 
unique(dt$Day[dt$Year==2008 & dt$Month==12]) # Month 11 & 12 constitute the same sampling event


# year 2009
unique(dt$Day[dt$Year==2009 & dt$Month==3])
unique(dt$Day[dt$Year==2009 & dt$Month==6])
unique(dt$Day[dt$Year==2009 & dt$Month==9])


# NOTE: at each sampling event, the three traps were open for 4-5 consecutive days
# hence, month and day do not represent one unique event in this dataset.
# In SampleDescription: the first month of the event will be used in concat with Year
# Data from the three pitfall traps will be pooled.


dt$Month <- ifelse(dt$Month ==5 & dt$Year==2007, 4, dt$Month)
dt$Month <- ifelse(dt$Month ==9 & dt$Year==2008, 8, dt$Month)
dt$Month <- ifelse(dt$Month ==12 & dt$Year==2008, 11, dt$Month)

c3 <- dt %>% group_by(Month, Year) %>% summarise(nPit=n_distinct(Plot)) 
# NOTE: mostly 3, 2 in a few cases, likely because no specimens were captured in one of the pitfall traps

rawData <- dt %>% group_by(Genus, Species,Latitude, Longitude, Month, Year) %>%
  summarise(Abundance=sum(Abundance))
range(rawData$Abundance) # 1 to 16


rawData$Biomass <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Genus)
rawData$Family <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(Family, .before=Genus)
rawData$Plot <- rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$Month, "_", rawData$Year)
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .after=Species)

rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(DepthElevation, .after=Longitude)

rawData$Day <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(Day, .after=DepthElevation)
rawData$StudyID <- rep(NA, nrow(rawData))


sum(is.na(rawData[,c(1,4,5,6,8,9,12,13)])) # 0
rawData <-as.data.frame(rawData)
str(rawData)


# Save: ------------------------------------------------------------------------
getwd()
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Amphibians_reptiles_Três_Lagoas_Mato_Grosso_do_Sul_Brazil_Toledo&Zornosa_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# Convex hulls --------------------------------------------------------------------
# 1. Convert data points into point spatial object
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


world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-60,-40), ylim = c(-30,-10)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################




