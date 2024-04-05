################################################################################
# Study title: Fish Communities - Forillon
# Curator: AFE
# Date: 29/06/2023
################################################################################

# Main sources =================================================================
# https://open.canada.ca/data/en/dataset/fe2441a6-8ae4-4884-b181-cd7ec53bd842

# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(clipr)
library(readxl)
rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/FishCommunities_Forillon_RivFishT_8_AFE"
sps <- read.csv2(paste0(files_dir, "/Forillon_PN_Aquatique_Communautés_poissons_2008-2017_dictionnaire_de_données_2.csv"), h=T, fileEncoding = "latin1")
dt <- read.csv2(paste0(files_dir, "/Forillon_PN_Aquatique_Situation_communautés_poissons_2008-2017_données_1.csv"), h=T, fileEncoding = "latin1")

locs <- read.csv(paste0(files_dir, "/Pêche électrique 2017 - Coordonnées.csv"), h=T)

# General checks ===============================================================
str(dt)
names(dt) <- dt[1,]
dt <- dt[-1,]               # Remove double heading
sort(unique(dt$COMMENTS))   # Some refer to fish data
remove <- names(dt[,c(17,18,20:29)])
dt <- dt[!names(dt) %in% remove]
dt <- hablar::retype(dt)

length(unique(dt$STREAM))      # 11, OK
unique(dt$STREAM)
unique(dt$TYPE_INSTALLATION)   # "Fermée trois côtés" "Fermée" (Closed or closed on three sides)
unique(dt$STATION_DIMENSION)

dtcheck1 <- dt %>% group_by(STREAM) %>% summarise(n=n_distinct(TYPE_INSTALLATION)) # OK, always 1 unique installation per site
dtcheck1B <- dt %>% group_by(STREAM) %>% distinct(TYPE_INSTALLATION) # OK
table(dt$SPECIES) # These are communities with many instances of species richness==1


# Check station dimension ======================================================
dtcheck2 <- dt %>% group_by(STREAM) %>% summarise(minD=min(STATION_DIMENSION), maxD=max(STATION_DIMENSION))
# the variability in areas sampled per site in very small except in Atocas & Petit-Gaspé.

#View(dt[dt$STREAM=="Atocas",])
sort(unique(dt$STATION_DIMENSION[dt$STREAM=="Atocas"]))
sort(unique(dt$COMMENTS[dt$STREAM=="Atocas" & dt$STATION_DIMENSION %in% c(55.5)]))
# NOTES: for 55.5:
# Significant changes in the fishing area. 
# Stream divided into several branches in the forest 
# so it was impossible to obtain a 100 m2 station. Faulty oxygen sensor.

sort(unique(dt$COMMENTS[dt$STREAM=="Atocas" & dt$STATION_DIMENSION %in% c(75)]))
# NOTES: for 75:
# The water level was particularly low. 
# The stretch of river was meandering and segmented into 
# several basins of different sizes which were added together 
# to give the total area. Downstream was blocked by an ice 
# jam and upstream was the CABIN sampling station. 
# The total sampling area was therefore reduced by the presence of these two obstacles.

sort(unique(dt$DATE[dt$STATION_DIMENSION==55.5])) # 2011-08-09
sort(unique(dt$DATE[dt$STATION_DIMENSION==75]))   # 2017-08-03
sort(unique(dt$DATE[dt$STREAM=="Atocas"]))        


#View(dt[dt$STREAM=="Petit-Gaspé",])
sort(unique(dt$STATION_DIMENSION[dt$STREAM=="Petit-Gaspé"]))
sort(unique(dt$COMMENTS[dt$STREAM=="Petit-Gaspé" & dt$STATION_DIMENSION %in% c(122.67)]))


# Abundance & biomasses ========================================================
# Abundance: each row is an individual
# Biomass:
range(dt$WEIGHT)        # 0.1 to 9999
range(dt$LENGTH)        # 22 to 9999

sort(unique(dt$WEIGHT)) # 9999.0 likely a typo
sum(dt$WEIGHT==9999.0)  # 6
sum(dt$LENGTH==9999.0)  # 2

dt$WEIGHT <- ifelse(dt$WEIGHT==9999.0 & dt$LENGTH==9999.0, NA, dt$WEIGHT)             # The comments indicate these are two escapes
dt$WEIGHT[dt$SPECIES=="Épinoche sp." & dt$WEIGHT==9999.0] <- NA                       # Only four obs of these species in the study, just one with weight data
mean(dt$WEIGHT[dt$SPECIES=="Omble de fontaine" & dt$LENGTH==59 & dt$WEIGHT !=9999.0]) # OK, take average for specimens of the same length
dt$WEIGHT[dt$WEIGHT==9999.0 & dt$SPECIES=="Omble de fontaine"] <- 1.93

sum(is.na(dt$WEIGHT))   # 5, OK

# NOTE: given that only 1 out of 4 records of Épinoche sp. in the same sampling event has weights, 
# the record with weight is changed to NA to avoid inequivalent Count - Weight aggregated records
dt$WEIGHT[dt$SPECIES=="Épinoche sp."] <- NA

dt <- dt[! (is.na(dt$WEIGHT) & dt$LENGTH==9999),] # rm to avoid inequivalent Count - Weight aggregated records


# Temporal data ================================================================
dt$YEAR <- as.numeric(str_split_fixed(dt$DATE, "-", 3)[,1])
dt$MONTH <- as.numeric(str_split_fixed(dt$DATE, "-", 3)[,2])
dt$DAY <- as.numeric(str_split_fixed(dt$DATE, "-", 3)[,3])

sort(unique(dt$YEAR))   # 2008 to 2017, OK
sort(unique(dt$MONTH))  # Always July and August, OK
sort(unique(dt$DAY))    # OK, nothing above 31 or under 1

dtcheck3 <- dt %>% group_by(STREAM, YEAR) %>% summarise(nDay=n_distinct(DAY), nM=n_distinct(MONTH)) # OK, always 1 concordant with daily samples


# Location data ================================================================
sort(unique(dt$STREAM))
sort(unique(locs$Nom_ruisseau))
locs$Nom_ruisseau[locs$Nom_ruisseau=="Petit-Gasp\xe9"] <- "Petit-Gaspé"
dt$STREAM <- gsub("_", " ", dt$STREAM)
dt$STREAM <- gsub("-", " ", dt$STREAM)
locs$Nom_ruisseau <- gsub("-", " ", locs$Nom_ruisseau)
setdiff(locs$Nom_ruisseau, unique(dt$STREAM))
setdiff(unique(dt$STREAM), locs$Nom_ruisseau)

locs$Nom_ruisseau[locs$Nom_ruisseau=="Kavanah"] <- "Kavanagh"
locs$Nom_ruisseau[locs$Nom_ruisseau=="English 2008"] <- "English"

# Remove sampling in English site in year 2017 ---------------------------------
dt <- dt[!(dt$STREAM=="English" & dt$YEAR==2017),]

# NOTE:
# New fishing site on English Creek in year 2017 relocated 300m upstream because 
# a beaver family was established at the orginal site in 2017. 
# After consulting, we were recommended to consider it a separate series since  
# data comparability in both locations was not certain. Because there's only one
# year of sampling in this new location, we remove it.

# Assign coordinates:-----------------------------------------------------------
dt$LATITUDE <- locs$Latitude[match(dt$STREAM, locs$Nom_ruisseau)]
dt$LONGITUDE <- locs$Longitude[match(dt$STREAM, locs$Nom_ruisseau)]
sum(is.na(dt$LATITUDE))
sum(is.na(dt$LONGITUDE))

length(unique(dt$LATITUDE))  # 11, OK
length(unique(dt$LONGITUDE)) # 11, OK

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=LONGITUDE, y=LATITUDE, alpha=0.01)) 
points       

#Zoom:
points_zoom <- points +
  ylim(40,55)+
  xlim(-70,-55)
points_zoom # OK

# DepthElevation ===============================================================

dtcheck4 <- dt %>% group_by(STREAM, DAY, MONTH, YEAR) %>% summarise(n=n_distinct(DEPTH)) # Always 1
range(dt$DEPTH) # OK

# Taxonomy =====================================================================
sort(unique(dt$SPECIES)) # 5 species only

dt$LATIN <- plyr::revalue(dt$SPECIES, c("Anguille d'Amérique"="Anguilla rostrata",
                                        "Épinoche à trois épines"="Gasterosteus aculeatus",
                                        "Épinoche sp."="Gasterosteidae sp1",
                                        "Omble de fontaine"="Salvelinus fontinalis",
                                        "Saumon atlantique"="Salmo salar"))

dt$GENUS <- str_split_fixed(dt$LATIN, " ", 2)[,1]
dt$SPECIES <- str_split_fixed(dt$LATIN, " ", 2)[,2]  # substitute by info in BT format
dt$FAMILY <- ifelse(dt$GENUS %in% c("Gasterosteus", "Gasterosteidae"), "Gasterosteidae", NA)
dt$FAMILY <- ifelse(dt$GENUS %in% c("Anguilla"), "Anguillidae", dt$FAMILY)
dt$FAMILY <- ifelse(dt$GENUS %in% c("Salmo", "Salvelinus"), "Salmonidae", dt$FAMILY)

sort(unique(dt$SPECIES))  # OK
sort(unique(dt$GENUS))    # OK
sort(unique(dt$FAMILY))   # OK

dt$GENUS[dt$GENUS=="Gasterosteidae"] <- NA   # No family records in Genus column


# Sample_desc & rawdata agg ====================================================
round(dt$STATION_WIDTH*dt$STATION_LENGTH, digits=0)==round(dt$STATION_DIMENSION, digits=0) 
# NOTE = above always true except mainly for some cases when width or length == 9999
# Standardize by area (STATION_DIMENSION)

dtcheck5 <- dt %>% group_by(STREAM, YEAR) %>% summarise(n=n_distinct(STATION_DIMENSION)) # OK, always 1

sort(unique(dt$TIME_PASS1))
sort(unique(dt$FISHING_TIME_PASS1))
sort(unique(dt$TIME_PASS2))
sort(unique(dt$FISHING_TIME_PASS2))
sort(unique(dt$TIME_PASS3))
sort(unique(dt$FISHING_TIME_PASS3))
#View(dt[dt$TIME_PASS2=="00:00",]) # Al diurnal surveys, these seem just a typo

sort(unique(dt$PASS))              # 1 2 3 (OK) Indicates the pass associated with the fish capture
dt$ABUNDANCE <- 1                  # each record is an observation of 1 fish, so this columns is provisional to be able to aggregate abundance and biomass at the same time

dt <- as.data.frame(dt)
dtraw <- dt %>% group_by(FAMILY, GENUS, SPECIES, STREAM, STATION_DIMENSION, LATITUDE, LONGITUDE, DEPTH, DAY, MONTH, YEAR) %>%
  summarise(Abundance=sum(ABUNDANCE), Biomass=sum(WEIGHT)) %>% ungroup() # 69 obs

sum(is.na(dtraw$Abundance))    # 0, OK
sum(dtraw$Abundance==0)        # 0, OK
sum(is.na(dtraw$Biomass))      # 1, OK
sum(dtraw$Biomass==0, na.rm=T) # 0, OK 

dtraw$Abundance <- (dtraw$Abundance/dtraw$STATION_DIMENSION)*100                # Density as ind/100m2
dtraw$Biomass <- (dtraw$Biomass/dtraw$STATION_DIMENSION)*100                  # Density as weights/100m2

names(dtraw) <- gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", names(dtraw), perl=TRUE)

dtraw <- dtraw %>% relocate(c(Abundance, Biomass), .before=Family)
dtraw$SampleDescription <- paste0(dtraw$Stream, "_",
                                  dtraw$Day, "_",
                                  dtraw$Month, "_",
                                  dtraw$Year)
dtraw$Plot <- rep(NA, nrow(dtraw))
dtraw <- dtraw %>% relocate(c(SampleDescription, Plot), .before=Latitude)
names(dtraw)[names(dtraw) == "Depth"] <-"DepthElevation"

dtraw$StudyID <- rep(NA, nrow(dtraw))
dtraw <- within(dtraw, rm(Stream, Station_dimension))

str(dtraw)
dim(dtraw)
range(dtraw$Abundance)
sum(is.na(dtraw$Biomass))
range(dtraw$Biomass, na.rm=T)

rawdata <- dtraw

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/FishCommunities_Forillon_RivFishT_8_AFE"
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
  coord_sf(xlim = c(-65,-64), ylim = c(48,49)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################