################################################################################
# Study: Spatial and temporal changes in the dung beetle diversity of a protected, but fragmented, landscape of the northernmost Neotropical rainforest
# Curator: Mario Favila & Renato Salomão (BioTIME: AFE)
# Date: July 2020 & December 2023
################################################################################


# Main sources =================================================================
# https://doi.org/10.1016/j.ecolind.2019.105968


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

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read Data: ===================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Dung_beetles_Mexico_RSalomao&MFavila_99-00_16-17_AFE"
dt <- read_excel(paste0(files_dir, "/Scarabaeinae_BioTIMETemplate.xlsx"), sheet="rawData")


# Structure: ===================================================================
dim(dt) #1260 x 15
head(dt)
str(dt)


# Abundances and biomasses: ====================================================
range(dt$Abundance)       # 1 to 105 ind

unique(dt$Biomass)
sum(dt$Biomass=="NULL")   # 148
sum(is.null(dt$Biomass)) 
sum(dt$Biomass=="NA")     # 0
sum(is.na(dt$Biomass))

dt[dt$Biomass=="NULL",] 
 

dt$Biomass[dt$Biomass=="NULL"] <- NA
sum(is.na(dt$Biomass)) # 148
dt$Biomass <- as.numeric(as.character(dt$Biomass))
range(dt$Biomass, na.rm=T) # 1.30 to 5305.14 (g)


sum(dt$Abundance)        # 4593 total ind


# Time: ========================================================================
str(dt)
unique(dt$Day)  # OK
unique(dt$Month)
unique(dt$Year)

dt$Month <- plyr::revalue(dt$Month, c("ago"=8,
                                      "april"=4,
                                      "august"=8,
                                      "feb"=2,
                                      "february"=2,
                                      "january"=1,
                                      "jun"=6,
                                      "june"=6,
                                      "mar"=3,
                                      "may"=5,
                                      "oct"=10,
                                      "october"=10)) # standardise to BT
dt$Month <- as.integer(dt$Month)


# Latitude & longitude: ========================================================
unique(dt$Latitude)
unique(dt$Longitude)
length(unique(paste0(dt$Latitude, dt$Longitude))) # 9

dt$Latitude <- gsub("[^0-9.-]", " ", dt$Latitude)
dt$Latitude <- trimws(dt$Latitude)

dt$Longitude <- gsub("[^0-9.-]", " ", dt$Longitude)
dt$Longitude <- trimws(dt$Longitude)

dt$LatitudeII <- paste0(as.character(dt$Latitude), " 0")
dt$LongitudeII <- paste0(as.character(dt$Longitude), " 0")

dt$LatitudeII <- as.numeric(conv_unit(dt$LatitudeII, "deg_min_sec", "dec_deg")) 
dt$LongitudeII <- as.numeric(conv_unit(dt$LongitudeII, "deg_min_sec", "dec_deg"))*(-1)


# Check location: ==============================================================
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=LongitudeII, y=LatitudeII, alpha=0.01)) 
points  

points_zoom <- points +
  ylim(0,50)+
  xlim(-120,-50)
points_zoom    # OK


# Site =========================================================================

unique(dt$Plot) #NA
unique(dt$Habitatype) # 2
unique(dt$SampleDescription) # 17 
length(unique(paste0(dt$SampleDescription, "_", dt$Habitatype))) # 17


dt$SampleDescription <- plyr::revalue(dt$SampleDescription, c("C_buenavista"="CBuenavista", # Cerro Buenavista
                                                              "C_sonpaso"="CSonpaso",       # Cerro Sonpaso
                                                              "El_gallo"="ElGallo",         # Cerro El Gallo
                                                              "El_tigre"="ElTigre",         # La Cabaña del Tigre
                                                              "P_buenavista"="PBuenavista", # Pastizal Buena Vista
                                                              "P_cortez"="PCortez",         # Pastizal Cortel (A_Cortez will be Acahual Cortez)
                                                              "P_megallo"= "PMegallo",      # Pastizal Megallo
                                                              "P_pipiapan"="PPipiapan",     # Pastizal Pipiapan
                                                              "P_tigre"="PTigre", # Pastizal Tigre
                                                              "P_velasco"="PVelasco"))      # Pastizal Velasco
dt$SampleDescription <- gsub("_", "", dt$SampleDescription)   # rm underscore because it is used to split elements of sample decription/ sampling event ( see below)

dt$SampleDescription <- paste0(dt$SampleDescription, "_", dt$Habitatype)
sort(unique(dt$SampleDescription))
# 17 sites (11 forest, 6 pasture)

c1 <- dt %>% group_by(SampleDescription) %>% summarise(NYear=n_distinct(Year),
                                                       nMonth=n_distinct(Month)) 
# NOTE: different sites monitored with different frequencies, OK

c2 <- dt %>% group_by(SampleDescription, Month, Year) %>% summarise(nDay=n_distinct(Day))

#View(dt[dt$SampleDescription=="CMegallo_forest" & dt$Month==10 & dt$Year==1999,])
#View(dt[dt$SampleDescription=="ElGallo_forest" & dt$Month==5 & dt$Year==2000,])
#View(dt[dt$SampleDescription=="ElTigre_forest" & dt$Month==4 & dt$Year==1999,])
#View(dt[dt$SampleDescription=="ElTigre_forest" & dt$Month==6 & dt$Year==1999,])
#View(dt[dt$SampleDescription=="Pipiapan_forest" & dt$Month==5 & dt$Year==2000,])
#View(dt[dt$SampleDescription=="Pipiapan_forest" & dt$Month==10 & dt$Year==1999,])
# NOTE: confirmed that samples on different days in the same month and year are different sampling events.

# Taxonomy: ====================================================================
sort(unique(as.character(dt$Family)))      # All Scarabaeidae
sort(unique(as.character(dt$Genus)))       # Scarabaeinae is a subfamily
sum(dt$Genus=="Scarabaeinae") # 2
View(dt[dt$Genus=="Scarabaeinae",])        # OK, unidentified individuals. Records are kept as sp1.
dt$Genus[dt$Genus=="Scarabaeinae"] <- NA   # standardise to BT
sort(unique(as.character(dt$Species)))


# Elevation: ===================================================================
range(dt$DepthElevation)          # 150-870 m
length(unique(dt$DepthElevation)) # 12 (some sites with the same elevation)


# Rawdata: =====================================================================
names(dt)

# View(dt[is.na(dt$Biomass),])
# 148/1260*100  # 12% obs
# Keep data unaggregated

rawdata <- within(dt, rm(Habitatype, Plot, Latitude, Longitude))

rawdata <- rawdata %>% relocate(c(LatitudeII, LongitudeII), .before=DepthElevation)
names(rawdata)[names(rawdata)=="LatitudeII"] <- "Latitude"
names(rawdata)[names(rawdata)=="LongitudeII"] <- "Longitude"


rawdata$SampleDescription <- paste0(rawdata$SampleDescription, "_", rawdata$Day, "_",
                                    rawdata$Month, "_", rawdata$Year)

str(rawdata)
sum(is.na(rawdata$Abundance)) # 0
sum(is.na(rawdata$Biomass))   # 148


# Write csv ====================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Dung_beetles_Mexico_RSalomao&MFavila_99-00_16-17_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# convex hulls =================================================================
# 1. Convert data points into point spatial object
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
  coord_sf(xlim = c(-100,-90), ylim = c(15, 20)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
