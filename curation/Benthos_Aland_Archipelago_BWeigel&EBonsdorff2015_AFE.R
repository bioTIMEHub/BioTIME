################################################################################
# Study: Data from: Maintained functional diversity in benthic communities in spite of diverging functional identities.
# Curator: AFE
# Date: June 2020 & January 2024
################################################################################


# Main sources =================================================================
# https://datadryad.org/stash/dataset/doi:10.5061/dryad.6hc8q (Dryad)
# http://dx.doi.org/10.1111/oik.02894 (paper publication)


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
myd <- getwd()


# Read Data: ===================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Benthos_Aland_Archipelago_BWeigel&EBonsdorff2015_AFE"
dt <- read.csv(paste0(files_dir, "/Station_x_Species.csv"), h=T)                                   # downloaded from Dryad
dtadd <- read.csv(file = paste0(files_dir,"/Extended data Weigel et al. 2016.csv"), h=T, sep=",")  # contains additional info about the sampling events.
abb <- read.csv(file = paste0(files_dir,"/Abbreviations species and traits.csv"), h=T, sep=",")    # species names abbreviations.


# Data Structure: ==============================================================
dim(dt) # 63 x 25
str(dt) # sampling event is the first col and species are headers of numeric cols.

dt <- gather(dt, key="Species", value="Abundance", -X) # 1512 x 3
head(dt, 3L) 
str(dt)
names(dt)[names(dt)=="X"] <- "SamplingEvent"


# Abundance checks: ============================================================
range(dt$Abundance)         # 0 to 94.8
dt <- dt[!dt$Abundance==0,] # 388 obs


# Sample Event: ================================================================
dt$Year <- as.numeric(unlist(regmatches(dt$SamplingEvent, gregexpr("[[:digit:]]+", dt$SamplingEvent))))
unique(dt$Year)
dt$Station <- unlist(gsub('[[:digit:]]+', '', dt$SamplingEvent))
unique(dt$Station)
sum(unique(dt$Station) %like% "E_") # 8
sum(unique(dt$Station) %like% "S_") # 10

identical(sort(unique(dt$Station)), sort(unique(dtadd$Station))) # FALSE
setdiff(unique(dt$Station), unique(dtadd$Station))               # S_Hjor & S_Slot
setdiff(unique(dtadd$Station), unique(dt$Station))               # 0

dt$Station <- plyr::revalue(dt$Station, c("S_Hjor"="S_Hjort",
                                         "S_Slot"="S_Slott"))    # corrected typos


# Event Date: ==================================================================
# Sampling Day only available for year 2013, otherwise Month & Year

dt$Month <- rep(NA, nrow(dt))
dt$Month <- ifelse(dt$Year==1973, dtadd$Date.1973[match(dt$Station, dtadd$Station)], dt$Month) # 1973
dt$Month <- ifelse(dt$Year==1989, dtadd$Date.1989[match(dt$Station, dtadd$Station)], dt$Month) # 1989
dt$Month <- ifelse(dt$Year==2000, dtadd$Date.2000[match(dt$Station, dtadd$Station)], dt$Month) # 2000
dt$Month <- ifelse(dt$Year==2013, str_split_fixed(dtadd$Date.2013, "\\.", 3)[,2][match(dt$Station, dtadd$Station)], dt$Month) # 2013, always June
unique(dt$Month)
dt$Month <- as.integer(plyr::revalue(dt$Month, c("July"="7",
                                      "June"="6",
                                      "06"="6",
                                      "July "="7",
                                      "May"="5",
                                      "june"="6")))

dt$Day <- rep(NA, nrow(dt))
dt$Day <- ifelse(dt$Year==2013, str_split_fixed(dtadd$Date.2013, "\\.", 3)[,1][match(dt$Station, dtadd$Station)], dt$Day)     # day of sampling 2013
unique(dt$Day)
dt$Day <- as.integer(dt$Day)


# Coordinates: =================================================================
dt$Latitude <- dtadd$WGS84_N[match(dt$Station, dtadd$Station)]
dt$Longitude <- dtadd$WSG84_E[match(dt$Station, dtadd$Station)]

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
  ylim(50,90)+
  xlim(0,50)
points_zoom 


# Taxonomy checks: =============================================================
length(unique(dt$Species)) # 24
head(abb, 2L)
names(abb) <- plyr::revalue(names(abb), c("X"="Species",
                                          "X.1"="Species_abbreviation"))

abb <- abb[c(2:25),]

sort(unique(dt$Species))
sort(unique(abb$Species))

setdiff(dt$Species, abb$Species_abbreviation) #"Har" "Jae"
# Jae must be Jaera albifrons.
# Har is the polychaete Harmothoe sarsi

setdiff(abb$Species_abbreviation, dt$Species) #"Lym" "Bat"
#Lym: Lymneae sp.
#Bat: Bathyporeia pilosa

dt$Taxa <- rep(NA, nrow(dt))
dt$Taxa <- abb$Species[match(dt$Species, abb$Species_abbreviation)]
dt$Taxa <- ifelse(dt$Species=="Har", "Harmothoe sarsi", dt$Taxa)
dt$Taxa <- ifelse(dt$Species=="Jae", "Jaera albifrons", dt$Taxa)

dt$Taxa <- gsub("spp.", "sp", dt$Taxa)
dt$Taxa <- gsub("sp.", "sp", dt$Taxa)
sort(unique(dt$Taxa))

dt$Genus <- str_split_fixed(as.character(dt$Taxa), " ", 2)[, 1]
dt$Species <- str_split_fixed(as.character(dt$Taxa), " ", 2)[, 2]


sort(unique(dt$Genus))
#View(dt[dt$Genus=="Chironomidae",])
#View(dt[dt$Genus=="Ostracoda",])
#View(dt[dt$Genus=="Oligochaeta",])
# the three above are kept in Family, since they represent groups separate from any of the other taxa reported.

dt$Family <- rep(NA, nrow(dt))
dt$Family <- ifelse(dt$Genus %in% c("Chironomidae", "Ostracoda", "Oligochaeta"), dt$Genus, dt$Family)
dt$Genus <- ifelse(dt$Genus %in% c("Chironomidae", "Ostracoda", "Oligochaeta"), NA, dt$Genus)
unique(dt$Family)
unique(dt$Genus)

# "Halicryptus spnulosus" 
# "Cyanophtalma obscura" 
# "Potamopygurus antipodarium"
# "Pygospo sp"
# "Theodoxus fluvatilis" /// typos

dt$Species <- plyr::revalue(dt$Species, c("fluvatilis"="fluviatilis",
                                          "spnulosus"="spinulosus",
                                          "antipodarium"="antipodarum"))
dt$Genus <- plyr::revalue(dt$Genus, c("Cyanophtalma"="Cyanophthalma",
                                      "Potamopygurus"="Potamopyrgus",
                                      "Pygospo"="Pygospio")) # corrected following GBIF

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species))  # 0


# Prep for BT: =================================================================
c1 <- dt %>% group_by(Station) %>% summarise(nYear=n_distinct(Year)) 
# S_Bruk  no data for 1989: monitoring yielded abundance 0
length(unique(dt$Station[dt$Year==1973])) # 16
length(unique(dt$Station[dt$Year==1989])) # 15
length(unique(dt$Station[dt$Year==2000])) # 16
length(unique(dt$Station[dt$Year==2013])) # 16

c2 <- dt %>% group_by(Station, Year) %>% summarise(nMonth=n_distinct(Month)) # always 1


# Aggregate: ===================================================================
rawData <- dt %>% group_by(Family, Genus, Species, Station, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) # 388 obs
range(rawData$Abundance)              # 0.02 94.80

rawData$Biomass <-rep(NA, nrow(rawData))
rawData$Plot <-rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$Station, "_",
                                    rawData$Month, "_",
                                    rawData$Year)
rawData$DepthElevation <-rep(NA, nrow(rawData))
rawData$StudyID <-rep(NA, nrow(rawData))


rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Family)
rawData <- rawData %>% relocate(c(Plot, SampleDescription), .before=Latitude)
rawData <- rawData %>% relocate(c(DepthElevation), .after=Longitude)
rawData <- rawData %>% relocate(c(Day, Month), .before=Year)

names(rawData)
rawData <- as.data.frame(rawData)
rawData <- within(rawData, rm(Station))
str(rawData)


# Write csv ====================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Benthos_Aland_Archipelago_BWeigel&EBonsdorff2015_AFE"
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
  coord_sf(xlim = c(17,25), ylim = c(58, 64)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################