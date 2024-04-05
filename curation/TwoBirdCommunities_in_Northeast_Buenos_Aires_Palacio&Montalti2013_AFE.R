################################################################################
#Curation script
#Title: Two bird communities in North East of Buenos Aires province, Argentina
#Curator: AFE
#Date: 2020 & January 2024
################################################################################


# Main sources -----------------------------------------------------------------
# SEASONAL VARIATION AND EFFECT OF NON-NATIVE INVASIVE 
# VEGETATION ON TWO BIRD COMMUNITIES IN NORTHEAST OF 
# BUENOS AIRES PROVINCE, ARGENTINA


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
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/TwoBirdCommunities_in_Northeast_Buenos_Aires_Palacio&Montalti2013_AFE"
dt <- read_excel(paste0(files_dir, "/Base de datos Palacio & Montalti 2013 ON.xlsx"), sheet="obs.abundance")
sps <- read_excel(paste0(files_dir, "/Base de datos Palacio & Montalti 2013 ON.xlsx"), sheet="species codes")


# Structure: -------------------------------------------------------------------
dim(dt) # 240 x 50
head(dt)
str(dt)

dt <- gather(dt, key="Taxa", value="Abundance", -c(1:6))
str(dt)


# Abundance: -------------------------------------------------------------------
range(dt$Abundance)
dt <- dt[dt$Abundance>0,] # 610 obs, 1 to 22


# Taxonomy: --------------------------------------------------------------------
length(unique(dt$Taxa))   # 44
dt$Taxa2 <- sps$species.name[match(dt$Taxa, sps$code)]
sum(is.na(dt$Taxa2))      # 83
sort(unique(dt$Taxa2))

unique(dt$Taxa[is.na(dt$Taxa2)])
# "elapar" "ictpyr" "micmel" "molbon" "spimag" "zenaur"
# View(dt[is.na(dt$Taxa2),])

dt$Taxa2[dt$Taxa2=="Buteo magnirostris"] <- "Rupornis magniostris" # change to valid name
dt$Taxa2 <- ifelse(dt$Taxa=="molbon", "Molothrus bonariensis", dt$Taxa2)
dt$Taxa2 <- ifelse(dt$Taxa=="elapar", "Elaenia parvirostris", dt$Taxa2)
dt$Taxa2 <- ifelse(dt$Taxa=="zenaur", "Zenaida auriculata", dt$Taxa2)

# NOTE: main source checked to retrieve the ids below

# 1) spimag as spomag "Sporagra magellanica" (Typo)
dt$Taxa2 <- ifelse(dt$Taxa=="spimag", "Sporagra magellanica", dt$Taxa2)

# 2) micmel == black-capped warbling finch, Poospiza melanoleuca (poomel)
# with the invalid synonim= Microspingus melanoleucus (micmel).
dt$Taxa2 <- ifelse(dt$Taxa=="micmel", "Poospiza melanoleuca", dt$Taxa2)

# 3) ictpyr. 
# previously Icterus cayanensis and Icterus pyrrhopterus were considered the same species,
# but very recently they have been split. 
# Icterus pyrrhopterus as in the Appendix of the source.
dt$Taxa2 <- ifelse(dt$Taxa=="ictpyr", "Icterus pyrrhopterus", dt$Taxa2)

sum(is.na(dt$Taxa2))


dt$Genus <- str_split_fixed(as.character(dt$Taxa2), " ", 2)[, 1]
dt$Species <- str_split_fixed(as.character(dt$Taxa2), " ", 2) [, 2]

sort(unique(dt$Genus))
sort(unique(dt$Species))


# Temporal sampling event: -----------------------------------------------------
dt$Year <- as.integer(str_split_fixed(as.character(dt$date), "-", 3)[, 1])
dt$Month <- as.integer(str_split_fixed(as.character(dt$date), "-", 3)[, 2])
dt$Day <- as.integer(str_split_fixed(as.character(dt$date), "-", 3)[, 3])

unique(dt$Year)
unique(dt$Month)
unique(dt$Day)
range(dt$Day)

c1 <- dt %>% group_by(Day, Month, Year) %>% summarise(nEvent = n_distinct(date.number))
c2 <- dt %>% group_by(date.number) %>% summarise(nDay = n_distinct(Day),
                                                 nMonth = n_distinct(Month),
                                                 nYear = n_distinct(Year))
c3 <- dt %>% group_by(season) %>% summarise(nEvent = n_distinct(date.number)) # 5 to 7


# Coordinates: -----------------------------------------------------------------
# "...located at the Estación Biológica de Aves Silvestres (34°52’S, 58°8’W)."

dt$Latitude <- as.numeric(conv_unit("34 52 00", "deg_min_sec", "dec_deg"))*(-1)
dt$Longitude <- as.numeric(conv_unit("58 08 00", "deg_min_sec", "dec_deg"))*(-1)

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points


# Sampling grain: --------------------------------------------------------------
# Point of counting id (1-10) & habitat (OPEN (shrub) or FOREST).

length(unique(dt$point))          # 10
length(unique(dt$habitat.type))   # 2


length(unique(dt$point[dt$habitat.type=="forest"]))
length(unique(dt$point[dt$habitat.type=="open"]))


# NOTE : pool records in point counts within the same habitat.


# Rawdata: ---------------------------------------------------------------------
rawData <- dt %>% group_by(Genus, Species, habitat.type, Latitude, Longitude,
                           Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance))
range(rawData$Abundance) # 1 to 22


rawData$Biomass <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Genus)
rawData$Family <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(Family, .before=Genus)

rawData$SampleDescription <- paste0(rawData$habitat.type, "_",
                                    rawData$Day, "_", rawData$Month, "_",
                                    rawData$Year)
rawData$Plot <- rep(NA, nrow(rawData))

rawData <- rawData %>% relocate(c(SampleDescription, Plot), .before=Latitude)
rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(DepthElevation), .before=Day)
rawData$StudyID <- rep(NA, nrow(rawData))

rawData <- within(rawData, rm(habitat.type))
rawData <- as.data.frame(rawData)
dim(rawData)
str(rawData)


# Save: ------------------------------------------------------------------------
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/TwoBirdCommunities_in_Northeast_Buenos_Aires_Palacio&Montalti2013_AFE"
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


