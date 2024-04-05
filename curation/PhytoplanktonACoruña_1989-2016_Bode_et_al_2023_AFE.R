################################################################################
# Study title: Monthly series of phytoplankton abundance collected between 1989 and 2016 at A Coruña (NW Spain).
# Curator: AFE
# Date: August 2023
################################################################################

# Sources ======================================================================
# https://doi.pangaea.de/10.1594/PANGAEA.908800


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(readxl)
library(sf)
library(clipr)
library(pangaear)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()

# Read raw data files ==========================================================
dt_file <- pg_data(doi="10.1594/PANGAEA.908800") # Download Data from Pangaea
dt <- dt_file[[1]][["data"]]


# Explore Data =================================================================
names(dt)
names(dt) <- gsub(" [#/l]", "", names(dt), fixed = TRUE)

dt <- gather(dt, key="Taxa", value="Abundance", -c(1:2))

# Temporal Data ================================================================
sort(unique(dt$`Date/Time`))
dt$Day <- as.integer(str_split_fixed(dt$`Date/Time`, "-", 3)[,3])
dt$Month <- as.integer(str_split_fixed(dt$`Date/Time`, "-", 3)[,2])
dt$Year<- as.integer(str_split_fixed(dt$`Date/Time`, "-", 3)[,1])

sort(unique(dt$Day))   # OK
sort(unique(dt$Month)) # OK
sort(unique(dt$Year))  # OK


# Locations ====================================================================
dt_file[[1]][["metadata"]][["coverage"]]
dt$Latitude <- 43.421660
dt$Longitude <- -8.436660

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
  ylim(20,50)+
  xlim(-20,20)
points_zoom   # OK


# DepthElevation ===============================================================
sort(unique(dt$`Depth water [m]`)) # OK
names(dt)[names(dt)=="Depth water [m]"] <- "DepthElevation"


# Taxonomy =====================================================================
sort(unique(dt$Taxa))

sps <- do.call(rbind, dt_file[[1]]$metadata$parameters)
sps <- data.frame(sps[-c(1:2),])
sps$Name1 <- str_split_fixed(sps$X1, " ", 3)[,c(1)]
sps$Name2 <- str_split_fixed(sps$X1, " ", 3)[,c(2)]
sps$Name3 <- str_split_fixed(sps$X1, " ", 3)[,c(3)]
sps$Name3 <- gsub("[#/l]", "", sps$Name3, fixed = TRUE)
sps$Name3 <- gsub("(", "", sps$Name3, fixed = TRUE)
sps$Name3 <- gsub(")", "", sps$Name3, fixed = TRUE)
sps$Name3 <- gsub(" ", "", sps$Name3, fixed = TRUE)

dt$Taxa <- gsub(" ", "", dt$Taxa, fixed = TRUE)

# Obtained from PANGAEA (10.1594/PANGAEA.908800):
identical(sort(unique(dt$Taxa)), sort(unique(sps$Name3)))  # TRUE

dt$Genus <- sps$Name1[match(dt$Taxa, sps$Name3)]
dt$Species <- sps$Name2[match(dt$Taxa, sps$Name3)]

sort(unique(dt$Genus))   # OK
sort(unique(dt$Species)) # OK


# Abundances ===================================================================
sum(is.na(dt$Abundance))          # 22266
dt <- dt[!is.na(dt$Abundance),]   # 90154

c1 <- dt %>% group_by(Day, Month, Year) %>% summarise(n=n_distinct(DepthElevation))        # From 1 to 7

sum(dt$Abundance==0)              # 74656
dt <- dt[!dt$Abundance==0,]       # 15498
range(dt$Abundance)               # 20-15390000


# SampleDescription & Aggregation===============================================
c2 <- dt %>% group_by(Day, Month, Year) %>% summarise(n=n_distinct(DepthElevation))        # From 1 to 7
c3 <- dt %>% group_by(Day, Month, Year, DepthElevation) %>% summarise(n=n_distinct(Taxa))  # From 1 to 29
sum(c3$n==1) # 52 samples out of 312 yielding only 1 sps

rawdata <- dt %>%
  group_by(Genus, Species, Latitude, Longitude, DepthElevation, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) %>%
  ungroup()

rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata$Family <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass, Family), .before=Genus)

rawdata$SampleDescription <- paste0(rawdata$DepthElevation, "_", rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before=Latitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))

str(rawdata)

rawdata1 <- subset(rawdata, rawdata$Year < 2009)
rawdata2 <- subset(rawdata, rawdata$Year > 2008)  # some species were identified individually after 2008 but not before

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/PhytoplanktonACoruña_1989-2016_Bode_et_al_2023_AFE"
write.csv(rawdata1, file=paste0(path, "/rawdata1.csv"), row.names = F)   # series 1989-2008
write.csv(rawdata2, file=paste0(path, "/rawdata2.csv"), row.names = F)   # series 2009-2016


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

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
  coord_sf(xlim = c(-20,20), ylim = c(20,50)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################
