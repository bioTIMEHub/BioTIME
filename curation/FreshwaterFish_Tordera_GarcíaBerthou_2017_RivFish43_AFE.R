#######################################################################
# Curation Script: Long-term fish monitoring of the Tordera stream
# Curator: AFE
# Date: July 2022
#######################################################################


# Libraries ===========================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(data.table)
library(sf)
library(clipr)
library(rgdal)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Main sources =========================================================
# doi.org/10.1111/j.1365-2427.2009.02299.x
# doi.org/10.1371/journal.pone.0175932
# https://doi.org/10.1111/eff.12363


# Read data: ===========================================================
mypath <- getwd() 
rawdatapath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/FreshwaterFish_Tordera_GarcíaBerthou_2017_RivFish43_AFE"

coords <- read.csv(file = paste0(rawdatapath,"/coordinates.csv"), h=T)
dt <-  read.csv2(file = paste0(rawdatapath,"/Copy of Tordera_fish_upto2017.csv"), h=T)


# View data: ===========================================================
dim(dt)
sort(unique(dt$year))  # 2001 to 2017
sort(unique(dt$site))  # 10 sites
sort(unique(dt$site2)) # 10 sites

c1 <- dt %>% group_by(site) %>% summarise(n=n_distinct(year))       # 14 to 17 years
c2 <- dt %>% group_by(site, year) %>% summarise(n=n_distinct(date)) # 1 to 4


# Abundance & biomass ==================================================
# 1 row = 1 individual
sum(is.na(dt$species)) # 0
sum(dt$species=="")


dt$Abundance <- 1


sum(is.na(dt$total_weight_g))  # 0
sum(dt$total_weight_g=="")     # 24198
sum(dt$total_weight_g==" ")    # 0
sum(dt$total_weight_g==0)      # 0
dt$total_weight_g[dt$total_weight_g==""] <- NA

sum(dt$fork_length_mm=="", na.rm=T)     # 0
sum(dt$fork_length_mm==0, na.rm=T)      # 0
sum(is.na(dt$fork_length_mm))           # 2711
2711/38407*100

range(dt$fork_length_mm, na.rm=T) # 7 to 654, OK

sum(is.na(dt$fork_length_mm) & is.na(dt$total_weight_g)) # 2708

sort(unique(dt$year[is.na(dt$fork_length_mm)]))
sort(unique(dt$site[is.na(dt$fork_length_mm)]))


# Temporal data ========================================================

dt$Day <- as.numeric(str_split_fixed(dt$date, "/", 3)[,1])
dt$Month <- as.numeric(str_split_fixed(dt$date, "/", 3)[,2])

sort(unique(dt$Day))   # OK
sort(unique(dt$Month)) # OK


# Locations ============================================================
dtcoords <- data.frame(lon=coords$UTM_X, lat=coords$UTM_Y, site=coords$SiteID)
dtcoordsT <- dtcoords[,c(1:2)]
coordinates(dtcoordsT) <- c("lon", "lat")
proj4string(dtcoordsT) <- CRS("+init=epsg:32631") # UTM 31
CRS.new <- CRS("+init=epsg:4326") # WGS 84
df1 <-spTransform(dtcoordsT, CRS.new)
df1 <- cbind(as.data.frame(df1), dtcoords$site)
sum(is.na(df1$lon))  # 0
sum(is.na(df1$lat))  # 0

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=df1, 
             aes(x=lon, y=lat, alpha=0.01)) 
points       

#Zoom:
points_zoom <- points +
  ylim(35,45)+
  xlim(-5,10)
points_zoom 

dt$Latitude <- df1$lat[match(dt$site2, df1$`dtcoords$site`)]
sum(is.na(dt$Latitude))

dt$Longitude <- df1$lon[match(dt$site2, df1$`dtcoords$site`)]
sum(is.na(dt$Longitude))


# Taxonomy =============================================================
sort(unique(dt$species))
dt$species <- gsub("\\.", "", dt$species) # sp. to sp

dt <- dt[!dt$species=="NO CAPTURES",]
dt <- dt[!dt$species=="SITE DRY",]

dt$Genus <- str_split_fixed(dt$species, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$species, " ", 2)[,2]

sort(unique(dt$Genus))
sort(unique(dt$Species))

dt$Family <- ifelse(dt$Genus=="Anguilla", "Anguillidae", NA)
dt$Family <- ifelse(dt$Genus %in% c("Barbus", "Cyprinus", "Rutilus", "Squalius"), "Cyprinidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Chelon", "Mugilidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Gambusia", "Poeciliidae", dt$Family)
dt$Family <- ifelse(dt$Genus %in% c("Lepomis", "Micropterus") , "Centrarchidae", dt$Family)
dt$Family <- ifelse(dt$Genus %in% c("Oncorhynchus", "Salmo"), "Salmonidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Perca", "Percidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Phoxinus", "Leuciscidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Platichthys", "Pleuronectidae", dt$Family)
dt$Family <- ifelse(dt$Genus=="Salaria", "Blenniidae", dt$Family)

sort(unique(dt$Family))
sum(is.na(dt$Family))

# Taxonomic updates ====================================================
dt[dt$Genus=="Salaria",]
dt$Genus[dt$Genus=="Salaria"] <- "Salariposis"

unique(dt$Genus[dt$Species=="bigerri"])
dt$Species[dt$Species=="bigerri"] <- "septimaniae"

# SampleDescription & abundance aggregate ==============================
names(dt)[names(dt)=="year"] <- "Year"
names(dt)[names(dt)=="fork_length_mm"] <- "Biomass"

c3 <- dt %>% group_by(site) %>% summarise(nLat=n_distinct(Latitude), nLong=n_distinct(Longitude))
length(unique(dt$Latitude))
length(unique(dt$Longitude))

rawdata <- dt
rawdata$SampleDescription <- paste0(rawdata$site, "_",
                                    rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata$DepthElevation <- rep(NA, nrow(rawdata))
rawdata$StudyID <- rep(NA, nrow(rawdata))

rawdata <- rawdata %>%
  select(., Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID)

#View(rawdata)

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/FreshwaterFish_Tordera_GarcíaBerthou_2017_RivFish43_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls ====================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
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
  coord_sf(xlim = c(-5,10), ylim = c(35,45)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ###################################################