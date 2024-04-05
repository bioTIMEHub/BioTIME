##################################################################
# Study title: Vegetation data of tree community from Tropical forests of Western Ghats
# Curator: AFE
# Date: 27/06/2023
##################################################################

# NOTE: this dataset was already sent in the BT template format

# Libraries ======================================================
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

rm(list=ls())

setwd("C:/Users/Usuario/Documents/PHD/BioTIMEGithub")
myd <- getwd()

# Read raw data files =============================================
path <- "C:/Users/Usuario/Documents/PHD/BIOTIME/BioTIMENewStudiesCuration/Vegetation_WhesternGhats_DrSreejith_AFE"
dt <- read_excel(paste0(path, "/BioTIMEContributorTemplate.xlsx"), sheet = "rawData")

# General checks ==================================================
str(dt)
names(dt) <- plyr::revalue(names(dt), c("Biomass (m2/ha)"="Biomass",
                                        "DepthElevation(m)"="DepthElevation"))

dtcheck1 <- dt %>% group_by(Plot) %>% summarise(nyear = n_distinct(Year))       # All 3 but Nadukani
unique(dt$Year[dt$Plot=="Nadukani"])
dtcheck2 <- dt %>% group_by(Plot, Year) %>% summarise(nmonth = n_distinct(Month),
                                                      nday = n_distinct(Day))   # Always measured in the same day

# Abundance & biomasses ===========================================
range(dt$Abundance)          # NAs
range(dt$Biomass)            # OK, no NAs

sum(is.na(dt$Abundance))     # 1
#View(dt[is.na(dt$Abundance),])        # The record has biomass data
dt$Abundance[is.na(dt$Abundance)] <- 2 # Confirmed by authors in email correspondence

range(dt$Abundance)          # 1 to 172


# Temporal data ===================================================
sort(unique(dt$Year))  # OK
sort(unique(dt$Month)) # OK, convert to Nº
sort(unique(dt$Day))   # OK

dt$Month <- plyr::revalue(dt$Month, c("April"=4,
                                      "August"=8,
                                      "December"=12,
                                      "February"=2,
                                      "January"=1,
                                      "July"=7,
                                      "June"=6,
                                      "March"=3,
                                      "May"=5,
                                      "November"=11,
                                      "October"=10,
                                      "September"=9)) # Standardize to BT format

# Location data ===================================================
sort(unique(dt$Plot))            # 10, OK
sort(unique(dt$DepthElevation))  # 11 codes
range(dt$Latitude)
range(dt$Longitude)

dtcheck3 <- dt %>% group_by(Plot) %>% summarise(nlat=n_distinct(Latitude), nlong=n_distinct(Longitude)) # OK
dtcheck4 <- dt %>% group_by(Plot) %>% summarise(nele=n_distinct(DepthElevation))                        # OK, two in Thamarasserry 4th mile
table(dt$DepthElevation[dt$Plot=="Thamarasserry 4th mile"]) # 373m might be a typo

sum(dt$DepthElevation=="373m")
dt$DepthElevation[dt$DepthElevation=="373m"] <- "375m"      # Likely a typo
dt$DepthElevation <- as.integer(gsub("m", "", dt$DepthElevation))
sort(unique(dt$DepthElevation))

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
  ylim(0,20)+
  xlim(70,90)
points_zoom   # OK

# Taxonomy ========================================================

sort(unique(dt$Family))
dt$Family[dt$Family=="Combertaceae"] <- "Combretaceae"     # Typo
sort(unique(dt$Genus))
sort(unique(dt$Species))

#View(dt[dt$Species=="sp.",])
dt$Species <- plyr::revalue(dt$Species, c("sp."="sp",
                                          "spp."="sp",
                                          "sp.1"="sp1",
                                          "sp.2"="sp2",
                                          "sp.3"="sp3",
                                          "sp.4"="sp4",
                                          "sp.5"="sp5",                        # Standardization to BT
                                          "racemosa var. racemosa"="racemosa", # lowest tax is species
                                          "Pendulus"="pendulus",               # Typo
                                          "benghalensis"="bengalensis"))       # Typo     
dt$Species <- plyr::revalue(dt$Species, c("airy-shawii"="retusa"))             # Synonym


# Sample_desc & rawdata agg =======================================

rawdata <- dt %>% 
  group_by(Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup()

# 1405 obs
# Counts & aboveground biomass aggregated by sampling event & taxa

rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$SampleDescription <- paste0(rawdata$Plot, "_", rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .after=Species)
rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(StudyID, .after=Year)


range(rawdata$Abundance)  # 1 to 172
range(rawdata$Biomass)    # 1.614335e-03 1.728449e+02

str(rawdata)
rawdata$Month <- as.integer(rawdata$Month)


path <- "C:/Users/Usuario/Documents/PHD/BIOTIMEGithubMetadatas/Vegetation_WhesternGhats_DrSreejith_AFE"
write.csv2(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)

# Convex hulls ====================================================
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
  coord_sf(xlim = c(70,90), ylim = c(0,20)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ###################################################



