##########################################################################################
# Curation Script: Data_Ferronato2021_Guinder2018
# AFE
# February 2023
##########################################################################################

# Libraries ==============================================================================
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


# Read Data ====================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Data_Ferronato2021_Guinder2018_AFE"
dt <- read.csv(paste0(files_dir, "/Data_Ferronato2021_Guinder2018.csv"), h=T) # data converted to csv for easier load
str(dt)


dt <- gather(dt, key="Species", value="Abundance", -c(1:4))


# Coordinates ==================================================================
range(dt$Latitude)
range(dt$Longitude)

c1 <- dt %>% group_by(Latitude, Longitude) %>% summarise(n=n_distinct(ID.station))
c2 <- dt %>% group_by(Latitude, Longitude) %>% distinct(ID.station) # O1 & A1
# same coords for 2 different stations
# locations on map checked below


# Sampling Event Date ==========================================================
sort(unique(dt$Cruise))

dt$Month <- str_split_fixed(dt$Cruise, "-", 2)[,1]
dt$Month <- as.integer(plyr::revalue(dt$Month, c("Apr"=4,
                                      "Mar"=3,
                                      "Oct"=10,
                                      "Sep"=9)))
sort(unique(dt$Month))
dt$Year <- str_split_fixed(dt$Cruise, "-", 2)[,2]
dt$Year <- as.integer(plyr::revalue(dt$Year, c("16"=2016,
                                                 "13"=2013,
                                                 "15"=2015)))
sort(unique(dt$Year))


# Sampling Event Station =======================================================
sort(unique(dt$ID.station))


# Abundance ====================================================================
sum(is.na(dt$Abundance)) # 0
sum(dt$Abundance==0)     # 3914

dt <- dt[!dt$Abundance==0,]
range(dt$Abundance)      # 99.1618 634635.5200


# Taxonomy =====================================================================
sort(unique(dt$Species))
dt$Species <- gsub("\\.", " ", dt$Species)
dt$Species <- gsub("sp ", "sp", dt$Species)
dt$Species <- gsub("spp ", "sp", dt$Species)
dt$Species <- gsub("cf  ", "cf ", dt$Species)
dt$Species[dt$Species=="cf Azadinium sp"] <- "Azadinium sp"
dt$Species[dt$Species=="cf Heterocapsa sp"] <- "Heterocapsa sp"

dt$Species[dt$Species=="Non identified  pyramimonads 10 m"] <- "Pyramimonadales sp"
dt$Species[dt$Species=="Non identified ciliates 20 m"] <- "Ciliophora sp1"
dt$Species[dt$Species=="Non identified ciliates 25 m"] <- "Ciliophora sp2"
dt$Species[dt$Species=="Non identified ciliates 30 m"] <- "Ciliophora sp3"
dt$Species[dt$Species=="Non identified ciliates 40 m"] <- "Ciliophora sp4"
dt$Species[dt$Species=="Non identified ciliates 90 m"] <- "Ciliophora sp5"
dt$Species[dt$Species=="Non identified cryptomonads 10 m"] <- "Cryptophyta sp"
dt$Species[dt$Species=="Non identified flagellates  10 m"] <- "Flagellata sp1"
dt$Species[dt$Species=="Non identified flagellates 10 20 m"] <- "Flagellata sp2"
dt$Species[dt$Species=="Non identified pennates 1"] <- "Pennates sp1"
dt$Species[dt$Species=="Non identified pennates 2"] <- "Pennates sp2"
dt$Species[dt$Species=="Pseudo nitzschia sp"] <- "Pseudonitzschia sp"
dt$Species[dt$Species=="Rounded ciliate 20 m"] <- "Ciliophora sp6"
dt$Species[dt$Species=="Strombidium sp 20 25 m"] <- "Strombidium sp1"
dt$Species[dt$Species=="Strombidium sp 50 m"] <- "Strombidium sp2"
dt$Species[dt$Species=="Thalassiosira sp 1"] <- "Thalassiosira sp1"
dt$Species[dt$Species=="Thalassiosira sp 2"] <- "Thalassiosira sp2"
dt$Species[dt$Species=="Amphidomataceans sp"] <- "Amphidomataceae sp"
sort(unique(dt$Species))

dt$Family <- rep(NA, nrow(dt))
dt$Genus <- str_split_fixed(dt$Species, " ", 2)[,1]
sort(unique(dt$Genus))
dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]


dt$Family <- ifelse(dt$Genus %in% c("Ciliophora", "Cryptophyta", "Flagellata", "Gymnodiniales",
                                   "Pennates", "Pyramimonadales", "Amphidomataceae"), dt$Genus, dt$Family)
dt$Genus <- ifelse(dt$Genus %in% c("Ciliophora", "Cryptophyta", "Flagellata", "Gymnodiniales",
                                  "Pennates", "Pyramimonadales", "Amphidomataceae"), NA, dt$Genus)


sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species)) # 0


# rawData ======================================================================
ndt <- dt %>% group_by(Family, Genus, Species, Latitude, Longitude, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup()


ndt$Biomass <- rep(NA, nrow(ndt))
ndt$SampleDescription <- paste0(dt$Latitude, "_", dt$Longitude, "_", dt$Month, "_", dt$Year)
ndt$Plot <- rep(NA, nrow(ndt))
ndt$DepthElevation <- rep(5, nrow(ndt))
ndt$Day <- rep(NA, nrow(ndt))
ndt$StudyID <- rep(NA, nrow(ndt))

ndt <- ndt %>% relocate(c(Abundance, Biomass), .before=Family)
ndt <- ndt %>% relocate(c(SampleDescription, Plot), .before=Latitude)
ndt <- ndt %>% relocate(Day, .before=Month)
ndt <- ndt %>% relocate(DepthElevation, .before=Day)


str(ndt)

rawData <- ndt


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Data_Ferronato2021_Guinder2018_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
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
  coord_sf(xlim = c(-80, -40), ylim = c(-50,-30)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################





