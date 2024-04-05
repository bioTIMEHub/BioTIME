################################################################################
# Curation Script: Invertebrates_Pawmpawm_river_DriftSamples_Hynes1975_AFE
# AFE
# 2020 & February 2023
################################################################################


# Main Source ==================================================================
# Downstream drift of invertebrates in a river in sourthern Ghana


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


# Read Data ====================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Invertebrates_Pawmpawm_river_DriftSamples_Hynes1975_AFE"
dt <- read.csv(paste0(files_dir, "/transcript_driftHynes1975b.csv"), h=T) # data entered manually by AFE from manuscript with authors permission
str(dt)


# Abundance ====================================================================
sum(is.na(dt$Counts))        # 96, representing the "totals" (i.e. total Trichoptera) which were not entered.
dt <- dt[!is.na(dt$Counts),] # 372 observations

sum(dt$Counts==0)            # 153
dt <- dt[!dt$Counts==0,]     # 219 observations


# Sample Event Date ============================================================
sort(unique(dt$Day))
sort(unique(dt$Month))
class(dt$Month)

dt$Month <- as.integer(plyr::revalue(dt$Month, c("April "="4", "April"="4",
                                                      "August"="8",
                                                      "December"="12",
                                                      "January"="1",
                                                      "March"="3",
                                                      "May"="5",
                                                      "November"="11",
                                                      "September"="9")))

sort(unique(dt$Month))
sort(unique(dt$Year))


# DepthElevation ===============================================================
dt$DepthElevation <- 290 # (in m) from Hynes 1975 (Annual cycles of macro-invertebrates of a river in southern Ghana)


# Coordinates ==================================================================
# Obtained from Hynes 1975 (Annual cycles of macro-invertebrates of a river in southern Ghana)
# 6°12'N, 0°I3'W

require(measurements)
conv_unit("6 12 0", "deg_min_sec", "dec_deg")
conv_unit("-0 13 0", "deg_min_sec", "dec_deg")

dt$Latitude <- rep(6.2, nrow(dt))
dt$Longitude <- rep(-0.2167, nrow(dt))

####Check location####
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
  ylim(-30,30)+
  xlim(-20,20)
points_zoom 


# Taxonomy =====================================================================
sort(unique(dt$Taxa))
dt$Taxa <- plyr::revalue(dt$Taxa, c("Ants"="Formicidae",
                                    "Austrocaenis sp."="Austrocaenis sp",
                                    "Centroptiloides spp."="Centroptiloides sp",
                                    "Centroptilum spp." ="Centroptilum sp",
                                    "Ceratopogonidae 1"="Ceratopogonidae sp",
                                    "Chironomidae 1 & p"="Chironomidae sp",
                                    "Cloeon sp."="Cloeon sp",
                                    "Culicidae 1"="Culicidae sp",
                                    "Cyclops sp."="Cyclops sp",
                                    "Dytiscidae 1 & a"="Dytiscidae sp",
                                    "Elminthidae 1"="Elminthidae sp1",
                                    "Elminthidae a"="Elminthidae sp2",
                                    "Protodipseudopsis 1 & p"="Protodipseudopsis sp",
                                    "Pyraustidae 1 & p"="Pyraustidae sp",
                                    "Simulium spp 1 & p"="Simulium sp",
                                    "Other Coleoptera"="Coleoptera",
                                    "Other Diptera 1 & p"="Diptera",
                                    "Other Ephemeroptera"="Ephemeroptera",
                                    "Other Hemiptera"="Hemiptera",
                                    "Other terrestrial insects"="Insecta",
                                    "Other Trichoptera"="Trichoptera"))
sort(unique(dt$Taxa)) 

#Create empty columns:
dt$Family <- rep(NA, nrow(dt))
dt$Genus <- str_split_fixed(dt$Taxa, " ", 2)[,1]
sort(unique(dt$Genus))
dt$Species <- str_split_fixed(dt$Taxa, " ", 2)[,2]
sort(unique(dt$Species))
dt$Species[dt$Species==""] <- NA

toFamily <- c("Anisoptera", "Ceratopogonidae", "Chironomidae", "Coleoptera", "Culicidae", "Diptera", 
              "Dytiscidae", "Elminthidae", "Ephemeroptera","Formicidae", "Gastropoda", "Hemiptera", "Hydracarina",
              "Insecta", "Naididae", "Ostracoda", "Pleidae", "Pyraustidae", "Testacea", "Trichoptera", "Veliidae", "Zygoptera")
dt$Family <- ifelse(dt$Genus %in% toFamily, dt$Genus, dt$Family)
dt$Genus <- ifelse(dt$Genus %in% toFamily, NA, dt$Genus)

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species)) # 0


# rawData ======================================================================
names(dt)
ndt <- dt %>% group_by(Family, Genus, Species, Latitude, Longitude, DepthElevation, Day, Month, Year) %>%
  summarise(Abundance=sum(Counts)) %>% ungroup()

ndt$Biomass <- rep(NA, nrow(ndt))
ndt$SampleDescription <- paste0(ndt$Day, "_", ndt$Month, "_", ndt$Year)
ndt$Plot <- rep(NA, nrow(ndt))
ndt$StudyID <- rep(NA, nrow(ndt))

ndt <- ndt %>% relocate(c(Abundance, Biomass), .before=Family)
ndt <- ndt %>% relocate(c(SampleDescription, Plot), .before=Latitude)

str(ndt)

rawData <- ndt


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Invertebrates_Pawmpawm_river_DriftSamples_Hynes1975_AFE"
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
  coord_sf(xlim = c(-4, 4), ylim = c(4,12)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################
