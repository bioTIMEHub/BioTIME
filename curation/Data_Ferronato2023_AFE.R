##########################################################################################
# Curation Script: Data_Ferronato2023_AFE
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
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Data_Ferronato2023_AFE"
dt <- read.csv(paste0(files_dir, "/Data_Ferronato2023.csv"), h=T, check.names = FALSE) 
# NOTE: data converted to csv for easier load. Names contain multibyte string 41 characters, hence check.names=F

dt <- gather(dt, key="Species", value="Abundance", -c(1:4))
str(dt)
names(dt)
names(dt) <- c("Cruise", "ID_station", "Latitude", "Longitude", "Species", "Abundance")


# Coordinates ==================================================================
range(dt$Latitude)
range(dt$Longitude)

c1 <- dt %>% group_by(Latitude, Longitude) %>% summarise(n=n_distinct(ID_station))


# Sampling Event Date ==========================================================
sort(unique(dt$Cruise))

dt$Month <- str_split_fixed(dt$Cruise, "-", 2)[,1]
dt$Month <- as.integer(plyr::revalue(dt$Month, c("Dec"=12,
                                                 "Oct"=10)))
sort(unique(dt$Month))
dt$Year <- str_split_fixed(dt$Cruise, "-", 2)[,2]
dt$Year <- as.integer(plyr::revalue(dt$Year, c("16"=2016,
                                               "17"=2017,
                                               "19"=2019)))
sort(unique(dt$Year))


# Sampling Event Station =======================================================
sort(unique(dt$ID_station))


# Abundance ====================================================================
sum(is.na(dt$Abundance)) # 0
sum(dt$Abundance==0)     # 1517

dt <- dt[!dt$Abundance==0,]
range(dt$Abundance)      # 99.1618 1101191.7890


# Taxonomy =====================================================================
sort(unique(dt$Species))

dt$Species[dt$Species=="Non-identified flagellates >10 \xb5m"] <- "Flagellata sp1"
dt$Species[dt$Species=="Non-identified flagellates <10 \xb5m"] <- "Flagellata sp2"
dt$Species[dt$Species=="Strombidium 30-40\xb5m"] <- "Strombidium sp1"
dt$Species[dt$Species=="Ciliates 20-30\xb5m"] <- "Ciliophora sp1"
dt$Species[dt$Species=="gimnodiniales 30-50\xb5m"] <- "Gymnodiniales sp1"
dt$Species[dt$Species=="gimnodiniales 30 \xb5m"] <- "Gymnodiniales sp2"
dt$Species[dt$Species=="gimnodiniales 15-20 \xb5m"] <- "Gymnodiniales sp3"
dt$Species[dt$Species=="Strombidium aff. conicum 55-70 \xb5m"] <- "Strombidium aff. conicum"
dt$Species[dt$Species=="Ciliate 20\xb5m"] <- "Ciliophora sp2"
dt$Species[dt$Species=="Tecate dinoflagellates 20-25\xb5m"] <- "Dinoflagellata sp1"
dt$Species[dt$Species=="gimnodiniales 40-70\xb5m"] <- "Gymnodiniales sp4"
dt$Species[dt$Species=="cf. Alexandrium sp."] <- "Alexandrium sp"
dt$Species[dt$Species=="cf. Azadinium sp."] <- "Azadinium sp"
dt$Species[dt$Species=="Dinophysis cf. Acuminata"] <- "Dinophysis cf acuminata"
dt$Species[dt$Species=="Dinophysis cf. Rotundata"] <- "Dinophysis cf rotundata"
dt$Species[dt$Species=="Non-identified silicoflagellates"] <- "Dictyochales sp"
dt$Species[dt$Species=="Pseudo-nitzschia sp. 1"] <- "Pseudonitzschia sp1"
dt$Species[dt$Species=="Pseudo-nitzschia sp.2"] <- "Pseudonitzschia sp2"
dt$Species[dt$Species=="gimnodiniales 20-25\xb5m"] <- "Gymnodiniales sp5"

dt$Species <- gsub("\\.", " ", dt$Species)
dt$Species <- gsub("sp ", "sp", dt$Species)
dt$Species <- gsub("spp ", "sp", dt$Species)
dt$Species <- gsub("aff  ", "aff ", dt$Species)

dt$Family <- rep(NA, nrow(dt))
dt$Genus <- str_split_fixed(dt$Species, " ", 2)[,1]
sort(unique(dt$Genus))
dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]


dt$Family <- ifelse(dt$Genus %in% c("Ciliophora", "Flagellata", "Gymnodiniales",
                                    "Amphidomataceae", "Dictyochales", "Dinoflagellata"), dt$Genus, dt$Family)
dt$Genus <- ifelse(dt$Genus %in% c("Ciliophora", "Flagellata", "Gymnodiniales",
                                   "Amphidomataceae", "Dictyochales", "Dinoflagellata"), NA, dt$Genus)


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
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Data_Ferronato2023_AFE"
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


