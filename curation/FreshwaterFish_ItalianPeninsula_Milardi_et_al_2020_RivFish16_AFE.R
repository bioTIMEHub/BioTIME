################################################################################
# Study title: 
# Curator: AFE
# Date: 24/07/23
################################################################################

# Main sources =================================================================
# https://doi.org/10.1016/j.scitotenv.2019.134364

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
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/FreshwaterFish_ItalianPeninsula_Milardi_et_al_2020_RivFish16_AFE"

sites <- read_excel(paste0(files_dir, "/Italian sites_AG_MM - Copy.xlsx"), sheet = "Sites")          # Excel sheet created from main rawdata sheet for an easier read into R
dtR <- read_excel(paste0(files_dir, "/Italian sites_AG_MM - Copy.xlsx"), sheet = "AbundanceRecent")  # Excel sheet created from main rawdata sheet for an easier read into R
dtP <- read_excel(paste0(files_dir, "/Italian sites_AG_MM - Copy.xlsx"), sheet = "AbundancePast")    # Excel sheet created from main rawdata sheet for an easier read into R


# Abundances ===================================================================
head(dtR, 3L)
identical(names(dtR), names(dtP)) # TRUE

dtR <- gather(dtR, key="Species", value="Abundance", -c(1:2))
dtP <- gather(dtP, key="Species", value="Abundance", -c(1:2))

range(dtR$Abundance) # 0 to 5, no NAs
range(dtP$Abundance) # 0 to 5, no NAs

dtR <- dtR[!dtR$Abundance==0,]  # 335
dtP <- dtP[!dtP$Abundance==0,]  # 481


# Temporal data ================================================================
sort(unique(dtR$`Sampling date`))
sort(unique(dtR$`Site#`[dtR$`Sampling date`=="1905-06-29 UTC"])) # 608 (year 2007, problem when reading data bc no day or month)
sort(unique(dtP$`Sampling date`))
sort(unique(dtP$`Site#`[dtP$`Sampling date`=="1905-06-19 UTC"])) # 608 (year 1997, problem when reading data bc no day or month)
sort(unique(dtP$`Site#`[dtP$`Sampling date`=="1905-06-21 UTC"])) # 2164 (year 1999, problem when reading data bc no day or month, site 2163 would have had the same problem but removed previously bc total Abundance == 0)

dt <- rbind(dtR, dtP)
dt$Day <- as.integer(str_split_fixed(dt$`Sampling date`, "-", 3)[,3])
dt$Month <- as.integer(str_split_fixed(dt$`Sampling date`, "-", 3)[,2])
dt$Year <- as.integer(str_split_fixed(dt$`Sampling date`, "-", 3)[,1])

unique(dt$Day)
unique(dt$Month)
unique(dt$Year)

dt$Year <- ifelse(dt$`Site#`==608 & dt$`Sampling date`=="1905-06-29 UTC", 2007, dt$Year)
dt$Month <- ifelse(dt$`Site#`==608 & dt$`Sampling date`=="1905-06-29 UTC", NA, dt$Month)
dt$Day <- ifelse(dt$`Site#`==608 & dt$`Sampling date`=="1905-06-29 UTC", NA, dt$Day)

dt$Year <- ifelse(dt$`Site#`==608 & dt$`Sampling date`=="1905-06-19 UTC", 1997, dt$Year)
dt$Month <- ifelse(dt$`Site#`==608 & dt$`Sampling date`=="1905-06-19 UTC", NA, dt$Month)
dt$Day <- ifelse(dt$`Site#`==608 & dt$`Sampling date`=="1905-06-19 UTC", NA, dt$Day)

dt$Year <- ifelse(dt$`Site#`==2164 & dt$`Sampling date`=="1905-06-21 UTC", 1999, dt$Year)
dt$Month <- ifelse(dt$`Site#`==2164 & dt$`Sampling date`=="1905-06-21 UTC", NA, dt$Month)
dt$Day <- ifelse(dt$`Site#`==2164 & dt$`Sampling date`=="1905-06-21 UTC", NA, dt$Day)

names(dt)[names(dt)=="Site#"] <- "Site"
dt <- dt %>% group_by(Site) %>% filter(., length(unique(Year)) > 1) %>% ungroup() # rm recent sampling of site 2163 because the sampling yielded total abundance 0 in the past event
c1 <- dt %>% group_by(Site) %>% summarise(n=n_distinct(Year))                     # Always 2


# Locations ====================================================================
names(sites)[names(sites)=="Latitud_Y_N_WGS 84"] <- "Latitude"
names(sites)[names(sites)=="Long_X_E_WGS 84"] <- "Longitude"

dt$Latitude <- sites$Latitude[match(dt$Site, sites$`Site#`)]
sum(is.na(dt$Latitude))   # 0
dt$Longitude <- sites$Longitude[match(dt$Site, sites$`Site#`)]
sum(is.na(dt$Longitude))  # 0

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
  ylim(25, 55)+
  xlim(0,25)
points_zoom 


# Taxonomy =====================================================================
sort(unique(dt$Species)) 

setdiff(sort(unique(dt$Species[dt$Year < 2003])), sort(unique(dt$Species[dt$Year > 2003]))) # OK, as indicated in data file
setdiff(sort(unique(dt$Species[dt$Year > 2003])), sort(unique(dt$Species[dt$Year < 2003]))) # OK, as indicated in data file

dt$Genus <- str_split_fixed(dt$Species, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]

sort(unique(dt$Genus))
sort(unique(dt$Species)) 


# Elevation ====================================================================

range(sites$Altitude)                    # 0 to 800
unique(sites$`Site#`[sites$Altitude==0]) # 2299 (close to the coast)

dt$DepthElevation <- sites$Altitude[match(dt$Site, sites$`Site#`)]
sum(is.na(dt$DepthElevation))


# Rawdata ======================================================================
# Data is already aggregated (Abundance classifications)
c2 <- dt %>% group_by(Site) %>% summarise(nLat=n_distinct(Latitude), nLong=n_distinct(Longitude)) # Always 1
length(unique(dt$Site))     # 78
length(unique(dt$Latitude)) # 78
dt$Biomass <- rep(NA, nrow(dt))
dt$Family <- rep(NA, nrow(dt))
dt$SampleDescription <- paste0(dt$Site, "_", dt$Day, "_", dt$Month, "_", dt$Year)
dt$Plot <- rep(NA, nrow(dt))
dt$StudyID <- rep(NA, nrow(dt))

dt <- dt %>%
  select(., Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID)

rawdata <- dt

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/FreshwaterFish_ItalianPeninsula_Milardi_et_al_2020_RivFish16_AFE"
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
  coord_sf(xlim = c(0,25), ylim = c(25,55)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()


# End of script ################################################################
