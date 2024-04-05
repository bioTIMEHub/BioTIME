##########################################################################################
# Curation Script: CiliatesBahíaBlancaEstuary1986_2006_Guinder_et_al_2022_AFE  (1986 -2006)
# AFE
# August 2023
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


# Main source: ---------------------------------------------------------------------------
# https://doi.org/10.1016/j.marenvres.2018.12.001 

rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ====================================================================
# Period: 1986 -2006
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/CiliatesBahíaBlancaEstuary1986_2006_Guinder_et_al_2022_AFE"
dt <- read_excel(paste0(files_dir, "/ciliates.xlsx"), sheet="tintinnids")


# Explore Data ===========================================================================
head(dt)
names(dt)

dt <- gather(dt, key="Taxa", value="Abundance", -c(1:3))

# Temporal ===============================================================================
sort(unique(dt$date))
sort(unique(dt$month))
sort(unique(dt$year))
dt$Day <- str_split_fixed(str_split_fixed(dt$date, " ", 2)[,1], "-", 3)[,3]
sort(unique(dt$Day))

sum(is.na(dt$year))   # 0
sum(is.na(dt$month))  # 0
sum(is.na(dt$Day))    # 0

dtc1 <- dt %>% group_by(year) %>% summarise(n=n_distinct(month))            # Variable over the years
dtc2 <- dt %>% group_by(year, month) %>% summarise(n=n_distinct(Day))       # Variable over the years

# Location ===============================================================================
dt$Latitude <- -38.751575
dt$Longitude <- -62.379864

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points           # OK

#Zoom:
points_zoom <- points +
  ylim(-70,-30)+
  xlim(-100,-40)
points_zoom      # OK


# Taxonomy ===============================================================================
sort(unique(dt$Taxa))

# Standardise to BioTIME:
dt$Taxa <- gsub("T. ", "Tintinnopsis ", dt$Taxa)
dt$Taxa <- gsub(" var. mortensenii", "", dt$Taxa)

dt$Taxa[dt$Taxa=="Tintinnopsis aff. semiciliatum"] <- "Tintinnidium aff. semiciliatum"
dt$Taxa[dt$Taxa=="Tintinnopsis beroidea"] <- "Tintinnidium beroidea"  # All other T. are Tintinnopsis

dt$Taxa <- gsub("sp. 1", "sp1", dt$Taxa)  
dt$Taxa <- gsub("sp. 2", "sp2", dt$Taxa)

dt$Species <- str_split_fixed(dt$Taxa, " ", 2)[,2]
dt$Genus <- str_split_fixed(dt$Taxa, " ", 2)[,1]
dt$Family <- rep(NA, nrow(dt))

sort(unique(dt$Genus))    # Ok
sort(unique(dt$Species))  # OK


# Abundance ==============================================================================
range(dt$Abundance)  # 0-7800

sum(dt$Abundance==0) # 1448
dt <- dt[!dt$Abundance==0,] # Remove 0s

dtc3 <- dt %>% group_by(year, month, Day) %>% summarise(n=n_distinct(Taxa)) # Only a few with 1 taxon only (Day-Month-Year is sampling event)


# SampleDescription & Aggregation ========================================================
names(dt) <- str_to_title(names(dt))

rawdata <- dt %>% group_by(Family, Genus, Species, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() # 752 final observations

range(rawdata$Abundance) # 6.8 7800.0

rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$Day <- as.integer(rawdata$Day)

rawdata$SampleDescription <- paste0(rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before=Latitude)

rawdata$DepthElevation <- rep(-0.5, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(DepthElevation), .before=Day)

rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(StudyID), .after=Year)

str(rawdata)

#View(rawdata)

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/CiliatesBahíaBlancaEstuary1986_2006_Guinder_et_al_2022_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls ==========================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
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


# End of script #########################################################################


