################################################################################
#Curation script
#Title: Whitelands Wood Birds Survey
#Curator: Fritha West and AFE
#Date: 2021 & January 2024
################################################################################

# Main sources -----------------------------------------------------------------
# The Whitelands Project CIC., 2020. The Whitelands Project Management Plan. Unpublished report. 


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


# Read data:--------------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Whitelands_Wood_Bird_Survey_Westetal2020_AFE"
dt <- read_excel(paste0(files_dir, "/Whitelands-Biotime-07122020_fixed.xlsx"), sheet="Whitelands-Biotime-07122020")

dim(dt)      # 2821 x 14
head(dt, 3L) # OK
str(dt)


# Abundance --------------------------------------------------------------------
sum(is.na(dt$Abundance))
sum(dt$Abundance=="NA")  # 195
dt <- dt[!dt$Abundance=="NA",] # 2626

dt$Abundance <- as.integer(dt$Abundance)
range(dt$Abundance)

dt <- dt[!dt$Abundance==0,]    # 1072

unique(dt$Biomass)


# Temporal sampling events -----------------------------------------------------
unique(dt$Day)   # NA
unique(dt$Month) #"Apr"  "Aug"  "Jul"  "Jun"  "Mar"  "May"  "Sept"
dt$Month <- as.integer(plyr::revalue(dt$Month, c("Apr"=4,"Aug"=8,"Jul"=7,
                                      "Jun"=6,"Mar"=3,"May"=5,
                                      "Sept"=9)))


sort(unique(dt$Date)) #2006-2018
names(dt)[names(dt) =="Date"] <- "Year" 

c1 <- dt %>%
  group_by(Year) %>%
  summarise(DistinctMonth=n_distinct(Month))

unique(dt$Month[dt$Year=="2006"]) # July to September
unique(dt$Month[dt$Year=="2018"]) # May to September


# Coordinates: -----------------------------------------------------------------
unique(dt$Latitude)
unique(dt$Longitude) 

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

points_zoom <- points +
  ylim(40,60)+
  xlim(-10,10)
points_zoom    


# Taxonomy: --------------------------------------------------------------------
unique(dt$Family)
unique(dt$Genus)
sort(unique(dt$Species))

# Carduelis cardeulis is Carduelis carduelis
dt$Species[dt$Species=="Carduelis cardeulis"] <- "Carduelis carduelis"


dt$Genus <- str_split_fixed(dt$Species, " ", 2)[,1]
dt$Species <- str_split_fixed(dt$Species, " ", 2)[,2]
sort(unique(dt$Genus)) 
sort(unique(dt$Species)) 


# Rawdata: ---------------------------------------------------------------------
dt$dups <- paste0(dt$Genus, dt$Species, dt$Month, dt$Year)
sum(duplicated(dt$dups)) # 0

rawData <- dt
rawData <- within(rawData, rm(dups, SampleDesc))
rawData$SampleDescription <- paste0(rawData$Month, "_", rawData$Year)
rawData <- rawData %>% relocate(SampleDescription, .before=Latitude)
str(rawData)
getwd()


# Save: ------------------------------------------------------------------------
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Whitelands_Wood_Bird_Survey_Westetal2020_AFE"
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
  coord_sf(xlim = c(-2,2), ylim = c(50,55)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################

