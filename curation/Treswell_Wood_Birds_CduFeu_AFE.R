################################################################################
# Study: Treswell Wood Bird Data
# Curator: AFE
# Date: June 2020 & January 2024
################################################################################


# Main sources =================================================================
# The Bernard Tucker Memorial Lecture Treswell Wood: 40 years of integrated population monitoring


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)
library(measurements)
library(data.table)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")


# Read Data: ===================================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Treswell_Wood_Birds_CduFeu_AFE"
dt <- read.csv(file = paste0(files_dir,"/TreswellWoodStandardSiteDataBiotime.csv"), h=T, sep=",")
sp <-  read.csv(file = paste0(files_dir,"/Species_eq.csv"), h=T, sep=";")


# Check structure: =============================================================
dim(dt) #30039 x 9
str(dt) 
class(dt)


# Taxonomy: ====================================================================
length(unique(dt$Species)) # 51 
identical(sort(unique(dt$Species)), sort(unique(sp$Species.Code))) # T
sort(unique(sp$Scientific.Name))

dt$ScientificNames <- sp$Scientific.Name[match(dt$Species, sp$Species.Code)]
dt$Genus <- str_split_fixed(dt$ScientificNames," ", 2)[,1]
sort(unique(dt$Genus))
dt$Species <- str_split_fixed(dt$ScientificNames," ", 2)[,2]
sort(unique(dt$Species))


# Abundance: =================================================================== 
# Each record represents one capture of a bird in a standard site net


# Coordinates: =================================================================
# Obtained from Google maps & overlapping grid image
# 53.307573 (N), -0.859682 (W)
dt$Latitude <- rep(53.307573, nrow(dt))
dt$Longitude <- rep(-0.859682, nrow(dt))

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


# Sampling Event Grain: ========================================================
length(unique(dt$Grid))           # 30
length(unique(dt$Standard.site))  # 7


c1 <- dt %>% group_by(Year, Month, Day) %>% summarise(nSite=n_distinct(Standard.site), nGrid=n_distinct(Grid)) # different sites visited on different days
c2 <- dt %>% group_by(Standard.site) %>% summarise(nYear=n_distinct(Year))                                     # all sites monitored very year
c3 <- dt %>% group_by(Standard.site, Year) %>% summarise(nMonth=n_distinct(Month), nDay=n_distinct(Day))       # mostly 5 months every year. Missing visits reported in metadata
c4 <- dt %>% group_by(Standard.site, Year) %>% summarise(nInt=n_distinct(Interval))                           


# Sampling Event Date ==========================================================
sort(unique(dt$Year))  # 1978-2020
sort(unique(dt$Month)) # 1-12
sort(unique(dt$Day))   # 1-31, also 33
sum(dt$Day==33)        # 27
dt$Month[dt$Day==33]   # 12
dt$Interval[dt$Day==33]# 5
dt$Year[dt$Day==33]    # 5

# NOTE: one visit (last to site 2 in 2005) could not be done by the year end because 
# of weather preventing mist netting.  Instead it was done on the first possible 
# opportunity on the following year -- > 2nd January 2006. 

c5 <- dt %>% group_by(Standard.site, Grid, Year, Interval) %>% summarise(n=n())
sum(c5$n==1)  # 828
c5B <- dt %>% group_by(Standard.site, Grid, Year, Interval) %>% summarise(n=n_distinct(ScientificNames))
sum(c5B$n==1) # 1049
c6 <- dt %>% group_by(Standard.site, Year, Interval) %>% summarise(n=n())
sum(c6$n==1)  # 3 
c6B <- dt %>% group_by(Standard.site, Year, Interval) %>% summarise(n=n_distinct(ScientificNames))
sum(c6B$n==1) # 8 (pool by site)


# Aggregate abundances =========================================================
rawData <- dt %>% group_by(Genus, Species, Latitude, Longitude, Day, Month, Year, Standard.site, Interval) %>%
  summarise(Abundance=n()) # number of observations (rows) in the specified grouping
range(rawData$Abundance)   # 1 to 40 inds


rawData$Biomass <- rep(NA, nrow(rawData))
rawData$Family <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(Abundance, Biomass, Family), .before=Genus)
rawData$SampleDescription<- paste0(rawData$Standard.site, "_", 
                                   rawData$Interval, "_",
                                   rawData$Year)
rawData$Plot<- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .after = Species)
rawData$DepthElevation <- rep(NA, nrow(rawData))
rawData <- rawData %>% relocate(c(DepthElevation), .after = Longitude)

sum(rawData$Day==33) # 10
rawData$Year[rawData$Day==33] <- 2006
rawData$Month[rawData$Day==33]<- 1
rawData$Day[rawData$Day==33]<-2       # Correct date of sampling, but keep Interval 5 of year 2005 in SampleDescription
sum(rawData$Abundance[rawData$Day==2 & rawData$Month==1 & rawData$Year==2006]) # 27, OK

rawData$StudyID <- rep(NA, nrow(rawData))
rawData <- within(rawData, rm(Standard.site, Interval))
rawData <- as.data.frame(rawData)


# Write csv ====================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Treswell_Wood_Birds_CduFeu_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)


# convex hulls =================================================================
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
area # 0 because it's only one pair of central coords

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
  coord_sf(ylim=c(40,60), xlim=c(-10,10)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################


