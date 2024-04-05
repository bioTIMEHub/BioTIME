################################################################################
# Study title:  BioNet Biological Assessment Database - Macroinvertebrate Survey
# Curator: AFE
# Date: August 2023
################################################################################


# Main sources =================================================================
# https://programs.iowadnr.gov/bionet/
# https://publications.iowa.gov/20274/ (metadata & methods)


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
library(data.table)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/BioNet_Biological_Assessment_Database_BenthicInv_AFE"

sps <- read.csv(paste0(files_dir, "/BioNet-InvertebrateList.csv"), h=T)
dt <- read_excel(paste0(files_dir, "/Dump-BenthicInverts.xlsx"), sheet="Dump_BenthicInverts") # warnings ok


# Explore Data =================================================================
str(dt)
names(dt)


# Abundance Data ===============================================================
range(dt$SumOfIndiv)  # 0-828
sum(dt$SumOfIndiv==0) # 1
dt <- dt[!dt$SumOfIndiv==0,]


# Temporal Data ================================================================
dt$Day <- as.integer(str_split_fixed(dt$SampleDate, "-", 3)[,3])
dt$Month <- as.integer(str_split_fixed(dt$SampleDate, "-", 3)[,2])
dt$Year <- as.integer(str_split_fixed(dt$SampleDate, "-", 3)[,1])

sort(unique(dt$Day))
sort(unique(dt$Month))
sort(unique(dt$Year))   

dt <- dt %>% group_by(SiteID) %>% filter(., length(unique(Year)) > 1) %>% ungroup() # 102613 obs
c1 <- dt %>% group_by(SiteID) %>% summarise(nYear=n_distinct(Year))                 # From 2 to 18


# Locations ====================================================================
sum(is.na(dt$LatDD))  # 0
sum(is.na(dt$LongDD)) # 0


world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=LongDD, y=LatDD, alpha=0.01)) 
points       # OK

#Zoom:
points_zoom <- points +
  ylim(30,80)+
  xlim(-120, -70)
points_zoom # OK

c2 <- dt %>% group_by(SiteID) %>% summarise(nLat=n_distinct(LatDD),
                                            nLong=n_distinct(LongDD)) # each site has a unique pair


# Methods ======================================================================
table(dt$BugGear)
dt <- dt[!dt$BugGear=="Qualitative",]
c3 <- dt %>% group_by(SiteID, BugGear) %>% summarise(nYear=n_distinct(Year))                   # From 1 to 18
c4 <- dt %>% group_by(SiteID, Day, Month, Year) %>% summarise(nBug=n_distinct(BugGear))        # sometimes two gears x sampling event
c4$ConcatCheck <- paste0(c4$SiteID, "_", c4$Day, "_", c4$Month, "_", c4$Year)
tocheck <- unique(c4$ConcatCheck[c4$nBug==2])
c5 <- dt %>% group_by(SiteID, Day, Month, Year) %>% summarise(across(BugGear, ~paste(as.list(unique(.)), collapse="_")))
c5$ConcatCheck <- paste0(c5$SiteID, "_", c5$Day, "_", c5$Month, "_", c5$Year)
unique(c5$BugGear[c5$ConcatCheck %in% tocheck]) # mostly when both AS and Surber or Hess are used

# NOTE: to ensure maximum comparability of individual counts over time, and although 
# methods are tailored to the habitats present in a site at any moment in
# time, the dataset will be split by BugGear.

dt <- dt %>% group_by(SiteID, BugGear) %>% filter(., length(unique(Year)) > 1) %>% ungroup() # 33895 obs


# Taxonomy =====================================================================
sum(is.na(dt$FinalID))  # 0, OK
sum(is.na(dt$Species) & is.na(dt$Genus) & is.na(dt$Family)) # 2343

# Family -----------------------------------------------------------------------
sort(unique(dt$Family)) 
sort(unique(dt$FinalID[is.na(dt$Family)]))  # Higher, broader ranks
dt$Family <- ifelse(is.na(dt$Family), dt$FinalID, dt$Family)
sum(is.na(dt$Family))    # 0

sum(dt$Family=="")

# Genus ------------------------------------------------------------------------
sort(unique(dt$Genus))   
sort(unique(dt$FinalID[dt$Genus %like% "/"]))
unique(dt$Species[dt$Genus %like% "/"])        # NA
sum(dt$Genus %like% "/")                       # 91
sort(unique(dt$Family[dt$Genus %like% "/"]))

# Keep Family, since these indicate uncertain Genus IDs
dt$Genus <- ifelse(dt$Genus %like% "/", NA, dt$Genus)
dt$Genus[dt$Genus=="Agabini-Complex"] <- "Agabini"

sum(dt$Genus=="", na.rm=T)

# Species ----------------------------------------------------------------------
sort(unique(dt$Species))
sort(unique(dt$FinalID[dt$Species %like% "/"]))
sort(unique(dt$TSN[dt$Species %like% "/"]))

# Keep Genus and add sp, since these indicate uncertain Species IDs:
dt$Species <- ifelse(dt$Species %like% "/", "sp", dt$Species) # This will merge "Plauditus punctiventris/virilis" and "Plauditus dubius/virilis"
sum(dt$Species=="", na.rm=T)

sort(unique(dt$FinalID[dt$TSN=="509/542"]))                   # "Maccaffertium pulchellum/terminatum"


sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species))   # 0


# SampleDesc & Aggregation =====================================================
str(dt)

rawdata <- dt %>% group_by(Family, Genus, Species, SiteID, BugGear, LatDD, LongDD, Day, Month, Year) %>%
  summarise(Abundance=sum(SumOfIndiv)) %>% ungroup()  # 33882 obs
names(rawdata)[names(rawdata)=="LatDD"] <- "Latitude"
names(rawdata)[names(rawdata)=="LongDD"] <- "Longitude"

rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$SampleDescription <- paste0(rawdata$SiteID, "_", rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata$DepthElevation <- rep(NA, nrow(rawdata))

rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .after=Species)
rawdata <- rawdata %>% relocate(c(DepthElevation), .after=Longitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(StudyID), .after=Year)

rawdata <- as.data.frame(rawdata)
str(rawdata)
range(rawdata$Abundance)  # 1 828


# Split ========================================================================
unique(rawdata$BugGear)
lr <- split(rawdata, f=rawdata$BugGear)
lr <- lapply(lr, function(x){within(x, rm(SiteID, BugGear))})
names(lr)


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/BioNet_Biological_Assessment_Database_BenthicInv_AFE/RawData"
for(i in names(lr)){
  write.csv(lr[[i]], paste0(path, "/", i,".csv"), row.names=F)
}


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
coordslr <- lapply(lr, function(x){dt_coord <- x %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()})


# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid
centroidlr <- lapply(coordslr, function(x){x %>% st_convex_hull() %>% st_centroid() %>% unlist}) 


# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
arealr <- lapply(coordslr, function(x){st_transform(x, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
    st_convex_hull() %>% st_area()}) 


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
  coord_sf(xlim = c(-120,-70), ylim = c(30,80)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################


