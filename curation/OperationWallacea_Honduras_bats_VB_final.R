## Curation script for OpWal: Honduras bats
## curator VB

setwd("D:/Dropbox/towards BioTIME v2/Vivi-dataset hunting files/OpWal/BioTIMEv02/BioTIMEv02")
ds_name <- "OperationWallacea_Honduras_bats"

library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)
library(dplyr)
library(readxl)

ds <- read_excel("Honduras bats/BioTIMEContributorTemplate_HonBatsOPWALL_VB_v02.xlsx", sheet = "rawData")


# transform to long format
str(ds)

#### check locations
unique(ds$Plot)
table(ds$Plot,ds$Year)
unique(ds$Latitude)
unique(ds$Longitude)

## map

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=as.data.frame(cbind(long = ds$Longitude, lat = ds$Latitude)), 
             aes(x=long, y=lat, alpha=0.01)) 
points # now correct

sort(unique(ds$Abundance)) # inspect values
is.numeric(ds$Abundance)

dim(ds)  #  2532   14

##### check taxa ####
sort(unique(ds$Species)) # only epiteth
sort(unique(ds$Genus))
sum(is.na(ds$Genus)) # no NAs
sum(ds$Genus=="") # no blanks
sum(is.null(ds$Genus)) # no NULLs
# seems ok

##### check dates ####

summary(ds$Year)
table(ds$Year) 

# seems right

sum(is.na(ds$Year)) # there's no NA counts
# ds <- ds[!is.na(ds$yearcollected),]
sum(ds$Year=="NULL") # there's no NULL counts
# ds <- ds[ds$yearcollected != "NULL",]
sum(ds$Year=="")  # there's no blanks
# ds <- ds[ds$yearcollected != "NULL",]

unique(ds$Month)
sort(unique(ds$Day))
unique(ds$Plot)

## sampleDesc
rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = ds$Family, 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDescr =  ds$SampleDescription,
                      Plot = ds$Plot,
                      Latitude = ds$Latitude, 
                      Longitude = ds$Longitude,
                      DepthElevation = rep("NULL",nrow(ds)),
                      Day = ds$Day,
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))
head(rawdata)

## aggregation check
dt_merged <- rawdata %>% group_by(Biomass,Family,Genus, Species,
                                  Plot,Latitude, Longitude,DepthElevation,
                                  Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- rawdata %>% group_by(Biomass,Family,Genus, Species,
                           Plot,Latitude, Longitude,DepthElevation,
                           Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1]

# all good now
### save raw data 
###code for calculating geographical features of the data

##load libraries
library(sp)
library(rgeos)

#Import the raw data file - has latitude & longitude

# this also transforms it from WGS84 coordinate reference to a mercator projection in km for calculating area in sq km
points<- SpatialPoints(cbind(rawdata$Longitude,rawdata$Latitude)) #%>% 
# sp::spTransform(., CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))


##2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

# 0.01157107

###get centroid

convhull<-gConvexHull(points)
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords
