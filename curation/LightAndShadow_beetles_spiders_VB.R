###  Curation script for LightAndShadow_beetles_spiders###

###### upload libraries #####
# library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)
library(dplyr)

###### read dataset ######
mypath <- getwd()     
mypath

ds <- read.csv(file = paste0(mypath,"/6mal6_Biotime_raw_spider_beetle.csv"), h=T)

# transform to long format
str(ds)
dim(ds)

#### add and check locations
unique(ds$Plot)
table(ds$Plot)
table(ds$Latitude)

## map

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=as.data.frame(cbind(long = ds$Longitude, lat = ds$Latitude)), 
             aes(x=long, y=lat, alpha=0.01)) 
points

sort(unique(ds$Abundance)) # inspect values
is.numeric(ds$Abundance)

dim(ds)  #  6902    14

##### check taxa ####
length(unique(ds$Species)) # 
length(unique(ds$Genus))
sum(is.na(ds$Species)) # no NAs
sum(ds$Species=="") # no blanks
sum(is.null(ds$Species)) # no NULLs
sort(unique(ds$Species)) # sorting taxa to catch duplications and mispelling
sort(unique(ds$Genus)) # sorting taxa to catch duplications and mispelling
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
ds$Month[ds$Month == "August"] <- 8
ds$Month[ds$Month == "September"] <- 9
ds$Month[ds$Month == "Mai"] <- 5
ds$Month[ds$Month == "Juni"] <- 6
ds$Month[ds$Month == "April"] <- 4
ds$Month[ds$Month == "Juli"] <- 7
unique(ds$Month)
unique(ds$Day)

unique(ds$Plot)
ds$Plot <- substr(ds$Plot,start=1, stop =3)

## sampleDesc
ds$SampleDescription 
ds$SampleDescription <- as.factor(paste0("LightAndShadow_spiders_beetles_",
                                         ds$Latitude,"_",
                                         ds$Longitude,"_",
                                         ds$DepthElevation,"_",
                                         ds$Month,"_",
                                         ds$Year,"_",
                                         ds$Plot))

dim(ds)
str(ds)

rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = rep(NA,nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDesc =  ds$SampleDescription,
                      Plot = ds$Plot,
                      Latitude = ds$Latitude, 
                      Longitude = ds$Longitude,
                      DepthElevation = ds$DepthElevation,
                      Day = rep("NULL",nrow(ds)),
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

dim(dt)[1] - dim(dt_merged)[1] #nope!

View(dt[duplicated(dt[,2:14]),4:13]) # this got aggregated
write.csv(dt[duplicated(dt[,2:14]),4:13], "double_entries_rawdata_spiders_beetles.csv")
ds <- dt_merged

rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = rep(NA,nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDesc =  as.factor(paste0("LightAndShadow_fungi_",
                                                     ds$Latitude,"_",
                                                     ds$Longitude,"_",
                                                     ds$DepthElevation,"_",
                                                     ds$Month,"_",
                                                     ds$Year,"_",
                                                     ds$Plot)),
                      Plot = ds$Plot,
                      Latitude = ds$Latitude, 
                      Longitude = ds$Longitude,
                      DepthElevation = ds$DepthElevation,
                      Day = rep("NULL",nrow(ds)),
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))


### save raw data 
write.csv(rawdata, "LightandShadow2021_beetles_spiders_RAWDATA_VB.csv", row.names = FALSE)

dim(ds)
str(ds)
rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = rep("NULL",nrow(ds)), 
                      Family = rep(NA,nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDesc =  paste0(ds$Latitude ,"_",
                                           ds$Longitude,"_",
                                           ds$DepthElevation,"_",
                                           ds$Plot, "_",
                                           "LightAndShadow_spider_beetles_",
                                           #ds$daycollected,"_",
                                           ds$Month,"_",
                                           ds$Year),
                      #ds$depth,"_",
                      #ds$bottomdepth),
                      Plot = ds$Plot,
                      Latitude = ds$Longitude, 
                      Longitude = ds$Latitude,
                      DepthElevation = ds$DepthElevation,
                      Day = rep("NULL",nrow(ds)),
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))
head(rawdata)

### save raw data 
write.csv(rawdata, "LightAndShadow_beetles_spiders_RAWDATA.csv", row.names = FALSE)

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

# 0.0164812

###get centroid

convhull<-gConvexHull(points)
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords
