## Curation script for Wilson and Graham: reef fish
## curator VB

setwd("D:/Dropbox/towards BioTIME v2/Vivi-dataset hunting files/Graham")
ds_name <- "Seychelles reef fish"

library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)
library(dplyr)


ds <- read.csv("Graham_Wilson Seychelles FISH data_BioTIME.csv", head = TRUE)

# transform to long format
str(ds)

names(ds)
ds<- ds[,c(1,2,4,5,6,11,12,15,16)]

str(ds)

#write.csv(unique(ds[,c(1,8,9)]),"location_coords.csv", row.names = FALSE)

# get to know the data 
table(ds$count, ds$Year)# some years less replicates
table(ds$Location, ds$Year) # some years less sites
unique(ds$lat)
unique(ds$lon)
names(ds)
dim(ds)

# check taxonomy
sort(unique(ds$Species))
table(ds$Species)

ds$genus <- word(ds$Species,1)
sort(unique(ds$genus))
#View(ds[ds$genus=="Ambyglyphidodon",]) #this should be "Amblyglyphidodon"
ds$genus[ds$genus=="Ambyglyphidodon"] <- "Amblyglyphidodon"
#View(ds[ds$genus=="Cantherines",]) #this should be "Cantherhines"
ds$genus[ds$genus=="Cantherines"] <- "Cantherhines"
#View(ds[ds$genus=="siganus",]) #this should be "Siganus"
ds$genus[ds$genus=="siganus"] <- "Siganus"

ds$sp <- word(ds$Species,2)
sort(unique(ds$sp))

#View(ds[ds$sp=="chrysopterus",])#this should be "chrysopterum" (Sufflamen chrysopterum)
#View(ds[ds$sp=="chrysopterum",])
ds$sp[ds$sp == "chrysopterus"] <- "chrysopterum"

#View(ds[ds$sp=="prasiognathos",])
#View(ds[ds$sp=="prasiognathus",])#this should be "prasiognathos" (Scarus prasiognathos)
ds$sp[ds$sp == "prasiognathus"] <- "prasiognathos"

#View(ds[ds$sp=="trifascialis",])#this fine
#View(ds[ds$sp=="trifasciatus",])#this fine

#View(ds[ds$sp=="zanzibarensis",])
#View(ds[ds$sp=="zanzibariensis",])#this should be zanzibarensis(Chaetodon zanzibarensis)
ds$sp[ds$sp == "zanzibariensis"] <- "zanzibarensis"

sort(unique(ds$sp))
names(ds)
dim(ds)

# aggregate

ds <- ds %>% group_by(Location,count,Year, Family, genus,sp, lat, lon) %>% 
  mutate(Abundance = n(), Biomass = sum(mass.g)) %>% 
  select(Location,count,Year, Family, genus,sp, lat, lon, Abundance, Biomass) %>% 
  unique()
summary(ds)#yes, all good
tail(sort(ds$Abundance))
table(ds$Abundance)
tail(sort(ds$Biomass))

ds[ds$Abundance == max(ds$Abundance),] # species and biomass makes sense
ds[ds$Biomass == max(ds$Biomass),] # species and abundance make sense
nrow(ds) # 27647 records

# prepare for export
rawdata <- data.frame(Abundance = ds$Abundance,
                            Biomass = ds$Biomass, 
                            Family = ds$Family, 
                            Genus = ds$genus,
                            Species = ds$sp,
                            SampleDescr =  as.factor(paste0(ds_name, "_",
                                                            ds$Location, "_",
                                                            ds$lon, "_",
                                                            ds$lat, "_",
                                                            ds$Year,"_",
                                                            ds$count)),
                            Plot = ds$count,
                            Latitude = ds$lat, 
                            Longitude = ds$lon, 
                            DepthElevation = rep("NULL",nrow(ds)),
                            Day = rep("NULL",nrow(ds)), 
                            Month = rep("NULL",nrow(ds)), 
                            Year = ds$Year,
                            StudyID = rep("NULL",nrow(ds)))
head(rawdata)

## map

world_map <- map_data("world")
world <- ggplot() + coord_fixed() +xlab("") + ylab("")
world  <- world  +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),
               colour="gray60", fill="gray60")
points <- world  +
  geom_point(data=as.data.frame(cbind(long = rawdata$Longitude, lat = rawdata$Latitude)),
             aes(x=long, y=lat, alpha=0.01))
points
#seems right

write.csv(rawdata, paste0(ds_name, "_RAWDATA_VB.csv"), row.names = FALSE)



##load libraries for computing areas and central coords
library(sp)
library(rgeos)

#Import the raw data file - has latitude & longitude

# this also transforms it from WGS84 coordinate reference to a mercator projection in km for calculating area in sq km
points<- SpatialPoints(cbind(rawdata$Longitude,rawdata$Latitude)) #%>% 
# sp::spTransform(., CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))


##2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

# 0.0615608

###get centroid

convhull<-gConvexHull(points)
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords

# 55.56077 -4.530888
