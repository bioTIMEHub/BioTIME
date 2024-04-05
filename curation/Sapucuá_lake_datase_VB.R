## Curation script for Sapucuá lake datase
## curator VB

setwd("D:/Dropbox/towards BioTIME v2/Vivi-dataset hunting files/Ale-Brazil")
ds_name <- "Sapicua_lake_fish"

library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)
library(dplyr)
library(readxl)

ds <- read_excel("Sapucuá lake dataset_VB.xlsx", sheet = "rawData")

# transform to long format
str(ds)

ds <- ds[,1:14]
names(ds)
str(ds)

#### check locations
unique(ds$Plot)
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
points

sort(unique(ds$Abundance)) # inspect values
is.numeric(ds$Abundance)
table(ds$Abundance)

dim(ds)  #  175  14

##### check taxa ####
sort(unique(ds$Species)) # only epiteth
sum(is.na(ds$Species))
summary(str_count(ds$Species, " ")) #some have 3 strings
ds[which(str_count(ds$Species, " ") == 2),]
unique(ds[which(str_count(ds$Species, " ") == 2),]$Species)

# fix taxonomy
ds$Species[ds$Species == "Brycon aff. pesu"] <- "Brycon pesu"
ds$Species[ds$Genus == "Leporinus"] 
ds$Species[ds$Species == "Leporinus sp. 1"] <- "Leporinus sp"
summary(str_count(ds$Species, " ")) 
# yep!

ds$Species <- word(ds$Species, 2)
sort(unique(ds$Species)) # only epiteth
ds$Species[ds$Species  == "sp."] <- "sp"
length(unique(ds$Genus))
sum(is.na(ds$Species)) # no NAs
sum(ds$Species=="") # no blanks
sum(is.null(ds$Species)) # no NULLs

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
table(ds$Abundance)

## sampleDesc
ds$SampleDescription 
ds$SampleDescription <- as.factor(paste0(ds_name, "_",
                                         ds$Latitude,"_",
                                         ds$Longitude,"_",
                                         ds$Day, "_",
                                         ds$Month,"_",
                                         ds$Year))

dim(ds)
str(ds)

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
table(dt_merged$Abundance)

### save raw data 

ds<- dt_merged

rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = ds$Biomass, 
                      Family = ds$Family, 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDescr =  as.factor(paste0(ds_name, "_",
                                                      ds$Latitude,"_",
                                                      ds$Longitude,"_",
                                                      ds$Day, "_",
                                                      ds$Month,"_",
                                                      ds$Year)),
                      Plot = ds$Plot,
                      Latitude = ds$Latitude, 
                      Longitude = ds$Longitude,
                      DepthElevation = ds$DepthElevation,
                      Day = ds$Day,
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep("NULL",nrow(ds)))

dim(rawdata) #175  14


write.csv(rawdata, paste0(ds_name, "_RAWDATA_VB.csv"), row.names = FALSE)

###geographical features of the data have been provided