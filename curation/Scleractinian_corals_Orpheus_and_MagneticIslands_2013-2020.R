## Curation script for Baird and Kuo update
## curator VB

setwd("D:/Dropbox/towards BioTIME v2/Vivi-dataset hunting files/updates/Baird")
ds_name <- "Scleractinian_corals_Orpheus_and_MagneticIslands_2013-2020"

library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(maps)
library(dplyr)
library(readxl)

ds <- read_excel("PIT_OIMI_2013-20_noturf.xlsx", sheet = "data")
str(ds)
names(ds)
str(ds)
dim(ds)
#### add location based on previous submission
coords <- read.csv("plot_to_coords.csv")
ds_coords <- merge(ds,coords, by=c("site","depth"))
unique(ds_coords[,c("site","depth","PLOT","LATITUDE","LONGITUDE")])
ds <- ds_coords
#cool

sort(unique(ds$Points)) # inspect values
table(ds$Points)

dim(ds)  #  3533   14

##### check taxa ####
sort(unique(ds$taxa))
sum(is.na(ds$taxa))
summary(str_count(ds$taxa, " ")) #some have 1 or 3 strings
unique(ds[which(str_count(ds$taxa, " ") == 0),]$taxa)
# all these are not hard corals and will need to go
ds <- ds[-which(str_count(ds$taxa, " ") == 0),]
ds[which(str_count(ds$taxa, " ") == 2),]
unique(ds[which(str_count(ds$taxa, " ") == 2),]$taxa) 
#give a species epiteth to keep it different - check that Acropora sp will do

ds$Genus <- word(ds$taxa, 1)
unique(ds$Genus) # ok, fix this as well
ds <- ds[ds$Genus != "Briareum",]# not a hard coral
ds <- ds[ds$Genus != "other",] # not a hard coral
ds <- ds[ds$Genus != "soft",]# not a hard coral
ds <- ds[ds$Genus != "black",] # not a exa coral
ds <- ds[ds$Genus != "macroalgal" ,]# not a hard coral
ds <- ds[ds$Genus != "macroalgae" ,]# not a hard coral
ds <- ds[ds$Genus != "Macroalgae",]# not a hard coral
# back to Acropora cf nasuta
sort(unique(ds$taxa[ds$Genus == "Acropora"])) #there is a few to fix
ds$taxa[ds$taxa == "Acropora 74-0521"] <- "Acropora sp1"
ds$taxa[ds$taxa == "Acropora 74-0533"] <- "Acropora sp2"
ds$taxa[ds$taxa == "Acropora 74-2080"] <- "Acropora sp3"
ds$taxa[ds$taxa == "Acropora cf nasuta"] <- "Acropora sp4"
ds$taxa[ds$taxa == "Acropora digitifera_fat"] <- "Acropora fat"
ds$taxa[ds$taxa == "Acropora sp_nov_1"] <- "Acropora sp5"
ds$taxa[ds$taxa == "Acropora sp_nov_2"] <- "Acropora sp6"

summary(str_count(ds$taxa, " "))
# yep!

ds$Species <- word(ds$taxa, 2)
sort(unique(ds$Species)) # check weirdos
ds[ds$Species == "74-2074",] # Favites 74-2074 x2
ds[ds$Species == "74-2074",]$Species <- "sp"
ds[ds$Species == "74-2112",] # Acanthastrea 74-2112
ds[ds$Species == "74-2112",]$Species <- "sp"
ds[ds$Species == "fruiticosa",] #Goniporoa fruiticosa  x1 in 2019
ds[ds$Species == "fruiticosa",]$Species <- "fruticosa"
ds[ds$Species == "fruticosa",] #Goniopora fruticosa x2 in 2020
ds[ds$Species == "spp",]
ds$Species[ds$Species  == "spp"] <- "sp"

length(unique(ds$Genus))
sum(is.na(ds$Genus)) # no NAs
sum(ds$Genus=="") # no blanks
sum(is.null(ds$Genus)) # no NULLs
length(unique(ds$Species))
sum(is.na(ds$Species)) # no NAs
sum(ds$Species=="") # no blanks
sum(is.null(ds$Species)) # no NULLs

# seems ok

##### check dates ####

summary(ds$date)
str(ds$date)
ds$Year <- as.numeric(format(ds$date, format = "%Y"))
ds$Month <- as.numeric(format(ds$date, format = "%m"))
ds$Day <- as.numeric(format(ds$date, format = "%d")) 
summary(ds)
# seems right

## sampleDesc
ds$SampleDescription <- as.factor(paste0(ds_name, "_",
                                         ds$LATITUDE,"_",
                                         ds$LONGITUDE,"_",
                                         ds$transect,"_",
                                         ds$Day, "_",
                                         ds$Month,"_",
                                         ds$Year))

dim(ds)
str(ds)

rawdata <- data.frame(Abundance = rep("NULL",nrow(ds)),
                      Biomass = ds$Points, 
                      Family = rep("NULL",nrow(ds)), 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDescr =  ds$SampleDescription,
                      Plot = ds$transect,
                      Latitude = ds$LATITUDE, 
                      Longitude = ds$LONGITUDE,
                      DepthElevation = rep("NULL",nrow(ds)),
                      Day = ds$Day,
                      Month = ds$Month,
                      Year = ds$Year,
                      StudyID = rep(429,nrow(ds)))
head(rawdata)

## aggregation check
dt_merged <- rawdata %>% group_by(Abundance, Family,Genus, Species,
                                  Plot,Latitude, Longitude,DepthElevation,
                                  Day, Month,Year,StudyID) %>%
  summarise(Biomass = sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- rawdata %>% group_by(Abundance, Family,Genus, Species,
                           Plot,Latitude, Longitude,DepthElevation,
                           Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1]
dim(dt)
dim(dt_merged)

table(dt_merged$Biomass)

### save raw data 

ds<- as.data.frame(dt_merged)

rawdata <- data.frame(Abundance = ds$Abundance,
                      Biomass = ds$Biomass, 
                      Family = ds$Family, 
                      Genus = ds$Genus,
                      Species = ds$Species,
                      SampleDescr =  as.factor(paste0(ds_name, "_",
                                                      ds$Latitude,"_",
                                                      ds$Longitude,"_",
                                                      ds$Plot, "_",
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
                      StudyID = rep(429,nrow(ds)))

dim(rawdata) #2441   14


write.csv(rawdata, paste0(ds_name, "_RAWDATA_VB.csv"), row.names = FALSE)

###geographical features as per previous contribution





