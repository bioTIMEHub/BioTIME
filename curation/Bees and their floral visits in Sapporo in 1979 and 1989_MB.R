# Curation Script ---------------------------------------------------------

# Dataset: Bees and their floral visits in Sapporo in 1979 and 1989
# Location: Hokkaido, Japan
# Curator: Miguel Barbosa
# Date: July 2022

# Set up ------------------------------------------------------------------

# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)


install.packages("reshape2")
install.packages("reshape")
install.packages("dplyr")


library(reshape2)
library(reshape)
library(dbplyr)
library(readxl)
library(stringr)
library(dplyr)



# Data on bee species from 1979 and 1989. Bee species were quantified in weekly sampling census at two locations
# University campus (Hokkaido) (UC) and at the Botanic Garden (BG).
# a total of 12900 specimens were annotated


# Import data

setwd("~/Library/CloudStorage/OneDrive-UniversityofStAndrews/Desktop-Uni/Papers/BioTIME_v2")

getwd()

dt <- read.csv("ERDP-2020-01.2.1-Sakagami_bee_1979_1989.csv", header=T)



# check data
names(dt)


dt$Abundance<-"1"  # create variable abundance

colnames(dt)[5] <- "Species"  # change name of variable Bee to species





# need to split Date into day and month

dummy<-str_split_fixed(string = dt$Date, pattern = "_", n = 2)

dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Month", "Day")



dt$Month<-dummy$Month
dt$Day<-dummy$Day



names(dt)

# delete variables specimenID, Sex, date, Period & Flower

dt<-dt[,-c(1, 6, 7, 8, 9)]  # delete date

names(dt) # all good





# add coordinates, the paper gives values of 43°03′N, 141°19'′E for UC
# and 43°06′N, 141°21'E for Botanic Garden
# use this link to transform coordinates into WSG84
# https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html
# UC - 43.05 Latitude and 141.316667 Longitude
# BG - 43.1 Latitude and 141.35 Longitude


dt$Latitude <- as.numeric("")
dt$Longitude <- as.numeric("")


dt$Latitude[dt$site=="UC"]<-"43.05"
dt$Longitude[dt$site=="UC"]<-"141.316667"

dt$Latitude[dt$site=="BG"]<-"43.1"
dt$Longitude[dt$site=="BG"]<-"141.35"




###############################################################################

                  ##### DATASET CRITERIA ########

###############################################################################


# Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(dt$Year))

n_distinct(dt$Year) >= 2



# checking if there are Nas and zero abundances

min(dt$Abundance)>0 #TRUE. All good


#checking for blanks

sum(dt$Abundance=="") > 0 #FALSE - no blanks



names(dt)#make sure the data has all the necessary fields

dt$Biomass<- ""
dt$Genus<-""
dt$StudyID<-""
dt$SampleDescription<-""
dt$Plot<-""
dt$DepthElevation<-""

names(dt)




###############################################################################

              ###### check structure #######

###############################################################################

str(dt)
dt$site<-as.factor(dt$site)
dt$Plot<-as.factor(dt$Plot)
dt$DepthElevation<-as.numeric(dt$DepthElevation)
dt$Day<-as.factor(dt$Day)
dt$Month<-as.factor(dt$Month)
dt$Year<-as.factor(dt$Year)
dt$Species<-as.factor(dt$Species)
dt$Latitude<-as.numeric(dt$Latitude)
dt$Longitude<-as.numeric(dt$Longitude)
dt$Abundance<-as.numeric(dt$Abundance)

str(dt)





###############################################################################

                      ###### Taxonomy #######

###############################################################################

# the file ERDP-2020-01.2.1-Sakagami_bee_1979_1989.csv contains 12900 specimenens from 6 families



sort(unique(dt$Species))

# Lg.(Evylaeus) sp. Evylaeus is a subgenus of the Genus Lasioglossum 


dt<-dt[!with(dt, Species%in%c("Lg.(Evylaeus) sp.")), ]



# some of the species have () in between the genus and the species. Need to remove these

dt$Species1 <- str_remove_all(dt$Species, "\\(.*?\\)")     



dummy2<-str_split_fixed(string = dt$Species1, pattern = " ", n = 6)    # split species into genus and species

dummy2<-as.data.frame(dummy2)

colnames(dummy2) <-c("Genus", "SpeciesNew", "NA1", "NA2", "NA3", "NA4")

names(dummy2)


dt$Species<-dummy2$SpeciesNew
dt$Genus<-dummy2$Genus


names(dt)

dt<-dt[,-16]

dt$Species<-as.factor(dt$Species)
dt$Genus<-as.factor(dt$Genus)

summary(dt$Species)

dt$Species[dt$Species==""]<-"sp."


str_which(dt$Genus, 'idae$|eae$') # all good
str_which(dt$Species, 'idae$|eae$') # all good





###############################################################################

            ###### Check location of study #######

###############################################################################

library(maps)
library(ggmap)




world_map <- map_data('world')
world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21, size=3, color='orange')

points + coord_fixed(xlim=c(120,160), ylim=c(30,55))                     #the location seems correct :)



detach('package:maps')




###############################################################################

                ######## Prepare for export ###########

###############################################################################


#aggregate abundance records that are same species, survey day and site.

dt_merged <- dt %>% group_by(Abundance, Biomass, Family, Genus,
                                  Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID, site) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?

##### 12082 seems correct





###############################################################################

        #### save the dataset name as an object so we save some typing

###############################################################################
dataset_name <- 'Bees and their floral visits in Sapporo in 1979 and 1989.csv'


dt_merged$SampleDescription <- as.factor(with(dt_merged, paste (Year,Month, Day, Latitude,Longitude, sep='_')))


length(levels(dt_merged$SampleDescription)) # how many sampling events?




dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)

head(dt_merged) # final check!




###############################################################################
                #####Export and spreadsheet prep
###############################################################################

write.csv(dt_merged, "Bees and their floral visits in Sapporo in 1979 and 1989_MB.csv") #written csv into working directory





###############################################################################

            ##### Study Area #####

###############################################################################

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)
