# Curation Script ---------------------------------------------------------

# Dataset: Flora @ LTSER zone atelier Armorique France 1994-2017
# Location: Armorique, France
# Curator: Miguel Barbosa
# Date: June 2022
#Alignier, Audrey
#DOI 10.23728/b2share.f87c5e9c27e940ee9b98900e302f0bb8



# Set up ------------------------------------------------------------------
# loading the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(taxize)


install.packages("reshape2")
install.packages("reshape")


library(reshape2)
library(reshape)

rm(list=ls())



#import data
# data in different spreedsheets in a wide format. I have merged all spreedsheets before importing.
# New file called "S073_all.csv")
# Vegetation was sampled in 25 m long quadrats. ie. 25 m^2  will be the grain size



dt<-read.csv("S073_all.csv", header=T)

head(dt, n=20)
dim(dt)
names(dt)




###############################################################################

            ##### changing wide to long format ########

###############################################################################

## species are in columns - lets change from wide to long
#using melt() to go from wide to long

names(dt)

dt_long <- melt(dt, id = c("Year","Plot"),
                variable.name="Species", value.name="Abundance")



names(dt_long)
str(dt_long)




# add coordinates, the paper gives values of 48°36′N, 1°32′W
# use this link to transform coordinates into WSG84
# https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html


dt_long$Latitude <- as.numeric(rep('48.6', nrow(dt_long)))
dt_long$Longitude <- as.numeric(rep('1.53', nrow(dt_long)))






###############################################################################

                     ##### DATASET CRITERIA ########

###############################################################################


# Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(dt_long$Year))

n_distinct(dt_long$Year) >= 2



# checking if there are Nas and zero abundances

min(dt_long$Abundance)>0 #FALSE. Need to remove Nas and zero abundances


dt_long <- dt_long[!is.na(dt_long$Abundance),]
dt_long <- filter(dt_long, Abundance > 0)


min(dt_long$Abundance)>0 # all good. zeros and Nas removed



#checking for blanks

sum(dt_long$Abundance=="") > 0 #FALSE - no blanks



#make sure the data has all the necessary fields

dt_long$Biomass<- ""
dt_long$Family<-""
dt_long$StudyID<-""
dt_long$SampleDescription<-""
dt_long$DepthElevation<-""
dt_long$Genus<-""
dt_long$Day<-""
dt_long$Month<-""



names(dt_long)




###############################################################################

                           ###### check structure #######

###############################################################################


str(dt_long)

dt_long$Plot<-as.factor(dt_long$Plot)
dt_long$DepthElevation<-as.numeric(dt_long$DepthElevation)
dt_long$Day<-as.factor(dt_long$Day)
dt_long$Month<-as.factor(dt_long$Month)
dt_long$Year<-as.factor(dt_long$Year)


str(dt_long)





###############################################################################

                           ###### Taxonomy #######

###############################################################################


length(sort(unique(dt_long$Species)))  # 25, all good!

sort(unique(dt_long$Species))



dummy2<-str_split_fixed(string = dt_long$Species, pattern = "\\.", n = 3)    # split species into genus and species

dummy2<-as.data.frame(dummy2)

colnames(dummy2) <-c("Genus", "Species", "NA")

names(dummy2)

view(dummy2)

dummy2$Species[dummy2$Species=="spp"]<-"sp" # change spp for sp


unique(dummy2$Genus)
str_which(dummy2$Genus, 'idae$|eae$') # all good

unique(dummy2$Species)
str_which(dummy2$Species, 'idae$|eae$') # all good


#add the new variables to dt_long
dt_long$Genus<-dummy2$Genus
dt_long$Species<-dummy2$Species

view(dt_long)

dt_long$Species<-as.factor(dt_long$Species)
dt_long$Genus<-as.factor(dt_long$Genus)





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

points <- world + geom_point(data=dt_long, aes(x=Longitude, y=Latitude), shape=21, size=3, color='orange')

points + coord_fixed(xlim=c(-10,30), ylim=c(30,55))                     #the location seems correct :)



detach('package:maps')




###############################################################################

                     ######## Prepare for export ###########

###############################################################################


#aggregate abundance records that are same species, survey day and site.

dt_merged <- dt_long %>% group_by(Abundance, Biomass, Family, Genus,
                                  Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(dt_long)[1]-dim(dt_merged)[1] # any change in aggregating?

##### 0




###############################################################################

#### save the dataset name as an object so we save some typing

###############################################################################
dataset_name <- 'LTER_HogeKempenNationalPark_Arachnids.csv'


dt_merged$SampleDescription <- as.factor(with(dt_merged, paste (Year,Month, Day, Latitude, Longitude,Plot, sep='_')))


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

write.csv(dt_merged, "LTER_HogeKempenNationalPark_Arachnids_MB.csv") #written csv into working directory




###############################################################################

                      ##### Study Area #####

###############################################################################


# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata

dt_coord <- dt_merged %>% select(Longitude, Latitude) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


