# Curation Script ---------------------------------------------------------

# Dataset: Beldade 2015
# Location: Tuamotu Archipelago, French Polynesia
# Curator: Miguel Barbosa
# Date: June 2022

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)





rm(list=ls()) # clear up the environment before starting





# make sure your working directory is set before running these lines

setwd("~/Desktop/DataCuration-workshop-main")

dt <- read.csv('Beldade_2015.csv', header=T)




# check data

names(dt) # there are a few species names that need to be removed (eg.Unidentified.goby )
head(dt, n=20)
dim(dt)




#need to have species in rows

#using melt() to go from wide to long

dt_long <- melt(dt, id = c("site","year"),variable.name="Species", value.name="Abundance")

# renaming variable and value

colnames(dt_long)[3]<-"Species"
colnames(dt_long)[4]<-"Abundance"


# because sites were unique we can use them to identify plot

colnames(dt_long)[1]<-"Plot"

#check new dataframe and data structure

head(dt_long)
dim(dt_long)
str(dt_long)
names(dt_long)





#make sure the data has all the necessary fields

dt_long$Biomass<- ""
dt_long$Family<-""
dt_long$StudyID<-""
dt_long$SampleDescription<-""
dt_long$Latitude<-""
dt_long$Longitude<-""
dt_long$DepthElevation<-""
dt_long$Day<-""
dt_long$Month<-""



names(dt_long)



# putting capital Year


colnames(dt_long)[2]<-"Year"


names(dt_long)



#just making sure variables are in the same order as the example


dt_long<-dt_long %>% select(Abundance,Biomass, Family, Species,
      SampleDescription,Plot,Latitude, Longitude,DepthElevation,Day,Month,
         Year, StudyID)


names(dt_long)



# check structure

str(dt_long)


dt_long$Plot<-as.factor(dt_long$Plot)
dt_long$DepthElevation<-as.numeric(dt_long$DepthElevation)



#At the moment there is one variable called Species with
#genus and species together. We need to create two new variables
# Genus and Species


# split genus+species

# NOTE! I have noticed that there are species called "unidentified.goby"
# and "unidentified.labrid". These need to be removed
# lets remove this entry before splitting the variable

sort(unique(dt_long$Species))

dt_long<-dt_long%>%filter(Species!="Unidentified.goby")
dt_long<-dt_long%>%filter(Species!="Unidentified.labrid")

#check

unique(dt_long$Species) # seems good



# lets split the variable "species" into genus and species
# some of the names have three objects G s ., hence we need to
# tell R to split into 3 objects

dummy<-str_split_fixed(string = dt_long$Species, pattern = "\\.", n = 3)

dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Genus", "Species", "NA")

dummy<-dummy[,-3]

head(dummy)

sort(unique(dummy$Species))
sort(unique(dummy$Genus))

#add these two new variables into dt_long
dt_long$Genus<-dummy$Genus
dt_long$Species<-dummy$Species



names(dt_long)


# check that genera and species are not family names (-idae/eae)

# this returns the record index number if there are any

str_which(dt_long$Genus, 'idae$|eae$')
str_which(dt_long$Species, 'idae$|eae$')


# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(dt_long$Species))





#re order variables
dt_long<-dt_long %>% select(Abundance,Biomass,Family, Genus, Species,SampleDescription,Plot,Latitude, Longitude,DepthElevation,
                            Day,Month,Year, StudyID)


names(dt_long)



str(dt_long) # all seems good!




#add coordinates, the paper gives values of 14°55′S, 148°36′W
#use this link to transform coordinates into WSG84
#https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html


dt_long$Latitude <- as.numeric(rep('14.9', nrow(dt_long)))
dt_long$Longitude <- as.numeric(rep('148.6', nrow(dt_long)))



#Check if WSG84 coordinates match study area

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt_long[1,], aes(x=Longitude, y=Latitude), col="red", shape=21)

points + coord_fixed(xlim=c(100,160), ylim=c(0,20))



#the location seems correct


detach('package:maps')






# DATASET CRITERIA

#Dataset consists of at least 2 years of sampling (they do not have to be consecutive).



length(unique(dt_long$Year))

n_distinct(dt_long$Year) >= 2



#checking primary fields

#checking if there are zero abundances

min(dt_long$Abundance)>0 #FALSE. Need to remove zero abundances

dt_long <- filter(dt_long, Abundance > 0)

#check

min(dt_long$Abundance)>0 #TRUE - no zero abundances



#checking for NAs/blanks

sum(dt_long$Abundance=="") > 0 #FALSE - no blanks




#Prepare for export


#aggregate abundance records that are same species, plot, survey day and site.

dt_merged <- dt_long %>% group_by(Abundance, Biomass, Family, Genus,
                                Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(dt_long)[1]-dim(dt_merged)[1] # any change in aggregating?


# save the dataset name as an object so we save some typing


dataset_name <- 'Beldade_2015'


#I am not adding Lat and Long because there is only one set

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year,Latitude, Longitude, Plot, sep='_')))


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






#Export and spreadsheet prep


write.csv(dt_merged, 'Beldade_2015_rawdata_MB.csv', row.names=F, na='') # replace your initials here







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


