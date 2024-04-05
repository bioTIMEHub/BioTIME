# Curation Script ---------------------------------------------------------

# Dataset: Burlakova_2012_1990&1995
# Location: Great Lakes, Ontario, Canada
# Curator: Miguel Barbosa
# Date: June 2022

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)

install.packages("MASS")
install.packages("reshape2")
install.packages("reshape")

library(MASS)
library(reshape2)
library(reshape)

# this is a messy dataset, with different methodologies and sampling efforts. I have subset the original dataset into
# comparable years (ie. same sampling methods). This first one is for sampling years 1990 and 1995.
# invertebrates were sampled using a Ponar sampler, the area sampled was 0.053 m2 and specimens were sorted through a 600um.
# there were 25 and 41 sampled stations in 1990 and 1995 respectively. Only 1 replicate per station was processed


# I will import all the data and later subset the years

d<-read.csv("burlakova_2021.csv", header=T)

names(d)
head(d, n=10)
dim(d)
str(d)

d<-d[,-1]

#sub-setting data - we only want years 1995 and 1990

d_new<-subset(d, Year=="1990" | Year== "1995")

unique(d_new$Year)


#Only one replicate per station was processed for most of the stations due to limited funding except for
#the annually sampled BioIndex sites (#41, 81A, 93, and 93A)  had all 3 replicate samples analyzed.
#these stations need to be deleted

# again I am sure there is an easy/faster way to do this, but I can think of any...

str(d_new)

d_new$Station

d_new <- d_new[-c(12,25,45,60,62),]



## species are in columns - lets change from wide to long
#using melt() to go from wide to long

names(d_new)

d_long <- melt(d_new, id = c("Station","Year", "Latitude", "Longitude", "Depth_m"),
               variable.name="Species", value.name="Abundance")



names(d_long)

# Changing the name of variables. Not sure why melt function did not rename the variables and values

colnames(d_long)[1] <- "Site"
colnames(d_long)[6] <- "Species"
colnames(d_long)[7] <- "Abundance"


names (d_long) # all good now



# DATASET CRITERIA

#Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(d_long$Year))

n_distinct(d_long$Year) >= 2



#checking if there are zero abundances

min(d_long$Abundance)>0 #FALSE. Need to remove zero abundances

d_long <- filter(d_long, Abundance > 0)

#check

min(d_long$Abundance)>0 #TRUE - no zero abundances



#checking for NAs/blanks

sum(d_long$Abundance=="") > 0 #FALSE - no blanks





#make sure the data has all the necessary fields

d_long$Biomass<- ""
d_long$Family<-""
d_long$StudyID<-""
d_long$SampleDescription<-""
d_long$Plot<-""
d_long$DepthElevation<-""
d_long$Day<-""
d_long$Month<-""



names(d_long)



# check structure

str(d_long)

d_long$Site<-as.factor(d_long$Site)
d_long$Plot<-as.factor(d_long$Plot)
d_long$DepthElevation<-as.numeric(d_long$DepthElevation)

str(d_long)




#At the moment there is one variable called Species with
#genus and species together. We need to create two new variables
# Genus and Species


# split genus+species


sort(unique(d_long$Species))

# lets split the variable "species" into genus and species
# some cells have two objects separated by _ hence we need to
# tell R to split into 2 variables
# note that I am adding 3 variables because some of the names should go on a Family variable



dummy<-str_split_fixed(string = d_long$Species, pattern = "\\_", n = 3)


dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Genus1", "Species1", "Family1")

names(dummy)



unique(dummy$Species1) # they all seem to be species


unique(dummy$Genus1) # there are a few families/Order and class in this list
                     # these need to be moved to a Family variable



# the below line of code searches words that end on "idae", "eae", ... and copy them to
# the Family variable on dummy



dummy$Family1[str_which(dummy$Genus1,'idae$|eae$')]<-dummy$Genus1[str_which(dummy$Genus1,'idae$|eae$')]

dummy$Family1[str_which(dummy$Genus1,'oda$|inae$')]<-dummy$Genus1[str_which(dummy$Genus1,'oda$|inae$')]

dummy$Family1[str_which(dummy$Genus1,'tea$|aeta$')]<-dummy$Genus1[str_which(dummy$Genus1,'tea$|aeta$')]




# I need to delete the "words" from genus that were copied into Family
# I am sure there is a smarter and quicker way to do this, but at the moment this is the best I can think of

dummy$Genus1[dummy$Genus1 == 'Caecidotea'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Gastropoda'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Naididae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Nemertea'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Sphaeriidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Tubificidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Chironomidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Oligochaeta'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Orthocladiinae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Tanypodinae'] <- 'NA'




#add these three new variables to d_long
d_long$Genus<-dummy$Genus1
d_long$Species1<-dummy$Species1
d_long$Family1<-dummy$Family1



#delete species and family variables

d_long<-d_long[,-c(6,9)]




#rename variables Species1 and Family 1

colnames(d_long)[15] <- "Species"
colnames(d_long)[16] <- "Family"


names(d_long) # all seems good





#The paper gives coordinates decimal degrees (WSG84)

#Check if WSG84 coordinates match study area

world_map <- map_data('world')
world <- ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   coord_fixed() +
   labs(x='Longitude', y='Latitude') +
   theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=d_long, aes(x=Longitude, y=Latitude), col="red", shape=21)

points + coord_fixed(xlim=c(-100,-60), ylim=c(20,50))

#the location seems correct


detach('package:maps')





#Prepare for export


#aggregate abundance records that are same species, survey day and site.

dt_merged <- d_long %>% group_by(Abundance, Biomass, Family, Genus,
                                  Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID, Site) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(d_long)[1]-dim(dt_merged)[1] # any change in aggregating?





# save the dataset name as an object so we save some typing


dataset_name <- 'Burlakova_2021'


#I am not adding Lat and Long because there is only one set

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name,Year, Site, Latitude, Longitude, sep='_')))


length(levels(dt_merged$SampleDescription)) # how many sampling events?

# in 1990 there were 21 sampling events and in 1995 45.



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
                         'StudyID',
                         'Site')] %>% arrange(Year, Family, Genus, Species)

head(dt_merged) # final check!



#Export and spreadsheet prep


write.csv(dt_merged, 'Burlakova_2021_1990&1995_rawdata_MB.csv', row.names=F, na='') # replace your initials here

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


