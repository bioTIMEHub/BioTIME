# Curation Script ---------------------------------------------------------

# Dataset: Christensen 2021
# Location: New Mexico, USA
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



# this is a comparatively clean dataset. The authors collect plant abundance data from 1915 to 2016 using 122 permanent
# quadrats of 1 m2. The raw data has been mostly curated into the BioTIME format by Alban. The most significant difference
# between BioTIME format and Alban's curation was that Alban only kept the stations sampled more than 1 year and only one month per year
# for BioTIME we are interested in repeated sampling across time, so we will keep all stations
# an important note - some records were marked with 1 which indicates that either the number of individuals was extrapolated/extimated
# or the notes are not eligible so an abundance was estimated. After discussing with Vivi and Cher, we decided to keep these
# data as most was for extremely abundant species (ie >500 individuals).
# because there were permanent quadrats, we can use site to identify plot



rm(list=ls()) # clear up the environment before starting




# make sure your working directory is set before running these lines

setwd("~/Library/CloudStorage/OneDrive-UniversityofStAndrews/Desktop-Uni/Papers/BioTIME_v2")

dt <- read.csv('christensen_2021.csv', header=T)



# check data

names(dt)
#some of the variables are not relevant for BioTIME

dt<-dt[-c(1,8,9,10,11)]

head(dt, n=20)

# changing variable names

colnames(dt)[1]<-"Site"
colnames(dt)[2]<-"Year"
colnames(dt)[3]<-"Month"
colnames(dt)[4]<-"Species"
colnames(dt)[5]<-"Abundance"


dim(dt)
names(dt)


###############################################################################

                  ##### DATASET CRITERIA ########

###############################################################################


# Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(dt$Year))

n_distinct(dt$Year) >= 2



# checking if there are zero abundances

min(dt$Abundance)>0 #TRUE - no zero abundances



#checking for NAs/blanks

sum(dt$Abundance=="") > 0 #FALSE - no blanks





#make sure the data has all the necessary fields

dt$Biomass<- ""
dt$Family<-""
dt$StudyID<-""
dt$SampleDescription<-""
dt$Latitude<-""
dt$Longitude<-""
dt$Plot<-""
dt$DepthElevation<-""
dt$Day<-""

names(dt)


# as I mentioned above, because quadrats were permanent we can use site as plot

dt$Plot<-dt$Site

dt<-dt[-1]

head(dt)








###############################################################################

                     ###### check structure #######

###############################################################################

str(dt)

dt$Plot<-as.factor(dt$Plot)
dt$DepthElevation<-as.numeric(dt$DepthElevation)
dt$Day<-as.numeric(dt$Day)
dt$Month<-as.numeric(dt$Month)


str(dt)







###############################################################################

                        ###### Taxonomy #######

###############################################################################


# At the moment there is one variable called Species with
# genus,species, variant and subspecies together.
# there are a few (n=18) species entries that were not ID to species level.
# these individuals were given an individual code (eg. UNKAF2)



sort(unique(dt$Species))

duplicated(unique(dt$Species)) # all FALSE which means no duplicates

sum(duplicated(unique(dt$Species))) # 0 which means no duplicates)



# split variable into 4 new variables

dummy<-str_split_fixed(string = dt$Species, pattern = "\\ ", n = 4)


dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Genus1", "Species1", "variant", "subspecies")

names(dummy)




# checked all genus and species and taxonomy seems ok

unique(dummy$Genus1) # bear in mind that some genus were identified (eg UNKAF4)

unique(dummy$Species1)






#add the two new variables to dt
dt$Genus1<-dummy$Genus1
dt$Species1<-dummy$Species1


names(dt)


#delete species from dt and rename Species1 as Species and Genus1 as Genus

dt<-dt[,-3]


colnames(dt)[14] <- "Genus"
colnames(dt)[15] <- "Species"


names(dt) # all seems good





###############################################################################

            ###### Check location of study #######

###############################################################################

#The paper gives coordinates coords


dt$Latitude <- as.numeric(rep('32.6166', nrow(dt)))
dt$Longitude <- as.numeric(rep('-106.75', nrow(dt)))





coords1 <- data.frame(
   Latitude = c(NW = 32.737108, NE = 32.737108, SE = 32.466879, SW = 32.466879),
   Longitude = c(NW = -106.926435, NE = -106.528942, SE = -106.528942, SW = -106.926435)
)



#Check if WSG84 coordinates match study area

world_map <- map_data('world') # check whether the GPS coordinates match expectations

world <- ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   coord_fixed() +
   labs(x='Longitude', y='Latitude') +
   theme_bw() + theme(panel.grid=element_blank())


points <- world + geom_point(data=coords1,
                             aes(x=Longitude, y=Latitude), col="red", shape=21)

world


points + coord_fixed(xlim=c(0,-110), ylim=c(20,60))




# the location seems correct
detach('package:maps')







###############################################################################

      ######## Prepare for export ###########


###############################################################################



# aggregate abundance records in order to be able to estimate the abundance of species for each unique sample



dt_merged <- dt %>% group_by(Abundance, Biomass, Family, Genus,
                                 Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?









###############################################################################

      #### save the dataset name as an object so we save some typing

###############################################################################


# remove variable site as this is not important

dataset_name <- 'christensen_2021'


dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year,Month,Plot,Latitude,Longitude, sep='_')))


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
               ##### Export and spreadsheet prep #####
###############################################################################

write.csv(dt_merged, 'christensen_rawdata_MB.csv', row.names=F, na='') # replace your initials here





###############################################################################

               ##### Study Area #####

###############################################################################


##load libraries
require(sf)
require(rgeos)
require(clipr)


coords1 <- data.frame(
   Latitude = c(NW = 32.737108, NE = 32.737108, SE = 32.466879, SW = 32.466879),
   Longitude = c(NW = -106.926435, NE = -106.528942, SE = -106.528942, SW = -106.926435)
)



# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object

# assumes your rawdata dataframe is called rawdata
dt_coord <- coords1 %>%   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()



# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long
centroid


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



# Plot the geometries -----------------------------------------------------
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
   geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
   geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
   coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your coordinates!
   labs(x = NULL, y = NULL) +
   theme_minimal()
