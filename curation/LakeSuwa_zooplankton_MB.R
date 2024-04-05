#Curation Script ----------------------------------------------------------

# Data set: Long-term zooplankton community records (1996-2017) 
# Location: Lake Suwa, Japan
# Curator: Miguel Barbosa & James Johnson
# Date: July 2022

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

# this dataset recorded the abundance of zooplankton in a Japanese lake
# data was collected on rotifers, cladocerans and copepods
# however, only Rotifers and cladocerans were identified to species/genus level
# copepods were identified to order, which is not enough resolution to be included on BioTIME
# two datasets (ie rotifers and for cladocerans)

# data was collected in a bi-weekly frequency for all years except
# 2001, 2007 and 2010. These years are excluded from data curation
# nevertheless these data is still available via raw data files





dr<-read.csv("ERDP-2017-06.2.1-Suwa_rotifers.csv", header=T)
dc<-read.csv("ERDP-2017-06.3.1-Suwa_cladocerans.csv", header=T)


names(dr)
names(dc)

head(dr, n=10)
head(dc, n=10)

dim(dr)
dim(dc)



str(d)



# merge name species into a single dataset
dt <- merge(dr, dc, by = "Date", all.x = TRUE)

names(dt)  # checked all good
head(dt, n=2)
view(dt)


# removing Year.x

dt<-dt[,-2]



# need to split Date into day, month and year

dummy<-str_split_fixed(string = dt$Date, pattern = "/", n = 3)

dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Day", "Month", "Year")

view(dummy) # all good


dt$Year<-dummy$Year
dt$Month<-dummy$Month
dt$Day<-dummy$Day




names(dt) # all good
view(dt)


# removing variable Date

dt<-dt[,-1]



# 2001, 2007 and 2011 were sampled bi-weekly or monthly, whereas the rest of the years sampling 
# occurred on a bi-weekly mode. I am thus removing 2001, 2007 and 2011 data from the rest of the curation.
# all data will still be available via raw data files



dt<-dt[!with(dt, Year%in%c("2001", "2007", "2011")), ]

sort(unique(dt$Year)) # all good









###############################################################################

          ##### changing wide to long format ########

###############################################################################

## species are in columns - lets change from wide to long
#using melt() to go from wide to long

names(d_new)

dt_long <- melt(dt, id = c("Year","Day", "Month"),
               variable.name="Species", value.name="Abundance")



names(dt_long)


# Changing the name of variables. Not sure why melt function did not rename variables and values

colnames(dt_long)[4] <- "Species"
colnames(dt_long)[5] <- "Abundance"

names (dt_long) # all good now




#add coordinates, the paper gives values of 36°2′N, 138°5′E
#use this link to transform coordinates into WSG84
#https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html


dt_long$Latitude <- as.numeric(rep('36.033333', nrow(dt_long)))
dt_long$Longitude <- as.numeric(rep('138.083333', nrow(dt_long)))





###############################################################################

              ##### DATASET CRITERIA ########

###############################################################################


# Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(dt_long$Year))

n_distinct(dt_long$Year) >= 2



# checking if there are Nas and zero abundances

min(dt$Abundance)>0 #FALSE. Need to remove Nas and zero abundances


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
dt_long$Plot<-""
dt_long$DepthElevation<-""
dt_long$Genus<-""




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

# the file ERDP-2017-06.5.1-Suwa_list contains 43 species (9 cladocerans,31 rotifers and 3 copepods)
# after removing copepods we should have 40 species, but instead we have 41....
# found the issue. One species name was called Year.y. I guess this occurred when I merged the
# two datasets

# removing Year.y from species


dt_long<-dt_long[!with(dt_long, Species%in%c("Year.y")), ]


length(sort(unique(dt_long$Species)))  # 40, all good!

sort(unique(dt_long$Species))



dummy2<-str_split_fixed(string = dt_long$Species, pattern = "\\.", n = 3)    # split species into genus and species

dummy2<-as.data.frame(dummy2)

colnames(dummy2) <-c("Genus", "Species", "NA")

names(dummy2)

view(dummy2)

dummy2$Species[dummy2$Species=="spp"]<-"sp" # change spp for sp


unique(dummy2$Genus)
unique(dummy$Species)


str_which(dummy2$Genus, 'idae$|eae$') # all good
str_which(dummy2$Species, 'idae$|eae$') # all good


#add the new variables to dt_long
dt_long$Genus<-dummy2$Genus
dt_long$Species<-dummy2$Species


sort(unique(dt_long$Genus))
sort(unique(dt_long$Species))



str(dt_long)

dt_long$Species<-as.factor(dt_long$Species)
dt_long$Genus<-as.factor(dt_long$genus)






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

points + coord_fixed(xlim=c(100,140), ylim=c(30,55))                     #the location seems correct :)



detach('package:maps')




###############################################################################

                ######## Prepare for export ###########

###############################################################################


#aggregate abundance records that are same species, survey day and site.

dt_merged <- dt_long %>% group_by(Abundance, Biomass, Family, Genus,
                             Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(dt_long)[1]-dim(dt_merged)[1] # any change in aggregating?

##### 0 seems correct 





###############################################################################

        #### save the dataset name as an object so we save some typing

###############################################################################
dataset_name <- 'Long-term zooplankton community records(1996-2017).csv'


dt_merged$SampleDescription <- as.factor(with(dt_merged, paste (Year,Month, Day, Latitude, Longitude, sep='_')))


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

write.csv(dt_merged, "Long-term zooplankton community records(1996-2017)_MB.csv") #written csv into working directory 

 


###############################################################################

                      ##### Study Area #####

###############################################################################


# it returns 0 Km2 for area of sampling, which is not corrrect. In the methods, it says that the lake occupies a 13.3 Km2 area

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


