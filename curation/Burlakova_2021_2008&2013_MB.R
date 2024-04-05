# Curation Script ---------------------------------------------------------

# Dataset: Burlakova_2021_2008&2013
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
# comparable years (ie. same sampling methods). This  one is for sampling years 2008 and 2013.
# invertebrates were sampled using a Ponar sampler, the area sampled was 0.048 m2 and specimens were sorted through a 500um.
# there were 51 and 45 stations in 2008 and 2013 respectively. In 2008 at each station, three replicate samples were collected
# but these were pooled into a single sample. We need to pool the abundances of the 3

# NOTE - after examining the data on S1 Metadata, the abundances of the 3 replicates were pooled


# I will import all the data and later subset the years

d<-read.csv("burlakova_2021.csv", header=T)



names(d)
head(d, n=10)
dim(d)
str(d)

d<-d[,-1]

#sub-setting data - we only want years 1995 and 1990

d_new<-subset(d, Year=="2008" | Year== "2013")

unique(d_new$Year)


# the paper Burlakova et al 2021 says "Forty-five stations were sampled.... At each station, samples were collected in triplicate 
# with a Ponar grab (0.048 m2), washed through a 500-μm net, and preserved separately in 5 % buffered

# I checked for duplicates in each station, but this returned 0, which means 2013 abundances were also pooled.
# this needs to be further investigated.

sum(duplicated(d$Station[d$Year==2013])) # 0 which means no duplicates







###############################################################################

              ##### changing wide to long format ########

###############################################################################

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






###############################################################################

                  ##### DATASET CRITERIA ########

###############################################################################


# Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(d_long$Year))

n_distinct(d_long$Year) >= 2



# checking if there are zero abundances

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









###############################################################################

                    ###### check structure #######

###############################################################################

str(d_long)

d_long$Site<-as.factor(d_long$Site)
d_long$Plot<-as.factor(d_long$Plot)
d_long$DepthElevation<-as.numeric(d_long$DepthElevation)
d_long$Day<-as.numeric(d_long$Day)
d_long$Month<-as.numeric(d_long$Month)


str(d_long)






###############################################################################

                            ###### Taxonomy #######

###############################################################################


#At the moment there is one variable called Species with
#genus and species together. We need to create two new variables
# Genus and Species


sort(unique(d_long$Species))

duplicated(d$Species) # 0 which means no duplicates


# split genus+species
# lets split the variable "species" into genus and species
# some cells have two objects separated by _ hence we need to
# tell R to split into 3 variables
# note that I am adding 3 variables because some of the names should go on a Family variable




dummy<-str_split_fixed(string = d_long$Species, pattern = "\\_", n = 3)


dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Genus1", "Species1", "Family1")

names(dummy)


unique(dummy$Species1) # they all seem to be species names


unique(dummy$Genus1) # there are a few families/Order and class in this list
# these need to be moved to a Family variable


# the below line of code searches words that end on "idae", "eae", ... and copy them to
# Family1 variable on dummy



dummy$Family1[str_which(dummy$Genus1,'idae$|eae$')]<-dummy$Genus1[str_which(dummy$Genus1,'idae$|eae$')]

dummy$Family1[str_which(dummy$Genus1,'oda$|inae$')]<-dummy$Genus1[str_which(dummy$Genus1,'oda$|inae$')]

dummy$Family1[str_which(dummy$Genus1,'eia$')]<-dummy$Genus1[str_which(dummy$Genus1,'eia$')]

dummy$Family1[str_which(dummy$Genus1,'tea$|aeta$')]<-dummy$Genus1[str_which(dummy$Genus1,'tea$|aeta$')]

unique(dummy$Family1)


# delete the "words" from genus that were copied into Family
dummy$Genus1[dummy$Genus1 == 'Caecidotea'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Enchytraeidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Gastropoda'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Lumbriculidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Naididae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Nemertea'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Sphaeriidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Tubificidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Chironomidae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Orthocladiinae'] <- 'NA'
dummy$Genus1[dummy$Genus1 == 'Diporeia'] <- 'NA'



# replacing "" with NA

dummy$Genus1[dummy$Genus1==""]<-"NA"
dummy$Family[dummy$Family1==""]<-"NA"




#add the three new variables to d_long
d_long$Genus1<-dummy$Genus1
d_long$Species1<-dummy$Species1
d_long$Family1<-dummy$Family1



#delete species, family and genus variables

d_long<-d_long[,-c(6,9,16)]

names(d_long)

#rename Species1, Family1 & Genus1

colnames(d_long)[14] <- "Genus"
colnames(d_long)[15] <- "Species"
colnames(d_long)[16] <- "Family"


names(d_long) # all seems good






###############################################################################

              ###### Check location of study #######

###############################################################################

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







###############################################################################

              ######## Prepare for export ###########

###############################################################################


#aggregate abundance records that are same species, survey day and site.

dt_merged <- d_long %>% group_by(Abundance, Biomass, Family, Genus,
                                 Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID, Site) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(d_long)[1]-dim(dt_merged)[1] # any change in aggregating?

##### this needs to be double checked! 







###############################################################################

        #### save the dataset name as an object so we save some typing

###############################################################################
dataset_name <- 'Burlakova_2021'


dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name,Year,Site, Latitude, Longitude, sep='_')))


length(levels(dt_merged$SampleDescription)) # how many sampling events?

# in 2008 there were 51 sampling events and in 2013 45 = 96




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





###############################################################################
        #####Export and spreadsheet prep
###############################################################################

write.csv(dt_merged, 'Burlakova_2021_2008&2013_rawdata_MB.csv', row.names=F, na='') # replace your initials here





###############################################################################

                          ##### Study Area #####

###############################################################################


##load libraries
require(sp)
require(rgeos)
require(clipr)

#  handy line to copy the raw data directly to your clipboard for pasting into Excel :)
clipr::write_clip(dt_merged)

##1. Convert data points into a SpatialPoints object
# this also transforms it from WGS84 coordinate reference to a mercator projection in km for calculating area in sq km
points204<- SpatialPoints(cbind(dt_merged$Longitude,dt_merged$Latitude), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  sp::spTransform(., CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))

##2. Calculate convex hull, area and centroid
convhull204<-gConvexHull(points204)
clipr::write_clip(gArea(convhull204))  ## get area of the convex hull and copy to clipboard

### get centroid
centroid204<-gCentroid(convhull204)

##to get the coordinates
centroid204@coords
clipr::write_clip((centroid204@coords[c(2,1)])) ## get convex hull centroid and copy to clipboard in Lat,Long
