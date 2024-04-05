# Curation Script ---------------------------------------------------------

# Dataset: Aphid data, Idaho, USA
# Location: Idaho, USA
# Curator: Miguel Barbosa & Bill
# Date: July 2022

# Set up ------------------------------------------------------------------

# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)


install.packages("MASS")
install.packages("reshape2")
install.packages("reshape")
install.packages("dplyr")


library(reshape2)
library(reshape)
library(dbplyr)
library(readxl)
library(stringr)
library(dplyr)

# Data were collected from 1985 to 2001 using suck traps installed 8 m above the ground. 
# the authors summed the abundance of species at the end of each week of sampling
# in total there were 25 sites sampled consistently that recorded 88 species of aphids
# raw data is located https://data.nkn.uidaho.edu/dataset/data-complex-life-histories-predispose-aphids-recent-decline



# Import data

setwd("~/Library/CloudStorage/OneDrive-UniversityofStAndrews/Desktop-Uni/Papers/BioTIME_v2")

getwd()

dt_abundance <- read.csv("Compiled_Trap_Records_Aphid_Idaho.csv")
dt_species <- read.csv('Species_Names_Codes_Info_Aphid_Idaho.csv')
dt_location <- read_excel('Location_Coordinates_Aphid_Idaho.xlsx', sheet=1, col_names=T, na='')





# check data

names(dt_abundance)
names(dt_species)
names(dt_location)


# metadata says there were 25 sites sampled, but there are 27 here.
# i suspect that Parma and Parma40, as well as Tetonia T and Tetonia S are the same sites, hence 25 sites
# to be consistent with the experimental designed explained here https://onlinelibrary.wiley.com/doi/epdf/10.1111/gcb.15739
# I merged Parma & Parma40, and tetonia S & tetonia T, into two sites, Parma and TETONIA

dt_abundance$LOCATION<-as.factor(dt_abundance$LOCATION)

levels(dt_abundance$LOCATION)[levels(dt_abundance$LOCATION)=="TETONIA S"] <- "TETONIA"

levels(dt_abundance$LOCATION)[levels(dt_abundance$LOCATION)=="TETONIA T"] <- "TETONIA"

levels(dt_abundance$LOCATION)[levels(dt_abundance$LOCATION)=="PARMA 40'"] <- "PARMA"


# there are now 25 sites as recorded in the methods of the paper https://onlinelibrary.wiley.com/doi/epdf/10.1111/gcb.15739
unique(dt_abundance$LOCATION)




# latitude and longitudes are in dt_location. I am sure there is an easier way to do this
# but I am doing the long way....
# there is some inconsistency between the location names in the Compiled_Trap_Records.csv and  Location_Coordinates.xlsx
# there is no lat and long for INEEL, MOCCASIN and MORO. On the other hand, there is coordinates for sites that are not in
# the compiled_trap_records (eg. Rockland Valley and Wilder). Further location Rexburg is called REDTOP in compiled_trap_records dataset
# we are just using the sites for which we have coordinates


# need to remove the sites/locations for which we do not have coordinates

 dt_abundance<-dt_abundance[!with(dt_abundance, LOCATION%in%c("INEEL", "MOCCASIN", "MORO")), ]

 #dt_abundance<-dt_abundance%>%filter(LOCATION!="INEEL"|"MOCCASIN"|"MORO") this should also work
 
 

 # setting variables longitude and latitude
 
dt_abundance$Latitude<-""
dt_abundance$Longitude<-""


data.frame(dt_location$LOCATION, dt_location$`Location lat`)

dt_abundance$Latitude[dt_abundance$LOCATION=="PICABO"]<-"43.30641"
dt_abundance$Latitude[dt_abundance$LOCATION=="TETONIA"]<-"43.81406"
dt_abundance$Latitude[dt_abundance$LOCATION=="SODA"]<-"42.65574"
dt_abundance$Latitude[dt_abundance$LOCATION=="ABERDEEN"]<-"42.94442"
dt_abundance$Latitude[dt_abundance$LOCATION=="MOSCOW"]<-"46.73327"
dt_abundance$Latitude[dt_abundance$LOCATION=="HOLBROOK"]<-"42.17307"
dt_abundance$Latitude[dt_abundance$LOCATION=="RIRIE"]<-"43.63235"
dt_abundance$Latitude[dt_abundance$LOCATION=="BONNERS FERRY"]<-"48.69107"
dt_abundance$Latitude[dt_abundance$LOCATION=="NEELEY"]<-"42.73352"
dt_abundance$Latitude[dt_abundance$LOCATION=="KIMBERLY"]<-"42.53490"
dt_abundance$Latitude[dt_abundance$LOCATION=="PARMA"]<-"43.78572"
dt_abundance$Latitude[dt_abundance$LOCATION=="ARBON"]<-"43.81406"
dt_abundance$Latitude[dt_abundance$LOCATION=="PRESTON"]<-"42.09937"
dt_abundance$Latitude[dt_abundance$LOCATION=="INEEL"]<-""
dt_abundance$Latitude[dt_abundance$LOCATION=="CONDA"]<-"42.72955"
dt_abundance$Latitude[dt_abundance$LOCATION=="ROCKLAND"]<-"42.57296"
dt_abundance$Latitude[dt_abundance$LOCATION=="CRAIGMONT"]<-"46.24202"
dt_abundance$Latitude[dt_abundance$LOCATION=="BURLEY"]<-"42.53664"
dt_abundance$Latitude[dt_abundance$LOCATION=="CALDWELL"]<-"43.66837"
dt_abundance$Latitude[dt_abundance$LOCATION=="Mt. HOME"]<-"43.13907"
dt_abundance$Latitude[dt_abundance$LOCATION=="SHELLEY"]<-"43.38472"
dt_abundance$Latitude[dt_abundance$LOCATION=="REDTOP"]<-"43.826849"
dt_abundance$Latitude[dt_abundance$LOCATION=="MOCCASIN"]<-""
dt_abundance$Latitude[dt_abundance$LOCATION=="MORO"]<-""
dt_abundance$Latitude[dt_abundance$LOCATION=="LEWISTON"]<-"46.405078"






# the same lengthy process but this time for Longitude
data.frame(dt_location$LOCATION, dt_location$'Location long')


dt_abundance$Longitude[dt_abundance$LOCATION=="PICABO"]<-"-114.0680"
dt_abundance$Longitude[dt_abundance$LOCATION=="TETONIA"]<-"-111.1590"
dt_abundance$Longitude[dt_abundance$LOCATION=="SODA"]<-"-111.6034"
dt_abundance$Longitude[dt_abundance$LOCATION=="ABERDEEN"]<-"-112.8378"
dt_abundance$Longitude[dt_abundance$LOCATION=="MOSCOW"]<-"-117.0016"
dt_abundance$Longitude[dt_abundance$LOCATION=="HOLBROOK"]<-"-112.6563"
dt_abundance$Longitude[dt_abundance$LOCATION=="RIRIE"]<-"-111.7729"
dt_abundance$Longitude[dt_abundance$LOCATION=="BONNERS FERRY"]<-"-116.3147"
dt_abundance$Longitude[dt_abundance$LOCATION=="NEELEY"]<-"-112.9150"
dt_abundance$Longitude[dt_abundance$LOCATION=="KIMBERLY"]<-"-114.3642"
dt_abundance$Longitude[dt_abundance$LOCATION=="PARMA"]<-"-116.9417"
dt_abundance$Longitude[dt_abundance$LOCATION=="ARBON"]<-"-112.5674"
dt_abundance$Longitude[dt_abundance$LOCATION=="PRESTON"]<-"-111.8767"
dt_abundance$Longitude[dt_abundance$LOCATION=="INEEL"]<-""
dt_abundance$Longitude[dt_abundance$LOCATION=="CONDA"]<-"-111.5325"
dt_abundance$Longitude[dt_abundance$LOCATION=="ROCKLAND"]<-"-112.8749"
dt_abundance$Longitude[dt_abundance$LOCATION=="CRAIGMONT"]<-"-116.4669"
dt_abundance$Longitude[dt_abundance$LOCATION=="BURLEY"]<-"-113.7911"
dt_abundance$Longitude[dt_abundance$LOCATION=="CALDWELL"]<-"-116.6875"
dt_abundance$Longitude[dt_abundance$LOCATION=="Mt. HOME"]<-"-115.6919"
dt_abundance$Longitude[dt_abundance$LOCATION=="SHELLEY"]<-"-112.1234"
dt_abundance$Longitude[dt_abundance$LOCATION=="REDTOP"]<-"-111.7852"
dt_abundance$Longitude[dt_abundance$LOCATION=="MOCCASIN"]<-""
dt_abundance$Longitude[dt_abundance$LOCATION=="MORO"]<-""
dt_abundance$Longitude[dt_abundance$LOCATION=="LEWISTON"]<-"-117.0051"




# merge name species into the dataset
dt <- merge(dt_abundance, dt_species, by = "CODE", all.x = TRUE)

names(dt)


# remove unwanted variables

dt<-dt[-c(1,2,8,9,10,12:21)]




# renaming variable and value

colnames(dt)[2]<-"Year"
colnames(dt)[3]<-"Abundance"
colnames(dt)[6]<-"Species"


names(dt)



# need to split year into day, month and year

dummy<-str_split_fixed(string = dt$Year, pattern = "/", n = 3)

dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Month", "Day", "Year1")


dt$Year<-dummy$Year1
dt$Month<-dummy$Month
dt$Day<-dummy$Day




names(dt) # all good








###############################################################################

                ##### DATASET CRITERIA ########

###############################################################################


# Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

length(unique(dt$Year))

n_distinct(dt$Year) >= 2



# checking if there are Nas and zero abundances

min(dt$Abundance)>0 #FALSE. Need to remove Nas and zero abundances


dt <- dt[!is.na(dt$Abundance),]
dt <- filter(dt, Abundance > 0)


# check

min(dt$Abundance)>0 # all good. zeros and Nas removed



#checking for blanks

sum(dt$Abundance=="") > 0 #FALSE - no blanks




names(dt)#make sure the data has all the necessary fields

dt$Biomass<- ""
dt$Family<-""
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

dt$Abundance<-as.numeric(dt$Abundance)
dt$Plot<-as.factor(dt$Plot)
dt$Latitude<-as.numeric(dt$Latitude)
dt$Longitude<-as.numeric(dt$Longitude)
dt$DepthElevation<-as.numeric(dt$DepthElevation)
dt$Day<-as.factor(dt$Day)
dt$Month<-as.factor(dt$Month)
dt$Year<-as.factor(dt$Year)


str(dt)






###############################################################################

                        ###### Taxonomy #######

###############################################################################

# the paper https://onlinelibrary.wiley.com/doi/epdf/10.1111/gcb.15739 says
# there 88 unique species were recorded. 
# I removed two rows from species names dataset because it contained text
# there are a mismatch between the CODES of dt_species and dt_abundance
# after discussing with Vivi we decided to remove the CODES for which 
# there are no species names allocated. 




length(sort(unique(dt$Species)))  # confirmed

sort(unique(dt$Species))


summary(as.factor(dt$Species))  #135 NAs from the missmatch 

dt <- dt[!is.na(dt$Species),]   # NAs removed


dt$Species[dt$Species == "Macrospihum rosae"] <- "Macrosiphum rosae"   #correct spelling replaced:)






dummy<-str_split_fixed(string = dt$Species, pattern = " ", n = 2)    # split species into genus and species

dummy<-as.data.frame(dummy)

colnames(dummy) <-c("Genus", "Species")

names(dummy)



sort(unique(dummy$Genus))
sort(unique(dummy$Species))




#add the three new variables to dt
dt$Genus<-dummy$Genus
dt$Species<-dummy$Species



sort(unique(dt$Genus))
sort(unique(dt$Species))



str(dt)



###############################################################################

              ###### Check location of study #######

###############################################################################

#The paper gives coordinates decimal degrees (WSG84)

#Check if WSG84 coordinates match study area


library(maps)
library(ggmap)


world_map <- map_data('world')
world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21, size=3, color='orange')

points + coord_fixed(xlim=c(-100,-140), ylim=c(30,55))                     #the location seems correct :)



detach('package:maps')









###############################################################################

                      ######## Prepare for export ###########

###############################################################################


#aggregate abundance records that are same species, survey day and site.

dt_merged <- dt %>% group_by(Abundance, Biomass, Family, Genus,
                                 Species, Plot, Latitude, Longitude,DepthElevation, Year, Day, Month,StudyID, LOCATION) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)


dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?

##### 738 this needs to be double checked! 










###############################################################################

        #### save the dataset name as an object so we save some typing

###############################################################################
dataset_name <- 'Aphiddata_Idaho.csv'


dt_merged$SampleDescription <- as.factor(with(dt_merged, paste (Year,Month, Day, LOCATION, Latitude, Longitude, sep='_')))


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
                         'StudyID',
                         'LOCATION')] %>% arrange(Year, Family, Genus, Species)

head(dt_merged) # final check!





###############################################################################
              #####Export and spreadsheet prep
###############################################################################

write.csv(dt_merged, 'Aphiddata_Idaho_rawdata_MB.csv', row.names=F, na='') # replace your initials here







###############################################################################

                        ##### Study Area #####

###############################################################################


##load libraries
require(sp)
require(rgeos)
require(clipr)
library(sf)



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

