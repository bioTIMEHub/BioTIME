# Curation Script ---------------------------------------------------------

# Dataset: SEMICE, Common small mammals monitoring in Spain
# Location: Barcelona province (Catalonia, NE Spain)
# Curator: Isaac Trindade Santos & Viviana Brambilla
# Date: 23-Oct-2022

# Set up ------------------------------------------------------------------
# load the necessary packages
library(tidyverse)
library(maps)
library(readxl)

library(ggplot2)
library(stringr)
library(lubridate)

library(sf)
library(clipr)

rm(list=ls()) # clear up the environment before starting

setwd("~/iCloud Drive (Archive)/Documents/BioTIME/SEMICE")
dt <- read_excel('BioTIMEContributorTemplate_ITorre_Final.xlsx', sheet=2, col_names=T, na='')

# Structure check ---------------------------------------------------------

n_distinct(dt$Year) >= 2
# TRUE
dim(dt) # check dimensions
# 609  13 records
str(dt) # check structure

# OBSERVATION - Some entries has abundance but no biomass

no_biomass<- subset(dt, Biomass == "NA")
no_biomass
dim(no_biomass)
#only 7 with no biomass - great!

dt$Biomass <- as.numeric(dt$Biomass)

summary(dt$Biomass)

######################################

dt$Biomass <- as.numeric(dt$Biomass)
dt$Year <- as.factor(dt$Year)
dt$Day <- as.factor(dt$Day)
dt$Month <- as.factor(dt$Month)
dt$Plot <- as.factor(dt$Plot)

unique(dt$Plot)

#[1] Avetosa de Passavets- Montseny           
#[2] Roureda del Turó Gros - Montnegre        
#[3] Franja prevenció nº1 - Collserola        
#[4] Garriga Pla de Querol - Garraf           
#[5] Màquia - Serralada Marina                
#[6] Matollar de ginebró - Montseny           
#[7] Pineda pi blanc - Garraf                 
#[8] Riera de Vallgorguina - Montnegre        
#[9] Pineda Mas de l'Artís                    
#[10] Franja prevenció nº2 - Collserola        
#[11] Màquia La Muntada - Sant Llorenç del Munt
#[12] Màquia Serralada Litoral

table(dt$Plot)
table(dt$Latitude, dt$Longitude)

# each of those plots has the latitude and longitude centroid

summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
min(dt$Abundance) > 0
# [1] TRUE
sum(dt$Abundance=="") > 0
# [1] FALSE


summary(dt$Year)
# 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 
#   31   43   38   48   36   35   39   41   46   44   63   56   42   47 

# Year, month and day must be integers or factors? Y
str(dt)
# TRUE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# TRUE
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# TRUE  #DepthElevation (NUMERIC)
# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------


# YEAR MONTH DAY
unique(dt$Day)
# 31 Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ... 31
unique(dt$Month)
# Levels: 4 5 6 7 8 10 11 12
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

# LAT LONG
min(dt$Latitude)
#[1] 41.26527
max(dt$Latitude)
#[1] 41.77667

min(dt$Longitude)
#[1] 1.69934
max(dt$Longitude)
#[1] 2.575551

# no blanks, no NAs

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[,], aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(35,45))

plot(points_zoom)

# all looks good

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names

sort(unique(dt$Species))

#[1] "Apodemus flavicollis"    "Apodemus sylvaticus"    
#[3] "Clethrionomys glareolus" "Crocidura russula"      
#[5] "Eliomys quercinus"       "Mus spretus"            
#[7] "Rattus rattus"           "Sorex araneus"          
#[9] "Sorex minutus"       

# create copy with Species column
dt$Species1<- dt$Species
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species1, start=2, end=-1)

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that

# [1] "Apodemus"      "Clethrionomys" "Crocidura"     "Eliomys"      
# [5] "Mus"           "Rattus"        "Sorex"        

dt$Species1 <- NULL # get rid of it now that we've split and checked

# check family too
sort(unique(dt$Family))

# [1] "Cricetidae" "Gliridae"   "Muridae"    "Soricidae" 

# Prepare raw data --------------------------------------------------------

dt <- dt %>% arrange(Year, Family, Genus, Species) %>% ungroup()
# now create empty columns needed to fit to template

dt$StudyID <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt$SampleDescription <- as.factor(paste(dt$Plot, dt$Day, dt$Month, dt$Year, sep = "_"))
dt_merged <- dt %>% 
  group_by(Genus,Species,SampleDescription) %>% 
  summarise(Abundance=sum(Abundance), Biomass = sum(Biomass)) %>% 
  ungroup()

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?
# [1] 0

dataset_name <- "SEMICE_Common.small.mammals.monitoring.in.Spain"
length(levels(dt_merged$SampleDescription)) #
#[1] 263


# reorder columns by BioTIME format
dt_merged <- dt[c('Abundance',
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
View(dt_merged) # final check :)
dt_merged$Plot <- NA
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------


# Export final ------------------------------------------------------------

write.csv(dt_merged, 'SEMICE_rawdata_ITS_VB.csv', row.names=F)
write_clip(dt_merged)

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
write_clip(area) # this is just to check against what the author inputted
