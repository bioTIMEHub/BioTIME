# Curation Script ---------------------------------------------------------

# Dataset: Angiosperms at Serra do Tombador
# Location: Brazil
# Curator: Viviana Brambilla
# Date: 16/02/2024

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

setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Ale-Brazil/Fidelis")
dt1 <- read_excel('BioTIME_Rodrigues-Fidelis_DataSet1.xlsx', sheet=1, col_names=T, na='')
dt2 <- read_excel('BioTIME_Rodrigues-Fidelis_DataSet2.xlsx', sheet=1, col_names=T, na='')
dt <- rbind(dt1,dt2)

# Structure check ---------------------------------------------------------

n_distinct(dt$Year) >= 2
# TRUE
dim(dt) # check dimensions
# 609  13 records
str(dt) # check structure

# OBSERVATION - Some entries has abundance but no biomass

no_biomass<- subset(dt, Biomass == "NA")
no_biomass
summary(dt$Biomass)

######################################

dt$Biomass <- as.numeric(dt$Biomass)
dt$Year <- as.factor(dt$Year)
dt$Day <- as.factor(dt$Day)
dt$Month <- as.factor(dt$Month)
dt$Plot <- as.factor(dt$Plot)

unique(dt$Plot)

table(dt$Plot)
table(dt$Latitude, dt$Longitude)
table(dt$Plot, dt$Year)

# each 10 plots are replicates within latitude and longitude centroid
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
min(dt$Biomass) > 0
# [1] TRUE
sum(dt$Biomass=="") > 0
# [1] FALSE

summary(dt$Year)

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
# no blanks, no NAs

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[,], aes(x=Longitude, y=Latitude), shape=21)
plot(points)

#looks good

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
dt$Species <- word(dt$Species1, start=2, end=2)

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that

dt$Species1 <- NULL # get rid of it now that we've split and checked

# check family too
sort(unique(dt$Family))
dt[dt$Family == "Er",]
dt[dt$Genus == "Crumenaria",]
dt$Family[dt$Family == "Er"] <- "Rhamnaceae"

# [1] "Cricetidae" "Gliridae"   "Muridae"    "Soricidae" 

# Prepare raw data --------------------------------------------------------

dt <- dt %>% arrange(Year, Family, Genus, Species) %>% ungroup()
# now create empty columns needed to fit to template

dt$StudyID <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt$SampleDescription <- as.factor(paste(dt$Plot, dt$Year, sep = "_"))
dt_merged <- dt %>% 
  group_by(Genus,Species,SampleDescription,Abundance,Plot,StudyID, Family,DepthElevation,  Latitude, Longitude, Day, Month,Year) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  ungroup()

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating?
# 7 got merged

dt[duplicated(dt),] %>% View() #only 2 were replicates, the others multiple entries for the same species


dataset_name <- "Angiosperms_at_Serra_do_Tombador"
length(levels(dt_merged$SampleDescription)) #
#[1] 118


# reorder columns by BioTIME format
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
View(dt_merged) # final check :)
dt_merged$Plot <- NA
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------


# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Angiosperms_at_Serra_do_Tombador_rawdata_VB', row.names=F)
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
