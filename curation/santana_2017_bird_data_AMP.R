#Data curation for santana 2017 dataset

setwd("C:/Users/apenny3/OneDrive - University of Edinburgh/PapersInPrep/BioTIME_2.0/BioTIME hack/santana_2017")

#load packages
require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)

#read in data from the original publication
ds <- read.csv("doi_10.5061_dryad.kp3fv__v2/bird_data.csv")

View(ds)


###
#check data characteristics
#1 - two years of data?

n_distinct(ds$Year) #6
unique(ds$Year) #1995 1996 1997 2010 2011 2012

# -> yes

#2 - Whole assemblages, not just populations?
# data description describes this as: Total number of individuals of each bird species recorded along each 250-m transect per year

# -> yes

#3 - Data should record abundance, biomass or both

# -> yes

#4 - Sampling methods and effort are consitent through time
 # -> yes, transects taken along a random orientation and sampled using a consistent methods.

#5 - #Individuals identified to species where possible

# -> yes


###
#OVERALL CHECKS

dim(ds)

str(ds) # Looks good

#Convert wide format data to long format

ds <- data.table::melt(ds,
                       id.vars = 1:6,
                       variable.name = "species",
                       na.rm = TRUE
)

str(ds)

#delete rows where value = 0
ds <- ds %>%
  filter(value != 0)

#Rename value to Abundance
colnames(ds)[which(colnames(ds) == "value")] <- "Abundance"

#test variable classes
is.numeric(ds$Abundance)

#Add transect locations and make sure they're numeric

locs <- read.csv("doi_10.5061_dryad.kp3fv__v2/transects_information.csv")

ds <- left_join(ds, locs, by = "Transect")

is.numeric(ds$Latitude)
is.numeric(ds$Latitude)

#Convert date format to year-month-day
unique(ds$Date)

#All but one of the dates has year, month and day - but one only has month and 
#day.

#So split the Date column at the second space, to make month and year cols.
#then split again to make day and month cols.

#We have some records which have no day info, only month and year. Assign these 
#NA as a Day

test <- ds

test$DateCopy <- test$Date

test$Date[which(test$Date == "April 1996")] <- "NA April 1996"

test <- test %>%
  separate(col = Date, into = c("Day", "Month", "Year"),
           sep = " ")

#check those cols
ds <- test

#Check Day
is.integer(ds$Day)
ds$Day <- as.integer(ds$Day)

unique(ds$Day)
summary(ds$Day)

#check the day 31 records 
test<- ds %>%
  filter(Day == 31)

unique(test$Month)  #March and May, as expected

rm(test)

#check Month
is.integer(ds$Month)

ds$Month[which(ds$Month == "March")] <- 3
ds$Month[which(ds$Month == "April")] <- 4
ds$Month[which(ds$Month == "May")] <- 5
ds$Month[which(ds$Month == "June")] <- 6

unique(ds$Month) #5, 4, 3, 6
ds$Month <- as.integer(ds$Month)

unique(ds$Month)

#check Year
is.integer(ds$Year)

ds$Year <- as.integer(ds$Year)

unique(ds$Year)

#Plot?
#We don't have Plot here - the transects aren't fixed from year to year

#DepthElevation - we don't have this either

#Taxonomic
is.character(ds$species) #FALSE
is.factor(ds$species) #TRUE

###
#PRIMARY FIELDS

min(ds$Abundance) > 0
sum(ds$Abundance == " ") > 0
summary(ds$Year)
length(which(is.na(ds$Abundance)))

unique(ds$species)

#No separation of taxa by sex or ontogeny

#check lat-longs
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=ds %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(-12, 0), ylim=c(35, 45))
detach('package:maps')

###
#SECONDARY FIELDS
#not applicable

###
#TAXONOMIC FIELDS

sort(unique(ds$species))

#No mis-spellings or NAs or blanks
#No uncertain IDs either.
#No non-organismal records

#fit taxonomic names into the BiOTIME format
ds$taxa <- ds$species

ds <- ds %>%
  separate(col = taxa, into = c("Genus", "Species"))

ds$Family <- ""

head(ds)

unique(ds$Species)
 
#remove the old species column
ds$species <- NULL

###
#Prepare for export

#Remove extra Farmland_area columm
ds$Farmland_area.y <- NULL

ds$DateCopy <- NULL

#Create sample description
ds <- ds %>%
  unite(col = SampleDescription, Transect, Farmland_area.x, Day, Month, Year, Recorder, Latitude, Longitude, sep = "_", remove = FALSE)

#Do we always have one observer per transect-day?
check <- ds %>%
  group_by(Transect, Farmland_area.x, Day, Month, Year, Latitude, Longitude) %>%
  mutate(n_recorders = n_distinct(Recorder))

unique(check$n_recorders) #1

#Remove columns we no longer want
ds$Rowname <- NULL
ds$Transect <- NULL
ds$Farmland_area.x <- NULL
ds$Recorder <- NULL

head(ds)

#Pool species within sampling events
dt_merged <- ds %>%
  group_by_at(vars(-Abundance)) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup() %>%
  arrange(Year, Family, Genus, Species)

#Add columns we also need
dt_merged$Biomass <- ''
dt_merged$Plot <- ''
dt_merged$DepthElevation <- ''
dt_merged$StudyID <- ''

#Rearrange to correct column order
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

#write.csv(dt_merged, 'santana2017_bird_data_AMP.csv', row.names=F, na='') # replace your initials here

#Getting spatial information for spreadsheet
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

