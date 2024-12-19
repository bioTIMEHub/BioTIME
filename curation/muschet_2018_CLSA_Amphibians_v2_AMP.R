#Curate the Muschet 2018 dataset:
#Cottonwood Lake Study Area - Invertebrate Counts 1992 - 2015

setwd("C:/Users/apenny3/OneDrive - University of Edinburgh/PapersInPrep/BioTIME_2.0/BioTIME hack/Muschet_2018")
rm(list=ls())

library(tidyverse)
library(readxl)
library(maps)

#This data came from https://github.com/chase-lab/metacommunity_surveys/tree/main/data/raw%20data/muschet_2018

#SPECIES is also a series of alphabetical codes, which will need to be translated.

# load the required packages
# you can tailor your own package preferences and functions used once you get used to curating!
require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)


# download the dataset (adapted from Alban's code)

#download.file(
#  url = "https://www.sciencebase.gov/catalog/file/get/624c7543d34e21f82764df13?name=CLSA_Amphibians1992_2021.csv",
#  destfile = "muschet_2018_CLSA_Amphibians_v2.csv"
#)


# import our data from the Excel spreadsheet
ds <- read.csv("muschet_2018_CLSA_Amphibians_v2.csv")

###
#OVERALL CHECKS
View(ds)

#check dimensions
dim(ds)
str(ds)

###
#standardising sampling (following Alban's code)

#select only surveys which lasted the whole 4 days

ds_copy_days <- ds %>%
  group_by(WETLAND, TRANSECT, YEAR, MONTH) %>%
  mutate(n_days = n_distinct(DAY)) %>%
  ungroup() %>%
  filter(n_days == 4)

# select Only months when all three transects were sampled

ds_copy_days <- ds_copy_days %>%
  group_by(WETLAND, YEAR, MONTH) %>%
  mutate(n_transects = n_distinct(TRANSECT)) %>%
  ungroup() %>%
  filter(n_transects == 3)

#If we use WETLAND_YEAR_MONTH as our sample description, will we have 
#even sampling?

#We should have: even numbers of transects and even numbers of days
ds_copy_days$n_days <- NULL
ds_copy_days$n_transects <- NULL

#check
ds_copy_days <- ds_copy_days %>%
  group_by(WETLAND, YEAR, MONTH) %>%
  mutate(n_days = n_distinct(DAY)) %>%
  mutate(n_transects = n_distinct(TRANSECT)) %>%
  ungroup()

unique(ds_copy_days$n_transects) #3
unique(ds_copy_days$n_days) #4

##even sampling

ds <- ds_copy_days

ds$n_transects <- NULL
ds$n_days <- NULL

rm(ds_copy_days)

summary(ds)

#there is an NA in the COUNT column
check <- ds[which(is.na(ds$COUNT)),] # just remove this record

ds <- ds[which(!is.na(ds$COUNT)),]

rm(check)

#Abundance and biomass should be numeric
is.numeric(ds$COUNT) #TRUE; we don't have biomass

#Coordinates - need to import these
##Adapted from Alban's code:
# Loading coordinates
coords <- read.csv("site locations.csv", skip = 1)
coords$Plot_name <- ifelse(nchar(coords$Plot_name) == 2, paste0(substr(coords$Plot_name, 1, 1), "0", substr(coords$Plot_name, 2, 2)), coords$Plot_name)

#Year, month, day to be integers or factors
#Year
class(ds$YEAR) #integer

class(ds$MONTH) #integer

class(ds$DAY) #integer

#All other columns are character, which is OK for now (come back to coordinates later - 
#the column for sex will be deleted as we will pool individuals within sexes and life stages).

###
##CHECKING PRIMARY FIELDS
#Abundance

min(ds$COUNT) > 0 #FALSE; there are zeroes

length(which(ds$COUNT == 0)) #7071

#check what these instances look like; if they seem to record absences then 
#delete them

check_zeroes <- ds[which(ds$COUNT == 0),]

str(check_zeroes)
View(check_zeroes)

unique(check_zeroes$SPECIES) # "AMMA" "NONE" "LOST" "RAPI" "RASY" - these aren't all indicative of absent or lost taxa.

#the metadata explains that "LOST" means the amphibian was lost from the trap before identification
#"NONE" = no amphibians present in trap
#"if a species was recorded, counts were greater than 0"

table(check_zeroes$SPECIES)

#AMMA LOST NONE RAPI RASY 
#3    1 7590    2    1 

#a very small number of records with a species name but COUNT = 0; I think we can safely delete these

#I don't see anything to suggest these aren't absences - so I'll remove them.
ds <- ds[which(ds$COUNT > 0),]

rm(check_zeroes)

min(ds$COUNT) > 0 #TRUE

sum(ds$COUNT=="") > 0 #FALSE, no blanks

#check YEAR, MONTH, DAY are all reasonable numbers
range(ds$YEAR) #1992, 2021
range(ds$MONTH) #5, 9
range(ds$DAY) #1, 4

#Pool the sexes
unique(ds$SEX) #F, M, NA, U - U means 'Undetermined'; NA means 'not applicable', i.e. when no amphibians were captured (SPECIES == "NONE")

check_sex <- ds[which(is.na(ds$SEX)),]

##Two records, both with SPECIES == "NONE" but COUNT == 1; delete these
ds <- ds[which(!is.na(ds$SEX)),]

rm(check_sex)

#do we want to also pool data from different ontogenies?
unique(ds$STAGE) #"A"   "L"   "N"   "YOY" 

#A = Adult; L = larval; N = neotenic; YOY = young of the year; NA = not applicable (i.e. no amphibians in trap)

#pool the different ontogenetic stages as well (all individuals were collected
#using the same methods)

#We can pool data from different sexes using dplyr as per workshop notes
ds <- ds %>%
  group_by(WETLAND, TRANSECT, SPECIES, MONTH, DAY, YEAR) %>% #Group by everything except SEX, STAGE and COUNT
  summarise(Abundance = sum(COUNT)) %>% ungroup() #this also renames COUNT to ABUNDANCE

###
#Assign latitude and longitude, and check that these are as expected.
ds_coords <- left_join(x = ds, y = coords, by = join_by(WETLAND == Plot_name))

#check the lat-longs
range(ds_coords$Latitude) #47.0957 47.1093
range(ds_coords$Longitude) #-99.1464 -99.0912

#Make a map
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=ds_coords, aes(x=Longitude, y=Latitude), shape=21)

points + coord_fixed(xlim=c(-140, -80), ylim=c(35,60))
detach('package:maps')

#Correct - all points in North Dakota


###
#SECONDARY FIELDS

#check that these don't have missing values
unique(ds_coords$WETLAND)
unique(ds_coords$TRANSECT) 

#we don't have PLOT or depth/elevation info here so never mind these

###
#taxonomic fields

#first translate the code for each species into a taxonomic name
#just do this manually based on the metadata rather than trying to extract it - 
#we have very few species here

taxonomy <- data.frame(species = c(
  "Ambystoma mavortium", "Chrysemys picta", "Lithobates tadpole",
  "Pseudacris maculata", "Lithobates pipiens", "Lithobates sylvaticus",
  "Thamnophis radix"), code = c("AMMA", "CHPI", "FROTAD", "PSMA", "RAPI", "RASY", "THRA"))

#Match these to the codes
ds_coords <- left_join(x = ds_coords, y = taxonomy, by = join_by(SPECIES == code))

#remove column SPECIES
ds_coords$SPECIES <- NULL

#check for blanks, NAs and zeroes
unique(ds_coords$species) #Nothing here, and we have already removed the species codes which denote absence

#For uncertain IDs, "Lithobates tadpole" should be "Lithobates sp"
ds_coords$species[which(ds_coords$species == "Lithobates tadpole")] <- "Lithobates sp"

#Create Family, Genus, Species columns
#we don't have any Family data

ds_coords$Family <- rep('', nrow(ds_coords))

#split the species column into Genus and Species at the space
ds_coords$Taxon <- ds_coords$species

ds_coords <- ds_coords %>%
  separate(col = Taxon, into = c("Genus", "Species"), sep = " ", remove = FALSE)

#Now we have the taxonomic cols

#no non-organismal records
#No family names in the genus column
#no mis-spellings
#no subspecies or annotated species names
#sp in the species column when needed

#remove the extra taxonomic cols
ds_coords$species <- NULL
ds_coords$Taxon <- NULL

###
#prepare for export
 
#correct column names and add empty columns
ds_coords$Biomass <- rep('', nrow(ds_coords))
ds_coords$Plot <- rep('', nrow(ds_coords))
ds_coords$DepthElevation <- rep('', nrow(ds_coords))
#day and month we have
ds_coords$StudyID <- rep('', nrow(ds_coords))
#add a new 'Day' column for dates
ds_coords$Day <- ""

#Make sure the column names are in BioTIME input format
colnames(ds_coords)[which(colnames(ds_coords) == "YEAR")] <- "Year"
colnames(ds_coords)[which(colnames(ds_coords) == "MONTH")] <- "Month"

#create a sample description column
dt_merged <- ds_coords %>%
  unite(col = SampleDescription, WETLAND, Year, Month, sep = "_", remove = FALSE) #left out latitude and longitude as there is only one per wetland; left out DAY as it isn't a date

#remove WETLAND and TRANSECT and DAY - no longer needed
dt_merged$WETLAND <- NULL
dt_merged$TRANSECT <- NULL
dt_merged$DAY <- NULL

# aggregate abundance records that are same species, plot, and month (survey days here aren't dates)
dt_merged <- dt_merged %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

n_distinct(dt_merged$SampleDescription) #1375 sampling events


#Organise columns
dt_merged <- dt_merged[,c("Abundance", "Biomass", "Family", "Genus", "Species", 
                     "SampleDescription", "Plot", "Latitude", "Longitude",
                     "DepthElevation", "Day", "Month", "Year", "StudyID")] %>% arrange(Year, Family, Genus, Species)

head(dt_merged)

#write data
#write.csv(dt_merged, "muschet_2018_CLSA_Amphibians_v2_AMP_rawdata.csv", row.names=F, na='')


###
#calculate convex hull
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

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim=c(-99.2, -99.05), ylim=c(47.05,47.15)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()

#looks good!







