# Curation Script ---------------------------------------------------------

# Dataset: Knops_2018a (Arthropod sweepnet sampling: Interactive Effects of Deer, Fire and Nitrogen)
# Location: Cedar Creek
# Contact: Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve - webmaster@cedarcreek.umn.edu
# Curator: James Cant
# Date: 29-Jun-2023

# ------------------------------------------------------------------

# load the necessary packages
library(reshape2)
library(stringr)
require(tidyverse)
require(maps)
require(stringr)
require(dplyr)
require(sf)
require(clipr)

###########################
# STEP 1: Download and open dataset
###########################

# clear workspace
rm(list=ls())

# assign book-keeping details.
data.name <- 'Knops_2018a'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Knops 2018a/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# define file name
infile <- 'E:/BioTime/Knops 2018a/Knops_2018a_OriginalData.txt'
# Run download
if(!dir.exists('E:/BioTime/Knops 2018a/') || !file.exists(infile))   {
  dir.create('E:/BioTime/Knops 2018a/',  showWarnings = FALSE)
  inUrl  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/310/8/03cac5186165dec2523a1b3bbe3b432a"
  download.file(inUrl, infile, method="curl")
}

# Open datafile
dat <-read.csv(infile,header=F
               ,skip=1
               ,sep="\t"
               , col.names=c(
                 "Experiment",
                 "Location",
                 "Plot",
                 "Exclosure",
                 "Fertilized",
                 "Burned",
                 "Date",
                 "Order",
                 "Family(Subfamily)",
                 "Genus",
                 "Specific.epithet",
                 "Further.ID",
                 "Life.stage",
                 "nSpecimens"    ), check.names=TRUE)

###########################
# STEP 2: Clean and reformat data variables
###########################

# Extract control data - the abundance counts involve counts made in artificially modified habitats and so only counts from unmodified plots are required
# Control plots = no fertilizer, no burining, and no deer exclusion
dat <- dat[which(dat$Exclosure == 'n' & dat$Fertilized == 'n' & dat$Burned == 'n'),]

# remove unneeded columns
dat$Experiment <- NULL
dat$Location <- NULL
dat$Fertilized <- NULL
dat$Burned <- NULL
dat$Exclosure <- NULL
dat$Further.ID <- NULL
dat$Life.stage <- NULL

### Clean Abundance (remove NAs) -----------------------
names(dat)[7] <- 'Abundance'
dat <- dat[!(is.na(dat$Abundance)),]
# check to confirm
min(dat$Abundance) > 0 # no zeroes?
sum(dat$Abundance == "" | dat$Abundance == ' ') == 0 # no blanks?
dat$Abundance <- as.numeric(dat$Abundance)

### Clean plot details ---------------------------------
unique(dat$Plot)
dat$Plot <- as.factor(dat$Plot)

### Clean Date info ------------------------------------
dat$Month <- as.factor(substr(dat$Date, 1,1))
dat$Day <- as.factor(substr(dat$Date, 3,4))
dat$Year <- as.factor(substr(dat$Date, 6, nchar(dat$Date)))
dat$Date <- NULL
# No negative values, 0s or NAs, and all are logical
# i.e month < 12, day < 31
summary(dat[c('Year', 'Month', 'Day')])

### Clean Taxonomy -------------------------------------
# Check no family names in the genus variable
dat[which(str_detect(dat$Genus, 'idae$|ini$')),]
# Check and Tidy spelling errors
# Order
sort(unique(dat$Order))
# Family
names(dat)[3] <- 'Family'
sort(unique(dat$Family))
dat$Family[dat$Family == '(Misc.Macrolepidoptera)'] <- 'Macrolepidoptera'
dat$Family[dat$Family == '(Misc.Microlepidoptera)'] <- 'Microlepidoptera'
dat$Family[dat$Family == 'Braconidae (Opiinae)'] <- 'Braconidae'
dat$Family[dat$Family == 'Cicadellidae (Typhlocybinae)'] <- 'Cicadellidae'
dat$Family[dat$Family == 'Ichneumonidae (Anomaloninae)'] <- 'Ichneumonidae'
dat$Family[dat$Family == 'Ichneumonidae (Banchinae)'] <- 'Ichneumonidae'
dat$Family[dat$Family == 'Ichneumonidae (Cremastinae)'] <- 'Ichneumonidae'
dat$Family[dat$Family == 'Ichneumonidae (Gelinae)'] <- 'Ichneumonidae'
dat$Family[dat$Family == 'Ichneumonidae (Porizontinae)'] <- 'Ichneumonidae'
dat$Family[dat$Family == 'Ichneumonidae(Branchinae)'] <- 'Ichneumonidae'
dat$Family[dat$Family == 'Sphecidae (Gorytini)'] <- 'Sphecidae'
dat$Family[dat$Family == 'Sphecidae (Psenini)'] <- 'Sphecidae'
dat$Family[dat$Family == 'mites'] <- NA
dat$Family[dat$Family == 'undet'] <- 'Unknown'
# Condense order into family for instances where family is unknown
dat[which(dat$Family %in% c('Unknown', '') | is.na(dat$Family)),]$Family <- dat[which(dat$Family %in% c('Unknown', '') | is.na(dat$Family)),]$Order
# Genera
sort(unique(dat$Genus))
dat$Genus[dat$Genus == 'Chelonus(s.str.)'] <- 'Chelonus'
dat$Genus[dat$Genus == 'Hylemya(s.str.)'] <- 'Hylemya'
dat$Genus[dat$Genus == 'Dorycephalus?'] <- 'Dorycephalus'
dat$Genus[dat$Genus == 'Gorytes?'] <- 'Gorytes'
dat$Genus[dat$Genus == 'Hybos?'] <- 'Hybos'
dat$Genus[dat$Genus == 'Incertella?'] <- 'Incertella'
dat$Genus[dat$Genus == 'Malloweia?'] <- 'Malloweia'
dat$Genus[dat$Genus == 'Philaenus(Philaenarcys)'] <- 'Philaenus'
dat$Genus[dat$Genus == 'undet'] <- 'Unknown'
# Species
names(dat)[5] <- 'Species'
sort(unique(dat$Species))
dat$Species[dat$Species == 'delta?'] <- 'delta'
dat$Species[dat$Species == 'fimbriolata?'] <- 'fimbriolata'
dat$Species[dat$Species == 'under'] <- 'sp'
dat$Species[dat$Species == 'undet'] <- 'sp'
# recorrect data format
dat$Genus <- as.factor(dat$Genus)
dat$Species <- as.factor(dat$Species)
dat$Family <- as.factor(dat$Family)
dat$Order <- NULL

# ADD SPATIAL INFORMATION ---------------------------------------------------
# No elevation data provided
dat$DepthElevation <- NA
# The study provides a boundary box for the study location so it is nessecary to identify a central GPS location
dt_coords <- data.frame(Longitude = c(-93.22445, -93.16289, -93.22445, -93.16289),
                        Latitude = c(45.44138, 45.44138, 45.384865, 45.384865))
# Convert data points into point spatial object
dt_coord <- dt_coords %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
# add latitude and longitude to data
dat$Longitude <- rep(centroid[1], dim(dat)[1])
dat$Latitude <- rep(centroid[2], dim(dat)[1])

# Check the coordinates match with the expected location of the data
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dat %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-150,-50), ylim=c(20,50))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# PREPARE RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, site, transect, and survey day.
dt_merged <- dat %>% group_by(Latitude, Longitude, DepthElevation, Plot, Year, Month, Day, Family, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Plot, Month, Day, Family, Genus, Species)
nrow(dat) - nrow(dt_merged) # any change in aggregating?
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Site <- rep('Cedar Creek', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Site, Plot, Year, Month, Day, sep='_')))
length(levels(dt_merged$SampleDescription)) # 8 samples (corresponds with the different transects run)

# Remove variables not needed in BIOTIME format
dt_merged$Site <- NULL

# reorder columns to match BioTIME format
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
                         'StudyID')] %>% arrange(Year, Month, Day, Plot, Family, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(fileSave, data.name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_merged)

# -------------------------------------------- End of Code ------------