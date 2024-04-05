# Curation Script ---------------------------------------------------------

# Dataset: Knops_2018b (Small Mammal Abundance: Interactive Effects of Deer, Fire and Nitrogen)
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
data.name <- 'Knops_2018b'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Knops 2018b/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# define file name
infile <- 'E:/BioTime/Knops 2018b/Knops_2018b_OriginalData.txt'
# Run download
if(!dir.exists('E:/BioTime/Knops 2018b/') || !file.exists(infile))   {
  dir.create('E:/BioTime/Knops 2018b/',  showWarnings = FALSE)
  inUrl  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/317/8/854936b5b38258e6fd02f2639e9dfa43"
  download.file(inUrl, infile, method="curl")
}

# Open datafile
dat <-read.csv(infile, header=F
               ,skip=1
               ,sep="\t"
               , col.names=c(
                 "Year",
                 "Plot",
                 "Fencing",
                 "Fertilization",    # modified
                 "Burning",   # modified
                 "Trapnights",
                 "Microtus_pennsylvanicus",
                 "Peromyscus_maniculatus",
                 "Undet_Soricidae.Shrew.",   # modified
                 "Zapus_hudsonicus"    ), check.names=TRUE)

###########################
# STEP 2: Clean and reformat data variables
###########################

# Extract control data - the abundance counts involve counts made in artificially modified habitats and so only counts from unmodified plots are required
# Control plots = no fertilizer, no burning, and no deer exclusion
dat <- dat[which(dat$Fencing == 0 & dat$Fertilization == 0 & dat$Burning == 0),]

# remove unneeded columns
dat$Burning <- NULL
dat$Fertilization <- NULL
dat$Fencing <- NULL
dat$Trapnights <- NULL

### Reshape data format --------------------------------
dat <- melt(dat, id.vars = c("Year", "Plot"),
            variable.name = 'MammalSp', value.name = 'Abundance')

### Clean Abundance (remove NAs) -----------------------
dat <- dat[!(is.na(dat$Abundance)),]
# check to confirm
min(dat$Abundance) > 0 # no zeroes?
dat <- dat[dat$Abundance > 0,] # remove zero entries
sum(dat$Abundance == "" | dat$Abundance == ' ') == 0 # no blanks?
dat$Abundance <- as.numeric(dat$Abundance)

### Clean plot details ---------------------------------
unique(dat$Plot)
dat$Plot <- as.factor(dat$Plot)

### Clean Date info ------------------------------------
dat$Year <- as.factor(dat$Year)

### Clean Taxonomy -------------------------------------
# Split taxonomic variable into genus and species
levels(dat$MammalSp)[levels(dat$MammalSp) == 'Microtus_pennsylvanicus'] <- 'NA Microtus pennsylvanicus'
levels(dat$MammalSp)[levels(dat$MammalSp) == 'Peromyscus_maniculatus'] <- 'NA Peromyscus maniculatus'
levels(dat$MammalSp)[levels(dat$MammalSp) == 'Undet_Soricidae.Shrew.'] <- 'Soricidae Unknown sp'
levels(dat$MammalSp)[levels(dat$MammalSp) == 'Zapus_hudsonicus'] <- 'NA Zapus hudsonicus'
dat$Family <- word(dat$MammalSp, 1)
dat$Genus <- word(dat$MammalSp, 2)
dat$Species <- word(dat$MammalSp, 3)
dat$MammalSp <- NULL
# recorrect data format
dat$Genus <- as.factor(dat$Genus)
dat$Species <- as.factor(dat$Species)
dat$Family <- as.factor(dat$Family)

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
dt_merged <- dat %>% group_by(Latitude, Longitude, DepthElevation, Plot, Year, Family, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Plot, Family, Genus, Species)
nrow(dat) - nrow(dt_merged) # any change in aggregating?
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Month <- rep(NA, nrow(dt_merged))
dt_merged$Day <- rep(NA, nrow(dt_merged))
dt_merged$Site <- rep('Cedar Creek', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Site, Plot, Year, sep='_')))
length(levels(dt_merged$SampleDescription)) # 8 samples (corresponds with the different transects run)

# Remove variables not needed within BIOTIME format
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