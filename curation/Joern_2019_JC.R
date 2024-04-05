# Curation Script ---------------------------------------------------------

# Dataset: Joern_2019 (Sweep Sampling of Grasshoppers on Konza Prairie LTER watersheds)
# Location: Konza Prairie
# Contact: knzlter@ksu.edu
# Curator: James Cant
# Date: 28-Jun-2023

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
data.name <- 'Joern_2019'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Joern 2019/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# define file name
infile <- 'E:/BioTime/Joern 2019/Joern_2019_OriginalData.csv'
# Run download
if(!dir.exists('E:/BioTime/Joern 2019/') || !file.exists(infile))   {
  dir.create('E:/BioTime/Joern 2019/',  showWarnings = FALSE)
  inUrl  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/29/14/3fb352e2478f776517f7e880fe31b808"
  download.file(inUrl, infile, method="curl")
}

# Open datafile
dat <-read.csv(infile)

###########################
# STEP 2: Clean and reformat data variables
###########################
# Fix any columns read in the wrong format
if (class(dat$DATACODE)!="factor") dat$DATACODE<- as.factor(dat$DATACODE)
if (class(dat$RECTYPE)!="factor") dat$RECTYPE<- as.factor(dat$RECTYPE)
if (class(dat$RECYEAR)!="factor") dat$RECYEAR <- as.factor(dat$RECYEAR)
if (class(dat$RECDAY)!="factor") dat$RECDAY <- as.factor(dat$RECDAY)
if (class(dat$RECMONTH)!="factor") dat$RECMONTH <- as.factor(dat$RECMONTH)
if (class(dat$WATERSHED)!="factor") dat$WATERSHED <- as.factor(dat$WATERSHED)
if (class(dat$SOILTYPE)!="factor") dat$SOILTYPE<- as.factor(dat$SOILTYPE)
if (class(dat$REPSITE)!="factor") dat$REPSITE<- as.factor(dat$REPSITE)
if (class(dat$SPCODE)!="factor") dat$SPCODE<- as.factor(dat$SPCODE)
if (class(dat$SPECIES)!="factor") dat$SPECIES<- as.factor(dat$SPECIES)
if (class(dat$S1)=='character') dat$S1 <- as.numeric(dat$S1)
if (class(dat$S2)=='integer') dat$S2 <- as.numeric(dat$S2)
if (class(dat$S3)=='integer') dat$S3 <- as.numeric(dat$S3)
if (class(dat$S4)=='integer') dat$S4 <- as.numeric(dat$S4)
if (class(dat$S5)=='integer') dat$S5 <- as.numeric(dat$S5)
if (class(dat$S6)=='integer') dat$S6 <- as.numeric(dat$S6)
if (class(dat$S7)=='integer') dat$S7 <- as.numeric(dat$S7)
if (class(dat$S8)=='integer') dat$S8 <- as.numeric(dat$S8)
if (class(dat$S9)=='integer') dat$S9 <- as.numeric(dat$S9)
if (class(dat$S10)=='integer') dat$S10 <- as.numeric(dat$S10)

# remove unneeded columns
dat$TOTAL <- NULL
dat$COMMENTS <- NULL
dat$DATACODE <- NULL
dat$RECTYPE <- NULL
dat$SPCODE <- NULL

### Reshape data format --------------------------------
dat <- melt(dat, id.vars = c("RECYEAR", "RECMONTH", "RECDAY", "WATERSHED", "SOILTYPE", "REPSITE", "SPECIES"),
            variable.name = 'Sample', value.name = 'Abundance')

### Clean Abundance (remove NAs) -----------------------
dat <- dat[!(is.na(dat$Abundance)),]
# check to confirm
min(dat$Abundance) > 0 # no zeroes?
dat <- dat[dat$Abundance > 0,] # remove zero entries
sum(dat$Abundance == "" | dat$Abundance == ' ') == 0 # no blanks?

### Clean site details ---------------------------------
names(dat)[4] <- 'Site'
names(dat)[5] <- 'Soiltype'
names(dat)[6] <- 'Repsite'
# Ensure consistency in site naming
unique(dat$Site)
unique(dat$Repsite)
unique(dat$Soiltype)
dat$Site <- toupper(dat$Site)
dat$Site <- as.factor(dat$Site)

### Clean Date info ------------------------------------
# Ensure date variables are factors
dat$Year <- as.factor(dat$RECYEAR); dat$RECYEAR <- NULL
dat$Month <- as.factor(dat$RECMONTH); dat$RECMONTH <- NULL
dat$Day <- as.factor(dat$RECDAY); dat$RECDAY <- NULL
# No negative values, 0s or NAs, and all are logical
# i.e month < 12, day < 31
summary(dat[c('Year', 'Month', 'Day')])
# Remove all data from prior to 2013 as this is not a full assemblages assessment. 
# The metadata declares that after 2013 three new species were added to the surveys (but state that these species where not new to the area)
summary(dat$Year)
dat <- dat[dat$Year %in% c('2013','2014','2015','2016','2017'),]

### Clean Taxonomy -------------------------------------
# Split taxonomic variable into genus and species
dat$Genus <- word(dat$SPECIES, 1)
dat$Species <- word(dat$SPECIES, 2)
# Insert Family variable and relocate any inappropriate genus entries
dat$Family <- dat$Genus
dat[str_which(dat$Family, 'idae$|eae$', negate = T),]$Family <- NA
dat[!(is.na(dat$Family)),c('Genus','Species')] <- NA
# Tidy spelling errors
dat$Genus <- str_to_sentence(dat$Genus)
# Check species
sort(unique(dat$Species))
dat$Species[dat$Species == 'spp.'] <- 'sp'
# Check genera
sort(unique(dat$Genus))
# Check family
sort(unique(dat$Family))
# Remove old taxonomy column
dat$SPECIES <- NULL
# change data format
dat$Genus <- as.factor(dat$Genus)
dat$Family <- as.factor(dat$Family)
dat$Species <- as.factor(dat$Species)

# Add and confirm LAT & LONGs and Elevation
dat$Latitude <- rep(39.093, dim(dat)[1])
dat$Longitude <- rep(-96.575, dim(dat)[1])
dat$DepthElevation <- rep(382, dim(dat)[1])
# check coordinates are in numeric format
is.numeric(dat$Latitude); is.numeric(dat$Longitude) 
sum(dat[c('Latitude', 'Longitude')] == "") == 0 # no blanks?
str_detect(dat %>% pull(Latitude, Longitude), '\\s') %>% sum() == 0 # no spaces?
sum(is.na(dat[c('Latitude', 'Longitude')])) == 0 # no NAs?

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
dt_merged <- dat %>% group_by(Latitude, Longitude, DepthElevation, Site, Soiltype, Repsite, Sample, Year, Month, Day, Family, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Site, Soiltype, Repsite, Sample, Month, Day, Genus, Species)
nrow(dat) - nrow(dt_merged) # any change in aggregating?
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Plot <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Site, Soiltype, Repsite, Sample, Year, Month, Day, sep='_')))
length(levels(dt_merged$SampleDescription)) # 8 samples (corresponds with the different transects run)

# Remove variables not listed in BIOTIME format (after saving key details in sample description)
dt_merged$Site <- NULL
dt_merged$Soiltype <- NULL
dt_merged$Repsite <- NULL
dt_merged$Sample <- NULL

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