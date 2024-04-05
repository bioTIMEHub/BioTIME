# Curation Script ---------------------------------------------------------

# Dataset: Joern_2020 (PBG07 Grasshopper species abundances in the Patch-Burn Grazing experiment at Konza Prairie.)
# Location: Konza Prairie
# Contact: knzlter@ksu.edu
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
data.name <- 'Joern_2020'
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Joern 2020/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# define file name
infile <- 'E:/BioTime/Joern 2020/Joern_2020_OriginalData.csv'
# Run download
if(!dir.exists('E:/BioTime/Joern 2020/') || !file.exists(infile))   {
  dir.create('E:/BioTime/Joern 2020/',  showWarnings = FALSE)
  inUrl  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/121/5/67a04b9c87218fd3c377d99f4e6b834e"
  download.file(inUrl, infile, method="curl")
}

# Open datafile
dat <-read.csv(infile)

###########################
# STEP 2: Clean and reformat data variables
###########################
# Fix any columns read in the wrong format
if (class(dat$Recyear)!="factor") dat$Recyear <- as.factor(dat$Recyear)
if (class(dat$Recday)!="factor") dat$Recday <- as.factor(dat$Recday)
if (class(dat$Recmonth)!="factor") dat$Recmonth <- as.factor(dat$Recmonth)
if (class(dat$Watershed)!="factor") dat$Watershed <- as.factor(dat$Watershed)
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
dat$Total <- NULL
dat$Comments <- NULL
dat$Datacode <- NULL
dat$Rectype <- NULL
dat$Spcode <- NULL

### Reshape data format --------------------------------
dat <- melt(dat, id.vars = c("Recyear", "Recmonth", "Recday", "Watershed", 'Repsite', 'Genus', "Species"),
            variable.name = 'Sample', value.name = 'Abundance')

### Clean Abundance (remove NAs) -----------------------
dat <- dat[!(is.na(dat$Abundance)),]
# check to confirm
min(dat$Abundance) > 0 # no zeroes?
sum(dat$Abundance == "" | dat$Abundance == ' ') == 0 # no blanks?

### Clean site details ---------------------------------
names(dat)[4] <- 'Site'
# Ensure consistency in site naming
unique(dat$Site)
dat$Site <- toupper(dat$Site)
dat$Site <- as.factor(dat$Site)

### Clean Date info ------------------------------------
names(dat)[1] <- 'Year'
names(dat)[2] <- 'Month'
names(dat)[3] <- 'Day'
# No negative values, 0s or NAs, and all are logical
# i.e month < 12, day < 31
summary(dat[c('Year', 'Month', 'Day')])

### Clean Taxonomy -------------------------------------
# Check no family names in the genus variable
dat$Genus <- str_to_sentence(dat$Genus)
dat[which(str_detect(dat$Genus, 'idae$|ini$')),]
# Insert Family variable and relocate any inappropriate genus entries
dat$Family <- dat$Genus
dat[str_which(dat$Family, 'idae$|eae$', negate = T),]$Family <- NA
dat[!(is.na(dat$Family)),c('Genus')] <- NA
dat$Family <- as.factor(dat$Family)

# Tidy spelling errors
# Check species
sort(unique(dat$Species))
dat$Species[dat$Species == 'spp.'] <- 'sp'
dat$Species[dat$Species == 'admirabil'] <- 'admirabilis'
dat$Species[dat$Species == 'bivitatta'] <- 'bivittata'
dat$Species[dat$Species == 'bivittatu'] <- 'bivittatus'
dat$Species[dat$Species == 'femurrubr'] <- 'femurrubrum'
dat$Species[dat$Species == 'nebrascen'] <- 'nebrascensis'
dat$Species[dat$Species == 'sp.'] <- 'sp'
dat$Species[dat$Species == 'viridifas'] <- 'viridifasciata'
dat$Species[dat$Species == 'xanthopte'] <- 'xanthoptera'
dat$Species[dat$Species == ''] <- 'sp'
# Check genera
sort(unique(dat$Genus))
dat$Genus[dat$Genus == 'Brachystol'] <- 'Brachystola'
dat$Genus[dat$Genus == 'Campylacan'] <- 'Campylacantha'
dat$Genus[dat$Genus == 'Chortophag'] <- 'Chortophaga'
dat$Genus[dat$Genus == 'Hesperotet'] <- 'Hesperotettix'
dat$Genus[dat$Genus == 'Pardalopho'] <- 'Pardalophora'
dat$Genus[dat$Genus == 'Phoetaliot'] <- 'Phoetaliotes'
dat$Genus[dat$Genus == 'Schistocer'] <- 'Schistocerca'
# recorrect data format
dat$Genus <- as.factor(dat$Genus)
dat$Species <- as.factor(dat$Species)

# Add and confirm LAT & LONGs and Elevation ------------------------------------
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

### Fix final variable
dat$Repsite <- toupper(dat$Repsite)
if (class(dat$Repsite)!="factor") dat$Repsite <- as.factor(dat$Repsite)

# PREPARE RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, site, transect, and survey day.
dt_merged <- dat %>% group_by(Latitude, Longitude, DepthElevation, Site, Repsite, Sample, Year, Month, Day, Family, Genus, Species) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Site, Repsite, Sample, Month, Day, Genus, Species)
nrow(dat) - nrow(dt_merged) # any change in aggregating?
# now create empty columns needed to fit to template
dt_merged$Biomass <- rep('', nrow(dt_merged))
dt_merged$StudyID <- rep('', nrow(dt_merged))
dt_merged$Plot <- rep('', nrow(dt_merged))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Site, Repsite, Sample, Year, Month, Day, sep='_')))
length(levels(dt_merged$SampleDescription)) # 8 samples (corresponds with the different transects run)

# Remove variables not contained within BIOTIME format
dt_merged$Site <- NULL
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
                         'StudyID')] %>% arrange(Year, Month, Day, Family, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(fileSave, data.name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_merged)

# -------------------------------------------- End of Code ------------