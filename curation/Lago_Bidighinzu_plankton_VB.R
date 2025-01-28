# Curation Script ---------------------------------------------------------

# Dataset: LTER Lago Bidighinzu - plankton communities from 1988 to 2015
# Location: Italy
# Curator: Viviana Brambilla
# Note* not a whole lot to clean bc contributor filled out template

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines

rm(list=ls())
setwd("~/Documents/BioTIME/Lago_Bidighinzu_plankon")
dt <- readxl::read_excel('BioTIMEContributorTemplate UNISS.xlsx', sheet=2, col_names=T, na='')

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? N
dt$Year <- as.integer(dt$Year)
dt$Month <- as.integer(dt$Month)
dt$Day <- as.integer(dt$Day)
# Secondary fields such as trawl, plot, transect etc must be factors or integers - not available here

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA here
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[,11:13]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

# all fine as only one coord pair overlapping the Lake is provided
dt$Latitude<- 40.556059 
dt$Longitude <- 8.661328

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))
sum(is.na(dt$Family))
dt$Family <- as.factor(dt$Family)

# and rename levels using:
levels(dt$Family) <- c("BAC"="Bacillariophyceae",
                                   "CHL"="Chlorophyceae",
                                   "CHR"="Chrysophyceae",
                                   "CON"="Conjugatophyceae",
                                   "CRY"="Cryptophyceae",
                                   "CYA"="Cyanobacteria",
                                   "DIN"="Dinophyceae",
                                   "EUG"="Euglenophyceae",
                                   "OTHER"="Other")


dt$species <- dt$Species # always work from a copy
# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(dt$species, 'idae$|eae$')
dt[str_which(dt$Species, 'idae$|eae$'),]

## check for Pseudanabaenaceae and copy that over to the Family field
dt$Family <- as.character(dt$Family)
dt$Family[dt$Species == "Pseudanabaenaceae"] <- "Pseudanabaenaceae"
dt$Species[dt$Family == "Pseudanabaenaceae"] <- NA

## Family == "Others" includes all the unidentified taxa together -> do not include!
dt <- dt[dt$Family!="Other",]
dim(dt)

# split provided into Genus and Species
# difference between sp. and spp.: 
# while sp. refers to one taxa, spp refers to all the species within an IDed genus.

# take the n.d. species out
str_which(dt$species, 'n.d.$')
dt[str_which(dt$species, 'n.d.$'),]
dt$species[str_which(dt$species, 'n.d.$')]

dt$Genus[dt$species == "Bacillariophyceae n.d."] <- NA
dt$Species[dt$species == "Bacillariophyceae n.d."] <- NA

dt$Family[dt$species == "Chroococcales n.d."] <- "Chroococcales"
dt$Genus[dt$species == "Chroococcales n.d."] <- NA
dt$Species[dt$species == "Chroococcales n.d."] <- NA

dt$species[dt$species == "Bacillariophyceae n.d."] <- NA
dt$species[dt$species == "Chroococcales n.d."] <- NA
dt$species[dt$species == "Cyanophyceae n.d"] <- NA

# create unique sp level for cf. species
str_which(dt$species, 'cf.')
dt[str_which(dt$species, 'cf.'),]
dt$species[str_which(dt$species, 'cf.')]

dt$species[dt$species == "Pandorina cf. smitthii"] <- "Pandorina sp1"
dt$species[dt$species == "Cyanocathena cf. imperfecta"] <- "Cyanocathena sp1"
dt$species[dt$species == "Merismopedia cf. tenuissima"] <- "Merismopedia sp1"
dt$species[dt$species == "Kirchneriella cf. lunaris"] <- "Kirchneriella sp1"

sort(unique(dt$species))

dt$Genus <- word(dt$species, 1) # separate genus to its own column
dt$Species <- word(dt$species, start=2) # species to its own column
sort(unique(dt$Species))

# records with undefined species epithets into the BioTIME format of sp/spp without period
dt$Species[dt$Species == "sp."] <- "sp"
dt$Species[dt$Species == "spp."] <- "spp"

summary(as.factor(dt$species))

dt %>% filter(Species == 'sp') %>% select(Genus, Species) # check undefined species records
# all looks ok

dt$SampleDescription <- paste(dt$Year, dt$Month, sep = "_")
# Prepare raw data --------------------------------------------------------
dt$Abundance
dt$Biomass <- NA
# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species, SampleDescription,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)
nrow(dt) - nrow(dt_merged)

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
                         'StudyID')] %>% arrange(Year, Plot, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)
summary(as.factor(dt_merged$SampleDescription))

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'Lago_Bidighinzu_plankton_rawdata_VB.csv', row.names=F)
library(clipr)
write_clip(dt_merged)




