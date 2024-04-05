# This is a data curation script for processing temporal vegetative cover data from Fukamachi et al (2020). Structure and dynamics of a mountain riparian forest at an upstream valley in central Japan. Ecological Research.
# -------------------------------------------------------------------------------
# Curator: James Cant
# Date last modified: July 2022
# -------------------------------------------------------------------------------

# load required packages
require(tidyverse)
require(readxl)
require(maps)
require(sp)
require(rgeos)
require(clipr)

# clear workspace
rm(list=ls())

# define save location
fileSave <- 'E:/BioTime/CuratedData/'

# import the data to be curated 
dt <- read.csv('E:/BioTime/Fukamachi et al 2020/Fukamachi_2020_OriginalData.csv')

# Always visually check the data
view(dt)

######################################
# STEP 1: Does the Dataset meet BioTime criteria?
######################################
# whether the dataset meets the criteria can be determined via scanning the methods and data metadata for the following details

# Depending on the data R can be used to explore whether some of the criteria are met.
n_distinct(dt$Year) >= 2 # have surveys been carried out over multiple years?

######################################
# STEP 2: Check the overall dataset and variable structure
######################################

# check dimensions
dim(dt)

# check variable class. For consistency across datasets the following variables need to be in the specified format
summary(dt)
# Abundance or biomass: numeric
# Coordinates: numeric
# Dates: POSIXct or
# Year, month, day* columns,: integers or factors
# Plot: factors or integers
# DepthElevation: numeric or factors (if they’re a treatment category)
# Taxonomic: characters or factors

# Implement corrections
dt$Year <- as.factor(dt$Year)
# correct variable format
dt$gridNO <- as.factor(dt$gridNO)
dt$Strata <- as.factor(dt$Strata)
dt$Plot <- NA

# remove unnecessary variables
dt$Abundance <- NULL # this does not mean abundance as in counts.

# Make life simpler by renaming variables to match the BioTime format
colnames(dt)[5] <- 'Biomass' # This is Biomass in coverage format

######################################
# STEP 3: Check Primary variables (Abundance/Biomass, GPS coordinates, and dates)
######################################
# During this check it is nessecary to make sure that:
# 1. Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
# 2. Fields are logical, real values, i.e. within possible limits
#    For instance Year < 2021, Month < 12 and day < 31.
# 3. We need to pool abundances for different sexes or life-forms (adults & juveniles). 
#    An exception to this are amphibia (anurans in particular) because the methodology to sample adults & tadpoles can be very different. 
#    If the authors submit the data separately, we keep it this way.

# The authors recorded percentage cover using the decimal cover Londo scale (Londo 1976)
# This scale needs to be reverted back to a percentage scale.
# In the Londo scale numerical values between 0 and 10 are used to represent a coverage category (i.e. 5-10%, 10-15% etc.). For the purposes of this reformat, the midpoint of each coverage category will be assigned to each corresponding decimal entry.
# Implement replacements
dt$Biomass[dt$Biomass == '10'] <- '97.5'
dt$Biomass[dt$Biomass == '9'] <- '90'
dt$Biomass[dt$Biomass == '8'] <- '80'
dt$Biomass[dt$Biomass == '7'] <- '70'
dt$Biomass[dt$Biomass == '6'] <- '60'
dt$Biomass[dt$Biomass == '5'] <- '50'
dt$Biomass[dt$Biomass == '4'] <- '40'
dt$Biomass[dt$Biomass == '3'] <- '30'
dt$Biomass[dt$Biomass == '2'] <- '20'
dt$Biomass[dt$Biomass == '1'] <- '10'
dt$Biomass[dt$Biomass == '0.4'] <- '4'
dt$Biomass[dt$Biomass == '0.2'] <- '2'
dt$Biomass[dt$Biomass == '0.1'] <- '0.75'
dt[which(dt$Biomass == "+"), 'Biomass'] <- '0.25'
# convert to numeric
dt$Biomass <- as.numeric(paste(dt$Biomass))

# Run checks
min(dt$Biomass) > 0 # there can be no zeros
sum(is.na(dt$Biomass)) # there can be no missing entries

# if there are rows that need to be removed (i.e. for any abundance entries of NA or 0)
dt <- dt[!is.na(dt$Biomass),]
dt <- dt[which(dt$Biomass != 0),]

# Insert GPS coordinates if not already present
dt$Latitude <- as.numeric(rep('36.533333', nrow(dt))) # these were obtained from the manuscript (given as the coordinates for the Field Museum Kusaki - where the study takes place.
dt$Longitude <- as.numeric(rep('139.416667', nrow(dt)))

# it is also worthwhile checking the coordinates plot as expected (identifies whether coordinates have been mixed up)
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(120,150), ylim=c(30,50))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

######################################
# STEP 4: Check Secondary variables (The remaining variables not explicitly checked above)
######################################
# These variables need checking for errors. However an error doesn't nessecary mean it needs removing. 
# Also these variables can be left blank if no details are provided
# 1. If the error (e.g. a NULL) makes it impossible to assign the value to a group, then remove it (not the full entry just the erroneous value)
# 2. If the error is a misspelling or a missing value that can be clearly re-assigned by reading the methods, then do not remove the record and fix it in the appropriate way.

# Plot variables must be inspected for NA, NULL, blank or values unspecified in source methods
sort(unique(dt$gridNO))
sort(unique(dt$Strata))

######################################
# STEP 5: Check Nomenclature provided 
######################################
# Checking the taxonomic names provided requires the use of stringr.

# Important taxonomic checks
# 1. Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes.
# 2. Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used. 
#     If the dataset distinguishes between different uncertain IDs (Genus sp), 
#     reformat them while making sure they’re recognised as separate species: like “Genus sp1” and “Genus sp2”
# 3. Fit proper taxa level with the correct fields (Family, Genus, Species)
# 4. Remove non-organismal records (e.g. rock, sand) and incidental taxa records.
# 5. Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names.
# 6. Re-allocate taxonomic classifications into the appropriate column.
# 7. Check for duplication and misspellings in the species names 
#    (e.g. Puffnius sp, Puffinus sp; unidentified/unknown; Buenia jeffreysi / Buenia jeffreysii; double spacing; several spaces, authors name & date, etc.). 
#    We’re not validating or updating species, but we do quick searches in Google Scholar to figure out which spelling is correct.
# 8. Subspecies and annotated species names should be disregarded (e.g. Amphiura (acrocnida) brachiate should be considered as Acrocnida brachiate)
#    Some taxa are less resolved and are placed into species complexes, e.g. Dascyllus trimaculatus agg. or Rubus fruticosus agg.
#    We denote this with “agg.” to make sure it’s not confused with other specific epithets.
# 9. If a record only has data until the genus level, it must have “sp” in species column.

# Run checks - always taking care to work from a copied species list
dt$Species <- dt$Latin_name

# check that genera are genera, not family names (i.e. -idae/eae)
# this returns the record index number if there are any
str_which(dt$Species, 'idae$|eae$')
sort(unique(dt$Species))

# Split taxonomic variable into Genus and species
dt$Genus <- word(dt$Species, 1) # separate genus to its own column
sort(unique(dt$Genus))
dt$Species <- word(dt$Species, start=2) # species to its own column. Only the second word to ignore subspecies
sort(unique(dt$Species))
# reformat name convention for consistency
dt$Species[dt$Species == 'spp.'] <- 'sp'
dt$Species[dt$Species == 'spp'] <- 'sp'
dt$Species[dt$Species == 'sp.'] <- 'sp'
dt$Species[dt$Species == 'イネ科'] <- 'sp'
dt$Species[dt$Species == 'カヤツリグサ科'] <- 'sp'
dt$Species[dt$Species == 'キジカクシ科'] <- 'sp'
dt$Species[dt$Species == 'ツユクサ科'] <- 'sp'
sort(unique(dt$Species)) # manually check to confirm
# reformat variables
dt$Genus <- as.factor(dt$Genus)
dt$Species <- as.factor(dt$Species)
# remove old taxonomy variable
dt$Latin_name <- NULL

######################################
# STEP 6: Prepare curated data for export
######################################

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Abundance <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <- rep('', nrow(dt))
dt$Month <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

# aggregate abundance records that are same species, plot, and survey day.
# NB: it may be nessecary to also summarise Biomass estimates if given.
dt_merged <- dt %>% group_by(Abundance, Genus, Species, gridNO, Strata, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, gridNO, Strata, Genus, Species)
# any change in aggregating?
dim(dt)[1]-dim(dt_merged)[1]

# save the dataset name as an object so we save some typing
dataset_name <- 'Fukamachi2020_VegetativeCoverJapan'
# Generate a sample description. For this put in as many non-blank fields unique to the sampling event from which the abundance estimate was obtained.
# If plot, DepthElevation, month, and day are available, add those in too
dt_merged$Family <- rep('', length(dt_merged$Abundance))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, gridNO, Strata, sep='_')))
length(levels(dt_merged$SampleDescription)) # how many sampling events?

# Remove variables that don't fit BIOTIME format (after sorting key details in sample description)
dt_merged$Strata <- NULL
dt_merged$gridNO <- NULL

# Now reorder the columns to the proper BioTime format:
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

######################################
# STEP 7: Export curated data and prepare spreadsheet
######################################

# Save file (remembering to include personal identifier)
write.csv(dt_merged, paste0(fileSave, '/', dataset_name, '_rawdata_JC.csv'), row.names=F, na='')

# The BioTime setup consists of a series of data tables outlining the Raw data (curated above) and it associated metadata
# all of which will be linked by the studyID.
# Most of the metadata will either be found in the source paper or provided by the dataset contributor.
# Some variables that may need calculating are the central coordinates (CentralLatitude, CentralLongitude) and sampling area. 

#  Handy line to copy the raw data directly to your clipboard for pasting into Excel :)
clipr::write_clip(dt_merged)

# ---------------------------------------------------- End of Code --------------------------------------------------