# This is a data curation script for the Hanko station bird monitoring LTER.
# -------------------------------------------------------------------------------

# Primary Author: James Cant
# Date last modified: June 2022
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

# define folder for saving
fileSave <- 'E:/BioTime/CuratedData/'

# import the data to be curated 
dt <- read.csv('E:/BioTime/Hanko Station/Hanko Station_Haliasdata_v1.5.csv')

# Always visually check the data
view(dt)

######################################
# STEP 1: Does the Dataset meet BioTime criteria?
######################################

n_distinct(dt$Year) >= 2 # have surveys been carried out over multiple years?
dt <- dt[which(dt$Year < "2020"),] # After 2020 mammals were introduced to the servey so the study becomes inconsistent from 2019 onwards.

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
dt$Local <- as.numeric(paste(dt$Local))
dt$Migr <- as.numeric(paste(dt$Migr))
dt$Night_migr <- as.numeric(paste(dt$Night_migr))

# Remove variables that are not required in the BioTime database
dt$FIN_name <- NULL; dt$SWE_name <- NULL; dt$ENG_name <- NULL; dt$Species_Abb <- NULL; dt$Species_code <- NULL; dt$Day.of.Year <- NULL
# The following variables are related to abundance counts, however according to the data metadata these are not consistent entries and should only be used with considerable care.
dt$Observed <- NULL; dt$Additional <- NULL
# Finally although these final variable is a reliable count of abundance. Its detail is already included within the Migr variable.
dt$Stand <- NULL

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

# First define Abundance counts. In this data set these are the sum of local observations, migratory observations, and night observations (avoid biasing against species that migrate during the night)
dt$Abundance <- rowSums(dt[, c('Local', "Migr", 'Night_migr')], na.rm = TRUE)

# Run checks
min(dt$Abundance, na.rm = TRUE) > 0 # there can be no zeros
sum(dt[which(is.na(dt$Abundance)),]) > 0 # there can be no missing entries
table(dt$Year)

# if there are rows that need to be removed (i.e. for any abundance entires of NA or 0)
dt <- dt[!is.na(dt$Abundance),]
dt <- dt[which(dt$Abundance != 0),]

# Insert GPS coordinates if not already present
dt$Latitude <- as.numeric(rep('59.817', nrow(dt))) # these were obtained from publications using the Hanko observation data
dt$Longitude <- as.numeric(rep('22.900', nrow(dt)))

# Check the coordinates match with the expected location of the data
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(20,30), ylim=c(50,70))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Remove replaced variables
dt$Local <- NULL; dt$Migr <- NULL; dt$Night_migr <- NULL

# Reformat the date variable
dt$Day <- as.factor(substr(dt$Date, start = 1, stop = 2))
dt$Month <- as.factor(substr(dt$Date, start = 4, stop = 5))

# Removed replaced variable 
dt$Date <- NULL

######################################
# STEP 4: Check Nomenclature provided 
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
dt$Species <- dt$Sci_name

# check that genera are genera, not family names (i.e. -idae/eae)
# this returns the record index number if there are any
str_which(dt$Species, 'idae$|eae$') # There appears to be a few family names parading as genera.

# check the species list for misspellings or non-BioTIME taxonomic convention names
# Check species
sort(unique(word(dt$Species, start=2, end=-1)))
# Check genera
sort(unique(word(dt$Species, 1)))
# Check combined
sort(unique(dt$Species))
# The Taxonomy of this data is a mess.

# For entries for which genus identity is unresolved we can just assign the family name 
dt$Family <- NA
dt[which(dt$Sci_name == "Alca / Uria"), "Family"] <- 'Alcidae'
dt[which(dt$Sci_name == "Anatidae sp"), "Family"] <- 'Anatidae'
dt[which(dt$Sci_name == "Anser / Branta"), "Family"] <- 'Anatidae'
dt[which(dt$Sci_name == "Bombycilla / Sturnus"), "Family"] <- "Passeriformes"
dt[which(dt$Sci_name == "Cyanistes / Parus / Periparus / Lophophanes / Poecile"), "Family"] <- "Paridae"
dt[which(dt$Sci_name == "Gelochelidon nilotica / Sterna sandvicensis"), "Family"] <- "Laridae"
dt[which(dt$Sci_name == "Haliaetus / Aquila"), "Family"] <- "Accipitridae"
dt[which(dt$Sci_name == "Halichoerus / Pusa"), "Family"] <- "Phocidae"
dt[which(dt$Sci_name == "Hirundo rustica X Delichon urbicum"), "Family"] <- "Hirundinidae"
dt[which(dt$Sci_name == "magnus Accipitriformes"), "Family"] <- "Accipitridae"
dt[which(dt$Sci_name == "magnus Anatidae"), "Family"] <- "Anatidae"
dt[which(dt$Sci_name == "magnus Aves"), "Family"] <- "Aves"
dt[which(dt$Sci_name == "medium Picidae"), "Family"] <- "Picidae"
dt[which(dt$Sci_name == "parvus Accipitriformes" ), "Family"] <- "Accipitridae"
dt[which(dt$Sci_name == "parvus Anatidae"), "Family"] <- "Anatidae"
dt[which(dt$Sci_name == "parvus Laridae"), "Family"] <- "Laridae"
dt[which(dt$Sci_name == "Periparus ater x Poecile montanus"), "Family"] <- "Paridae"
dt[which(dt$Sci_name == "Pernis / Buteo"), "Family"] <- "Accipitridae"
dt[which(dt$Sci_name == "Riparia / Hirundo / Delichon"), "Family"] <- "Hirundinidae"
dt[which(dt$Sci_name == "IK (iso kahlaaja)"), "Family"] <- "Charadriiformes"
dt[which(dt$Sci_name == "IPL (iso pikkulintu)"), "Family"] <- "Not specified"
dt[which(dt$Sci_name == "K (kahlaaja)"), "Family"] <- "Charadriiformes"
dt[which(dt$Sci_name == "KK (Keskikokoinen kahlaaja)"), "Family"] <- "Charadriiformes"
dt[which(dt$Sci_name == "KPL (keskikokoinen pikkulintu)"), "Family"] <- "Not specified"
dt[which(dt$Sci_name == "PK (pieni kahlaaja)"), "Family"] <- "Charadriiformes"
dt[which(dt$Sci_name == "PL (pikkulintu)"), "Family"] <- "Not specified"
dt[which(dt$Sci_name == "PPL (pieni pikkulintu)"), "Family"] <- "Not specified"
dt[which(dt$Sci_name == "V / K (vesilintu / kahlaaja)"), "Family"] <- "Charadriiformes"
# Now remove the species names of any entries for which only family identity is available
dt[which(!is.na(dt$Family)), "Species"] <- NA

# define any nessecary Species name replacements (for which genus is at least resolved)
initial_replace <- c(
  "Acrocephalus dumetorum / Acrocephalus palustris / Acrocephalus scirpaceus" = "Acrocephalus sp",
  "Anserini / Phalacrocorax" = 'Phalacrocorax sp',
  "Anthus richardi / Anthus godlewskii / Anthus campestris" = "Anthus sp",
  "Anthus richardi / Anthus godlewskii" = "Anthus sp",
  "Aquila clanga / Aquila pomarina / Aquila pomarina" = "Aquila sp",
  "Aquila clanga / Aquila pomarina" = 'Aquila sp',
  "Aythya fuligula / Aythya marila" = "Aythya sp",
  "Circus cyaneus / Circus macrourus / Circus pygargus" = "Circus sp",
  "Circus macrourus / Circus pygargus" = "Circus sp",
  "Corvus corone corone" = "Corvus corone",
  "Emberiza rustica / Emberiza pusilla / Emberiza aureola" = "Emberiza sp",
  "Gavia immer / Gavia adamsii" = "Gavia sp",
  "Larus glaucoides / Larus hyperboreus" = "Larus sp",
  "Loxia curvirostra / Loxia pytyopsittacus" = "Loxia sp",
  "magnus Corvus" = "Corvus sp",
  "magnus Falco" = "Falco sp",
  "magnus Larus" = "Larus sp",
  "magnus Turdus" = "Turdus sp",
  "Mergus serrator / Mergus merganser" = "Mergus sp",
  "parvus Falco" = "Falco sp",
  "parvus Turdus" = "Turdus sp",
  "Podiceps auritus / Podiceps nigricollis" = "Podiceps sp",
  "Podiceps cristatus / Podiceps grisegena" = "Podiceps sp",
  "Poecile montanus X Poecile cinctus" = "Poecile sp",
  "Sterna hirundo / Sterna paradisaea" = "Sterna sp")
  
# Apply initial species name fix before then checking for spelling errors.
dt$Species <- str_replace_all(dt$Species, initial_replace)
# Recheck species
sort(unique(word(dt$Species, start=2, end=-1)))
# Check genera
sort(unique(word(dt$Species, 1)))
# and recheck that all family ids have been move to the appropriate location
str_which(dt$Species, 'idae$|eae$')

# Now split taxonomic variable into genus and species if needed
dt$Genus <- word(dt$Species, 1) # separate genus to its own column
dt$Species <- word(dt$Species, start=2) # species to its own column. Only the second word to ignore subspecies
# check visually, but do this in your console.
dt %>% distinct(Family, Genus, Species) %>% View()

# remove original taxonomic variable if its has been split
dt$Sci_name <- NULL 

######################################
# STEP 5: Prepare curated data for export
######################################

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Biomass <- rep('', nrow(dt))
dt$Plot <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$Site <- rep('Halias', nrow(dt))

# aggregate abundance records that are same species, plot, and survey day.
# NB: it may be nessecary to also summarise Biomass estimates if given.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, Site, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Site, Family, Genus, Species)
# any change in aggregating?
dim(dt)[1]-dim(dt_merged)[1]

# save the dataset name as an object so we save some typing
dataset_name <- 'Hanko_LTER_Aves'
# Generate a sample description. For this put in as many non-blank fields unique to the sampling event from which the abundance estimate was obtained.
# If plot, DepthElevation, month, and day are available, add those in too
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Site, Day, Month, Year, sep='_')))
length(levels(dt_merged$SampleDescription)) # how many sampling events?

# Remove variables that don#t fit BIOTIME format (after storing key details in sample description)
dt_merged$Site <- NULL

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
# STEP 6: Export curated data and prepare spreadsheet
######################################

# Save file (remembering to include personal identifier)
write.csv(dt_merged, paste0(fileSave, dataset_name, '_rawdata_JC.csv'), row.names=F, na='')

# The BioTime setup consists of a series of data tables outlining the Raw data (curated above) and it associated metadata
# all of which will be linked by the studyID.
# Most of the metadata will either be found in the source paper or provided by the dataset contributor.
# Some variables that may need calculating are the central coordinates (CentralLatitude, CentralLongitude) and sampling area. 

#  Handy line to copy the raw data directly to your clipboard for pasting into Excel :)
clipr::write_clip(dt_merged)

# ---------------------------------------------------- End of Code --------------------------------------------------