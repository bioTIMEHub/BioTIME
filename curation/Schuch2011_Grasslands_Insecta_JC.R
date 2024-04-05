# This is a data curation script for Schuch et al. (2011). Long-term population trends in three grassland insect groups: a comparative analysis of 1951 and 2009. Journal of Applied Entomology. 
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

# Define fileSave location
fileSave <- 'E:/BioTime/CuratedData/'

######################################
# STEP 1: Import and format the data to be curated
######################################

dt <- read_excel("E:/BioTime/Schuch et al 2011/Schuch_2011_Original Data.xls", sheet = 1,
                 skip = 1,
                 col_names = T,
                 na = '')

# Always visually check the data
view(dt)

# Still some further tidying up todo.
# reformat column names
names(dt) <- c("Species_ID", "1951_I", "1951_II", "1951_III","1951_IV","1951_V","1951_VI",                
               "1951_X","1951_XI","1951_XII","Blank","2009_I","2009_II","2009_III","2009_IV",
               "2009_V","2009_VI","2009_X","2009_XI","2009_XII","Over-wintering stage",
               "Feeding type","Dispersal ability","Generations per year")
# remove unessecary data
dt <- dt[,!names(dt) %in% c("Blank","Over-wintering stage","Feeding type","Dispersal ability","Generations per year")]
# re-assign family ID
dt$Family <- NA
dt[2:93,]$Family <- "Auchenorrhyncha"
dt[102:189,]$Family <- "Heteroptera"
dt[198:213,]$Family <- "Orthoptera"
# remove unessecary rows
dt <- dt[which(!is.na(dt$Family)),]

# Convert into long format
dt <- as.data.frame(dt %>% 
                      gather(Year_Site, Abundance, "1951_I", "1951_II", "1951_III","1951_IV","1951_V","1951_VI",                
                             "1951_X","1951_XI","1951_XII","2009_I","2009_II","2009_III","2009_IV",
                             "2009_V","2009_VI","2009_X","2009_XI","2009_XII"))
# Finally seperate out the year and site information
dt$Year <- substr(dt$Year_Site, start = 1, stop = 4)
dt$Station <- substr(dt$Year_Site, start = 6, stop = nchar(dt$Year_Site))
dt$Plot <- rep(NA, nrow(dt))
dt$Year_Site <- NULL

######################################
# STEP 2: Does the Dataset meet BioTime criteria?
######################################

n_distinct(dt$Year) >= 2 # have surveys been carried out over multiple years?

######################################
# STEP 3: Check the overall dataset and variable structure
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
# Implement nessecary conversions
dt$Abundance <- as.numeric(paste(dt$Abundance))
dt$Year <- as.factor(dt$Year)
dt$Plot <- as.factor(dt$Plot)

# Remove variables that are not required in the BioTime database
# if there is only one site surveyed then this variable is not needed
if(length(unique(dt$Site)) == 1) {dt$Site <-NULL}

######################################
# STEP 4: Check Primary variables (Abundance/Biomass, GPS coordinates, and dates)
######################################
# During this check it is nessecary to make sure that:
# 1. Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
# 2. Fields are logical, real values, i.e. within possible limits
#    For instance Year < 2021, Month < 12 and day < 31.
# 3. Pool abundances for different sexes or life-forms (adults & juveniles). 
#    An exception to this are amphibia (anurans in particular) because the methodology to sample adults & tadpoles can be very different. 
#    If the authors submit the data separately, we keep it this way.

# Run checks
min(dt$Abundance, na.rm = T) > 0 # there can be no zeros
sum(is.na(dt$Abundance)) > 0 # there can be no missing entries

# if there are rows that need to be removed (i.e. for any abundance entires of NA or 0)
dt <- dt[!is.na(dt$Abundance),]
dt <- dt[which(dt$Abundance != 0),]

# Insert GPS coordinates if not already present
# The study provides the details of nearby towns which will need to be used to determine locality.
dt_coords <- data.frame(Longitude = c(10.2590, 9.0704),
                        Latitude = c(52.1615,  52.5163))
# Convert data points into point spatial object
dt_coord <- dt_coords %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
# add latitude and longitude to data
dt$Longitude <- rep(centroid[1], dim(dt)[1])
dt$Latitude <- rep(centroid[2], dim(dt)[1])

# it is also worthwhile checking the coordinates plot as expected (identifies whether coordinates have been mixed up)
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(0,10), ylim=c(40,60))
points_zoom # all looks good

# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

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
dt$Species <- dt$Species_ID

# check that genera are genera, not family names (i.e. -idae/eae)
# this returns the record index number if there are any
str_which(dt$Species, 'idae$|eae$')

# check the species list for misspellings or non-BioTIME taxonomic convention names
# Check species
sort(unique(word(dt$Species,start=2, end=-1)))
# Check genera
sort(unique(word(dt$Species, 1)))
# Check family
sort(unique(dt$Family))

# All correct....

# Split taxonomic variable into Family, genus and species if needed
dt$Genus <- word(dt$Species_ID, 1) # separate genus to its own column
dt$Species <- word(dt$Species_ID, start=2) # species to its own column. Only the second word to ignore subspecies

# check visually, but do this in your console.
dt %>% distinct(Species_ID, Genus, Species) %>% View()

# remove original taxonomic variable if its has been split
dt$Species_ID <- NULL 

######################################
# STEP 6: Prepare curated data for export
######################################

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <- rep('', nrow(dt))
dt$Month <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

# aggregate abundance records that are same species, plot, and survey day.
# NB: it may be nessecary to also summarise Biomass estimates if given.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, Plot, Station, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
# any change in aggregating?
dim(dt)[1]-dim(dt_merged)[1]

# save the dataset name as an object so we save some typing
dataset_name <- 'Schuch2011_Grasslands_Insecta'
# Generate a sample description. For this put in as many non-blank fields unique to the sampling event from which the abundance estimate was obtained.
# If plot, DepthElevation, month, and day are available, add those in too
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Station, sep='_')))
length(levels(dt_merged$SampleDescription)) # how many sampling events?

# Remove variables not contained in BIOTIME format
dt_merged$Station <- NULL

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
write.csv(dt_merged, paste0(fileSave, dataset_name, '_rawdata_JC.csv'), row.names=F, na='')
write_clip(dt_merged)

# ---------------------------------------------------- End of Code --------------------------------------------------