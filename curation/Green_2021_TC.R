
# Curation Script ---------------------------------------------------------

# Dataset: Signals of resilience and change in tidepool fish communities on the Pacific coast of Vancouver Island, Canada
# Location: Vancouver Island in British Columbia
# Curator: Tadhg Carroll
# Date: 25-Jul-2023

# Set up ------------------------------------------------------------------

# load packages
require(dplyr)
require(data.table)
require(skimr)
library(ggplot2)
require(maps)
require(stringr)


rm( list=ls() ) # clear up the environment before starting

getwd()
# Read in Original raw data from Dryad
dt <- dt <- base::readRDS( "./OriginalData_TC/Green_2021_OriginalData.rds" )

# Convert Year and Count to numeric
dt[ , ':=' ( Year = as.numeric(Year), Count = as.numeric(Count) ) ]


# The authors surveyed seven tide pools, each over multiple years with consistent sampling methods.
# However, the pools vary in size and volume, and the differences between some of them are relatively substantial given
# the scale of sampling (see Figure 1 from Green et al (2021).

# This means the raw counts do not meet BioTIME Criterion 4 "Sampling methods and effort are consistent through time".

# To fix this we can standardise the counts into densities by the tide pool volume, as the authors sampled all fish
# in the pools during each sampling event.

# Compute Densities to account for differing pool sizes
# First add tide pool volumes (m^3) extracted from Figure 1 in Green et al (2021)
dt[ , ':=' ( Vol = c(0.93, 0.5, 1.12, 0.6, 0.82, 5.32, 7.43)[ match( Tidepool.., c("1", "2", "3", "4", "5", "6", "7") ) ] )
    # Then compute densities of fish as counts per m^3 of water
    ][ , ':=' ( Density = Count/Vol )][]





# Structure check ---------------------------------------------------------

# Have a look at the raw data
View(dt)

# Skim dt_long for structure check & summary
skim(dt)
## No missing values and variables are all of the correct type.

# Sort into BioTIME format
dt <- dt[ , .( Abundance = Density, Biomass = NA, # Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
               Family = NA, Genus = NA, Species = Species,
               SampleDesc = paste( Month, Year, Tidepool.., sep = "_" ), Plot = Tidepool..,
               Latitude = 48.737685, Longitude = -125.118645,
               DepthElevation = NA, Day = NA, Month = Month, Year = Year,
               StudyID = "" ) ]
# Lat/Lon lifted from Alban's script and verified on Google Maps.




# Primary field checks -----------------------------------------------------

# 'skim' to look again at 'Primary fields'
skim(dt)


# Have a little look at Abundance
hist( dt$Abundance, breaks = 142 )
# Abundances (i.e. densities) look good.

## Double-check Abundances
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y


# Double-check Year
summary( dt[ , .( Year ) ] )



# Check the bounding area of recorded sample plots and the number of plots}
dt[ , .( min_lat = min(Latitude), min_long = min(Longitude), max_lat = max(Latitude), max_long = max(Longitude), n_plot = length(unique(Plot)) ) ]
## No blanks, no NAs
## Latitude constrained from -90 to 90.
## Longitude constrained -180 to 180.


# Are they where they should be?
world_map <- map_data('world') # check whether the GPS coordinates match expectations

world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)

# Plot the map
points + coord_fixed(xlim=c(-127,-122), ylim=c(45,51))
# And zoom out a bit...
points + coord_fixed(xlim=c(-142,-101), ylim=c(24,64))









# Secondary field checks -----------------------------------------------------


# I've already sorted the "Plot" column (which is just the tidepool IDs).

# Have a look at "Month" variable
unique(dt$Month)
## All good





# Taxonomic field check ---------------------------------------------------

# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()


# How many "Sebastes sp." and "Sebastodes sp."
dt[ Species %in% c("Sebastes sp.", "Sebastodes sp.") ][ order(Species) ]
## "Sebastes sp." and "Sebastodes sp." are both listed as species, but are in fact synonyms of
## the same Genus (https://www.mindat.org/taxon-2335318.html; https://en.wikipedia.org/wiki/Sebastes).
## There is only one occurrence of each (in different pools/years), I will replace "Sebastodes" with
## "Sebastes", as this seems the more up-to-date and commonly used name.

# Replace Sebastodes" with "Sebastes"
dt[ , ':=' ( Species = str_replace_all( Species, c("Sebastodes sp." = "Sebastes sp.") ) ) ]

# Have a look at the fixed Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ]
## All good


# Fit proper taxa level with the correct fields
dt[ , ':=' ( Genus = word(Species,1), Species = word(Species,2) )
    # And fix "sp."
    ][ , ':=' ( Species = str_replace_all( Species, c("sp." = "sp") ) ) ][]

# Check Species again
sort(unique(dt$Species))


# Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names
str_which(dt$Species, 'idae$|eae$')



# Important taxonomic checks

# 1. Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes.

## - *Not applicable*


# 2. Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.
#    If the dataset distinguishes between different uncertain IDs (Genus sp),
#    reformat them while making sure they’re recognised as separate species: like “Genus sp1” and “Genus sp2”

## - *Done*


# 3. Fit proper taxa level with the correct fields (Family, Genus, Species)

## - *Done*


# 4. Remove non-organismal records (e.g. rock, sand) and incidental taxa records.

## - *Not applicable*


# 5. Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names.

## - *Done*


# 6. Re-allocate taxonomic classifications into the appropriate column.

## - *Not applicable*


# 7. Check for duplication and misspellings in the species names
#   (e.g. Puffnius sp, Puffinus sp; unidentified/unknown; Buenia jeffreysi / Buenia jeffreysii; double spacing; several spaces, authors name & date, etc.).
#   We’re not validating or updating species, but we do quick searches in Google Scholar to figure out which spelling is correct.

## - *Done*


# 8. Subspecies and annotated species names should be disregarded (e.g. Amphiura (acrocnida) brachiate should be considered as Acrocnida brachiate)
#    Some taxa are less resolved and are placed into species complexes, e.g. Dascyllus trimaculatus agg. or Rubus fruticosus agg.
#    We denote this with “agg.” to make sure it’s not confused with other specific epithets.

## - *Not applicable*


# 9. If a record only has data until the genus level, it must have “sp” in species column.

## - *Done*





# Prepare for export --------------------------------------------------------

# Check for any duplicate species records/abundances per sample
dt_check <- dt[ , .( n = .N ), by = c("Genus", "Species", "SampleDesc" )]
dt_check[ , .( unique(n) ) ]
## All good


# Export curated data
fwrite( dt, file = "./Green_2021/Green_2021_TC_complete/Green_2021_RawData_TC.csv" )



