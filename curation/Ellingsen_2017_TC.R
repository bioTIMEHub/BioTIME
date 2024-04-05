
# Curation Script ---------------------------------------------------------

# Dataset: Long-term environmental monitoring for assessment of change: measurement inconsistencies over time and potential solutions
# Location: North Sea
# Curator: Tadhg Carroll
# Date: 19-Jul-2023

# Set up ------------------------------------------------------------------

# load packages
require(data.table)
require(skimr)
library(ggplot2)
require(maps)
require(stringr)

rm( list=ls() ) # clear up the environment before starting

getwd()
# Read in Original raw data from Dryad (but pre-processed using Ellingsen script)
dt_Org_processed <- fread( "./OriginalData_TC/Ellingsen_2017_OriginalData_PreProccessed.csv" ) %>%
  data.table()

# Coordinates and depths of sampling stations from Ellingsen et al 2018 (Table 1.)
dt_Ellingson_coords <- fread( "./OriginalData_TC/Ellingsen_2017_OriginalData_TABLE_1_coords.csv" )
View(dt_Ellingson_coords)

## "dt_Org_processed" is the raw data pre-processed using the "MOD-DRYAD" R script provided in the Dryad
## repository for the data to get rid of the taxonomic classification lines at the top of the data file,
## and add in "year", "stat" and "rep" variables.




# Structure check --------------------------------------------------------------

# Have a look at the raw data
View(dt_Org_processed)
## Need to 'melt' to long format (as species abundances are input in wide format), but other than that,
## no missing values and variables are of the correct type.

# Get the pre-processed data into long form (and rename Species and "Abundance" to BioTIME naming convention in the process)
dt_long <- melt( dt_Org_processed, id.vars = c("year", "stat", "rep"), measure.vars = c(2:389),
                 variable.name = "Species", value.name = "Abundance" )

# Skim dt_long for structure check & summary
skim(dt_long)
## Variables are of required type with no missing values.



# Add in Coordinatess and depths of sampling stations
dt_long <- dt_long[ dt_Ellingson_coords, on = .(stat) ]
## Remember, these were provided in table 1 of the the paper (Ellingsen et al 2017)


# Note: Many of the Taxon names included have no entries with Abundance > 0 and so will be excluded
dt_long[ , .( uniqueN(Species) ) ] # How many unique taxa in total
dt_long[ Abundance > 0, .( uniqueN(Species) ) ] # How many unique taxa with Abundance > 0
# Which taxa have no with Abundance > 0
dt_long[ , .( Total_N = sum(Abundance) ), by = .(Species) ][ order(Total_N) ] %>% View()



# Sort into BioTIME format
dt_long <- dt_long[ Abundance > 0, # Remove 'zero' value abundances
                    .( Abundance, Biomass = NA, # Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
                       Family = NA, Genus = NA, Species,
                       SampleDesc = paste(year, stat, rep, sep = "_"), Plot = stat, Latitude = latitude, Longitude = longitude,
                       DepthElevation = depth, Day = NA, Month = NA, Year = year,
                       StudyID = "" ) ]
View(dt_long)

## **Note:** "SampleDesc" is a concatenation of sampling Year (year), Sampling Station (stat) and the replicate number (rep).
## I've defined Sampling Station (stat) as a BioTIME plot, because each station is given a precise lat/lon coordinate that
## doesn't change over the course of the time-series.

# The five replicate samples per station per year are described as follows in the paper:
## "In the Regional Monitoring, biological, physical, and chemical samples are taken from the bottom sediments with a 0.1-m^2
## van Veen grab. At each station, five replicates for analyses of macrobenthos are taken."










# Primary field checks -----------------------------------------------------

# 'skim' to look again at 'Primary fields'
skim(dt_long)
## Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
## Fields are logical, real values, i.e. within possible limits

# Have a little look at Abundance
hist( dt_long$Abundance, breaks = 142 )
## There are some outliers, but I have give that they are macroinvertebrates, I expect there could well be some high abundances.

# Are the outliers plausible?
dt_long[ Abundance > 400 ]
## High abundance outliers are all "Galathowenia oculata", which is a segmented worm that grows to a maximum of 50mm.
## Plus there are many samples with >400 of them recorded. All good.


## Double-check Abundances
min(dt_long$Abundance) # check the minimum (no zeroes) Y
sum(dt_long$Abundance=="") # no blanks Y


# Double-check Year
summary( dt_long[ , .( Year ) ] )



# Check the bounding area of recorded sample plots and the number of plots
dt_long[ , .( min_lat = min(Latitude), min_long = min(Longitude), max_lat = max(Latitude), max_long = max(Longitude),
              n_plot = length(unique(Plot)) ) ]
## no blanks, no NAs
## Latitude constrained from -90 to 90.
## Longitude constrained -180 to 180.


# Are they where they should be?
world_map <- map_data('world') # check whether the GPS coordinates match expectations

world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt_long, aes(x=Longitude, y=Latitude), shape=21)

# Plot the map
points + coord_fixed(xlim=c(2,4), ylim=c(55,58))
# And zoom out a bit...
points + coord_fixed(xlim=c(-10,10), ylim=c(47,64))

detach('package:maps')

# Yup, all good.







# Secondary field checks -----------------------------------------------------


## I've already sorted the "Plot" column (just the sampling station, as described above).
## Depth comes from Table 1 in the paper, also explained above.






# Taxonomic field check ---------------------------------------------------


# Set as character vector
dt_long[ , ':=' ( Species = as.character(Species) ) ][]

# And look at the unique 'species' names for visual checks
dt_long[ , .( Species = sort(unique(Species)) ) ] %>% View()


# Pull apart Family (where present), Genus and Species in the data
dt_long[ , ':=' ( Family = ifelse( Species %like% 'idae$|eae$', Species, NA ),
                  Genus = word( Species, 1 ), Species = word( Species, start=2, end=-1 ) )
        # Remove Family names from Genus column and set unresolved Species to 'sp'
        ][ , ':=' ( Genus = ifelse( Genus %like% 'idae$|eae$', NA, Genus ),
            Species = ifelse( is.na(Species), "sp", Species ) )
          # And keep only epithets for Species
          ][ , ':=' ( Species = word( Species, start=-1 ) ) ]

# Have a look
View(dt_long)

# Check resolved taxonomy data
dt_long[ , .( sort( unique(Family) ) ) ] %>% View()
dt_long[ , .( sort( unique(Genus) ) ) ] %>% View()
dt_long[ , .( sort( unique(Species) ) ) ] %>% View()



# Important taxonomic checks

# 1. Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes.

## - *No blanks, NAs or zeros*


# 2. Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.
#    If the dataset distinguishes between different uncertain IDs (Genus sp),
#    reformat them while making sure they’re recognised as separate species: like “Genus sp1” and “Genus sp2”

## - *Done where applicable*


# 3. Fit proper taxa level with the correct fields (Family, Genus, Species)

## - *Done*


# 4. Remove non-organismal records (e.g. rock, sand) and incidental taxa records.

## - *Not necessary*


# 5. Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names.

## - *Done*


# 6. Re-allocate taxonomic classifications into the appropriate column.

## - *Done*


# 7. Check for duplication and misspellings in the species names
#   (e.g. Puffnius sp, Puffinus sp; unidentified/unknown; Buenia jeffreysi / Buenia jeffreysii; double spacing; several spaces, authors name & date, etc.).
#   We’re not validating or updating species, but we do quick searches in Google Scholar to figure out which spelling is correct.

## - *Done*


# 8. Subspecies and annotated species names should be disregarded (e.g. Amphiura (acrocnida) brachiate should be considered as Acrocnida brachiate)
#    Some taxa are less resolved and are placed into species complexes, e.g. Dascyllus trimaculatus agg. or Rubus fruticosus agg.
#    We denote this with “agg.” to make sure it’s not confused with other specific epithets.

## - *Done*


# 9. If a record only has data until the genus level, it must have “sp” in species column.

## - *Done*







# Prepare for export --------------------------------------------------------

# We have to create a lot of blank columns, aggregate abundances according to site, plot, latitude, longitude, and date.
# This way we have one data record per species per sampling event.
# SampleDescription will also reflect this unique sampling event in a string.

# I've specfied "SampleDesc" above as a concatenation of sampling Year (year), Sampling Station (stat) and the replicate number (rep).

# Check for any duplicate species records/abundances per sample
dt_check <- dt_long[ , .( n = .N ), by = c("Family", "Genus", "Species", "SampleDesc")]
dt_check[ , .( unique(n) ) ]
## All good.


# Export curated data
fwrite( dt_long, file = "./Ellingsen_2017/Ellingsen_2017_TC_complete/Ellingsen_2017_RawData_TC.csv" )







