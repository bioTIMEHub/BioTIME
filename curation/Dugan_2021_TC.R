
# Curation Script ---------------------------------------------------------

# Dataset: SBC LTER: Beach: Time series of abundance of birds on selected beaches, ongoing since 2008
# Location: Santa Barbara Coastline - USA
# Curator: Tadhg Carroll
# Date: 21-Jul-2023

# Set up ------------------------------------------------------------------

# load packages
require(dplyr)
require(data.table)
require(skimr)
library(ggplot2)
require(maps)
require(stringr)
library(lubridate)

rm( list=ls() ) # clear up the environment before starting

getwd()
# Read in Original raw data from Dryad
dt <- fread( "./OriginalData_TC/Dugan_2021_OriginalData.csv" ) %>%
  data.table()



# Structure check ---------------------------------------------------------

# Note: "-99999" in raw data means "value not recorded or not available", which needs fixing (Replace with NA).
## Use .SD to replace"-99999" values with NA in all columns of dt
dt <- dt[ , replace( .SD, .SD == -99999, NA ) ]


# Also need to get rid of observations of marine mammals, people, and dogs, keeping only observations of birds...
dt <- dt[ TAXON_CLASS == "Aves" ]



# Have a look at the raw data
View(dt)


# Skim dt_long for structure check & summary
skim(dt)
## Apart from "Genus" and "Species", variables are of required type with no missing values.

# Which bird observations are missing Genus/Species values?
unique( dt[ is.na(TAXON_SPECIES) ]$COMMON_NAME )
unique( dt[ is.na(TAXON_SPECIES) ]$TAXON_FAMILY )
## No worries, can just have these resolved to Family level when I do Taxonomy below.




# Coordinates are given as seperate metadata at data source (https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-sbc.51.10).
## Add in lat/lon
dt[ , ':=' ( latitude =
               c(34.40305, mean(34.39452, 34.391151), 34.40928,
                 34.470467, 34.410767, 34.408533)[ match( SITE, c("ABB", "CSB-CCB", "IVWB", "AQB", "EUCB", "SCLB") ) ],
             longitude =
               c(-119.74375, mean(-119.52699, -119.521236), -119.87385,
                 -120.118617, -119.842017, -119.551583)[ match( SITE, c("ABB", "CSB-CCB", "IVWB", "AQB", "EUCB", "SCLB") ) ] ) ][]

## **Note:** One of the transects runs over two beaches ("CSB-CCB"). The lat/lon is provided for each beach separately in the metadata,
## so the mean of the two is taken here (following Alban in the "Metacommunity" data curation script).


# Sort into BioTIME format
dt <- dt[ TOTAL > 0, # Remove zero abundance counts
          .( Abundance = TOTAL, Biomass = NA, # Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
             Family = TAXON_FAMILY, Genus = TAXON_GENUS, Species = TAXON_SPECIES,
             SampleDesc = NA, Plot = SITE, Latitude = latitude, Longitude = longitude,
             DepthElevation = NA, Day = day( mdy(DATE) ), Month = MONTH, Year = YEAR,
             StudyID = "" ) ]

## **Note:** From methods: "A standard 1 kilometer alongshore transect has been established at each of the study beaches."
## Therefore each of these transects has been designated a BioTIME "Plot".







# Primary field checks -----------------------------------------------------

# 'skim' to look again at 'Primary fields'
skim(dt)
## Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
## Fields are logical, real values, i.e. within possible limits

# Have a little look at Abundance
hist( dt$Abundance, breaks = 142 )

# Are the outliers plausible?
dt[ Abundance > 200 ]
## Some very high abundances, but they seem to be the same and similar species recorded with high counts.
## I have no good reason to think that there weren't large flocks of some birds occasionally.

## Double-check Abundances
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y


# Double-check Year
summary( dt[ , .( Year ) ] )



# Check the bounding area of recorded sample plots and the number of plots
dt[ , .( min_lat = min(Latitude), min_long = min(Longitude), max_lat = max(Latitude), max_long = max(Longitude),
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

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)

# Plot the map
points + coord_fixed(xlim=c(-124,-117), ylim=c(32,36))
# And zoom out a bit...
points + coord_fixed(xlim=c(-156,-42), ylim=c(7,84))

detach('package:maps')

# Yup, all good.







# Secondary field checks -----------------------------------------------------


## I've already sorted the "Plot" column (just the transect, as described above).
## This is the only secondary field.






# Taxonomic field check ---------------------------------------------------


# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( paste( Genus, Species) )) ) ] %>% View()


# Replace "spp" with "sp"
dt[ , ':=' ( Species = str_replace_all( Species, "spp", "sp" ) ) ][]



# Important taxonomic checks

# 1. Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes.

## - *Done*


# 2. Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.
#    If the dataset distinguishes between different uncertain IDs (Genus sp),
#    reformat them while making sure they’re recognised as separate species: like “Genus sp1” and “Genus sp2”

## - *Done*


# 3. Fit proper taxa level with the correct fields (Family, Genus, Species)

## - *Done*


# 4. Remove non-organismal records (e.g. rock, sand) and incidental taxa records.

## - *Done in initial checks above*


# 5. Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names.

str_which(dt$Species, 'idae$|eae$')

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


# Specify "SampleDesc"
dt[ , ':=' ( SampleDesc = paste( Plot, Day, Month, Year, sep = "_" ) ) ][]

# Check for any duplicate species records/abundances per sample
dt_check <- dt[ , .( n = .N ), by = c("Family", "Genus", "Species", "SampleDesc" )]
dt_check[ , .( unique(n) ) ]
## All good


# Export curated data
fwrite( dt, file = "./Dugan_2021/Dugan_2021_TC_complete/Dugan_2021_RawData_TC.csv" )






