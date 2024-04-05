
# Curation Script ---------------------------------------------------------

# Dataset: Aquatic snail abundance data for ten lakes in Vilas County, WI, USA, 1987-2020
# Location: Vilas County, WI, USA
# Curator: Tadhg Carroll
# Date: 01-Aug-2023

# Set up ------------------------------------------------------------------

# load packages
require(dplyr)
require(data.table)
require(skimr)
library(ggplot2)
require(maps)
require(stringr)
require(lubridate)


rm( list=ls() ) # clear up the environment before starting


getwd()
# Original raw data from (https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=417&revision=3)
dt <- fread( "./OriginalData_TC/Szydlowski_2022b_Snails_OriginalData.csv" )


# Melt data to long form (retaining relevant variables)
dt_long <-
  melt( dt,
        id.vars = c("ID", "date", "lake", "sector", "depth", "lat", "long"),
        measure.vars = c(16:45),
        variable.name = "Species",
        value.name = "Abundance" )


# On inspection, something's going on with the "date" values
sort(unique(dt_long$date))
## Dates for two of the sampling years are not as well resolved as the other two years.

# Extract Date info from character strings
dt_long[ , ':=' ( Year = str_extract( date, "(1|2)[0-9]{3}" ),
                  Month = str_extract( str_extract( date, "-(0)[0-9]{1}-" ), "(0)[0-9]{1}" ),
                  Day = str_extract( str_extract( date, "-[0-9][0-9]$" ), "[0-9][0-9]$" ) ) ]
## Terrible hack-y way to do it, but it works...







# Overall/Structure check ------------------------------------------------------

# Have a look at the raw data
View(dt_long)

# Skim data for structure check & summary
skim(dt_long)
## No missing/NA values for relevant variables.





# Sort into BioTIME format
dt <- dt_long[ Abundance > 0, # Remove zer-value abundances
               # Keep only BioTIME relevant variables (and specify (missing) BioTIME columns in correct order)
               .( Abundance = Abundance, Biomass = NA,
                  Family = NA, Genus = NA, Species = as.character(Species),
                  SampleDesc = paste( lake, sector, Year, sep = "_" ),
                  Plot = NA,
                  Latitude = lat, Longitude = long, DepthElevation = depth,
                  Day = as.numeric(Day), Month = as.numeric(Month), Year = as.numeric(Year),
                  StudyID = "" ) ]
## No "Plot" specified, as sample depths sometimes change slightly between years (see methods).
## "SampleDesc" is a the sector of a lake surveyed in a given year.











# Primary fields ---------------------------------------------------------------

# 'skim' to look again at Primary fields
skim(dt)


# Have a look at Abundance
hist( dt$Abundance, breaks = 142 )
## Snail Abundance is given as density per m^2.


# Double-check Abundances
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y


# Double-check Year
summary( dt[ , .( Year ) ] )



#  Check the bounding area of recorded sample plots and the number of plots
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
points + coord_fixed(xlim=c(-90,-89.5), ylim=c(45.5,46.5))
# And zoom out a bit...
points + coord_fixed(xlim=c(-114,-42), ylim=c(24,64))

## Yup






# Secondary fields -------------------------------------------------------------

# Sample depths were randomly assigned at the first year of the study and are assigned in m^2,
## associated with a given "sector" in a given year.










# Taxonomic fields -------------------------------------------------------------

# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()


# Sort a couple of minor issues in taxonomic data
dt[ , Species := str_replace_all( Species, c("_" = " ") )
    ][ , Species := str_replace_all( Species, c("spp" = "sp") ) ]


# Drop taxa "Unknown"
dt <- dt[ Species != "Unknown", ]


# Have a look at the updated Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()
## All good


# Fit proper taxa level with the correct fields
dt[ , ':=' ( Genus = word(Species,1), Species = word(Species,2) ) ]

# Check Genus/Species
sort(unique(dt$Genus))
sort(unique(dt$Species))

# Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names
str_which(dt$Species, 'idae$|eae$')

View(dt)


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

## - *Not applicable*


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

## - *Not applicable*


# 9. If a record only has data until the genus level, it must have “sp” in species column.

## - *Done*




# Prepare for export --------------------------------------------------------

# Check for any duplicate species records/abundances per sample
dt_check <- dt[ , .( n = .N ), by = c("Genus", "Species", "SampleDesc" )]
dt_check[ , .( unique(n) ) ]
## All good


# Export curated data
fwrite( dt, file = "./Szydlowski_2022/Szydlowski_2022b_TC_complete/Szydlowski_2022b_Snails_RawData_TC.csv" )



