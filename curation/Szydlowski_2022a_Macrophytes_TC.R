
# Curation Script ---------------------------------------------------------

# Dataset: Macrophyte abundance data for ten lakes in Vilas County, WI, USA, 1987-2020
# Location: Vilas County, WI, USA
# Curator: Tadhg Carroll
# Date: 02-Aug-2023

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
dt <- fread( "./OriginalData_TC/Szydlowski_2022a_Macrophytes_OriginalData.csv" )

# Melt data to long form (retaining relevant variables)
dt_long <-
  melt( dt,
        id.vars = c("ID", "date", "lake", "sector", "depth", "lat", "long"),
        measure.vars = c(15:63),
        variable.name = "Species",
        value.name = "Abundance" )







# Overall/Structure check ------------------------------------------------------

# Have a look at the raw data
View(dt_long)

# Skim data for structure check & summary
skim(dt_long)
## No missing/NA values for relevant variables.

# Sort into BioTIME format
# Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
dt <- dt_long[ Abundance > 0,
               .( Abundance = Abundance, Biomass = NA,
                  Family = NA, Genus = NA, Species = as.character(Species),
                  SampleDesc = paste( lake, sector, as.character(year(date)), sep = "_" ),
                  Plot = paste( lake, sector, sep = "_" ),
                  Latitude = lat, Longitude = long, DepthElevation = depth,
                  Day = day(date), Month = month(date), Year = year(date),
                  StudyID = "" ) ]
## "Plot" is a specific "sector" in a given lake, which represents a transect visited in each year of sampling.
## "SampleDesc" is a "Plot" surveyed in a given year.













# Primary fields ---------------------------------------------------------------

# 'skim' to look again at 'Primary fields'
skim(dt)
## Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
## Fields are logical, real values, i.e. within possible limits

# Have a look at Abundance
hist( dt$Abundance, breaks = 142 )
## Abundance is summed presences at marked intervals along 25m transects.
## There's a peak at 25 because this is the largest possible value.

# Double-check Abundances
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y


# Double-check Year
summary( dt[ , .( Year ) ] )



# Check the bounding area of recorded sample plots and the number of plots
dt[ , .( min_lat = min(Latitude), min_long = min(Longitude), max_lat = max(Latitude), max_long = max(Longitude),
         n_plot = length(unique(Plot)) ) ]

# Are they where they should be?
world_map <- map_data('world') # check whether the GPS coordinates match expectations

world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)

# Plot the map
points + coord_fixed(xlim=c(-91,-89), ylim=c(45,47))
# And zoom out a bit...
points + coord_fixed(xlim=c(-114,-42), ylim=c(24,64))

# All good.








# Secondary fields -------------------------------------------------------------

# I've already explained "Plot".

# Sample depths were randomly assigned at the first year of the study and are assigned in m^2.













# Taxonomic fields -------------------------------------------------------------

# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()

# Sort a few minor issues
dt[ , Species := str_replace_all( Species, c("_" = " ") )
    ][ , Species := str_replace_all( Species, c("spp" = "sp") )
       ][ , Species := str_replace_all( Species, c("Sagittaria sp emergent" = "Sagittaria sp") ) ][]

# Drop "Uknown_macroalgae" rows from data
dt <- dt[ Species != "Uknown macroalgae", ]


# Have a look at the updated Species binomial}
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()



# Fit proper taxa level with the correct fields
dt[ , ':=' ( Genus = word(Species,1), Species = word(Species,2) ) ]

# Check Genus/Species
sort(unique(dt$Genus))
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


# Why are they not unique? (i.e. Species recorded twice in a given sample...)
dt_check[ n > 1 ]
## Ah, I see. That one Genus that had "emergent" in (i.e. "Sagittaria sp emergent").

# Aggregate Counts
dt_Final <-
  dt[ , .( Abundance = sum(Abundance), Biomass = NA,
           Family = Family[1], Genus = Genus[1], Species = Species[1],
           SampleDesc = SampleDesc[1], Plot = Plot[1], Latitude = Latitude[1], Longitude = Longitude[1],
           DepthElevation = DepthElevation[1], Day = Day[1], Month = Month[1], Year = Year[1], StudyID = "" )
      , by = c("Genus", "Species", "SampleDesc" )][ , 4:17 ]





# Export curated data
fwrite( dt, file = "./Szydlowski_2022/Szydlowski_2022a_TC_complete/Szydlowski_2022a_Macrophytes_RawData_TC.csv" )







