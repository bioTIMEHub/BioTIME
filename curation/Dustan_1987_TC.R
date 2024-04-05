
# Curation Script ---------------------------------------------------------

# Dataset: Changes in the reef-coral community of Carysfort reef, Key Largo, Florida: 1974 to 1982
# Location: Key Largo, Florida, USA
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
# Original raw data for Table 3 of Dustan & Halas (1987)
dt <- fread( "./OriginalData_TC/Dustan_1987_OriginalData.csv", skip = 1L, header = TRUE )

# Data need some reformatting

# Melt data into long form
dt <- data.table::melt(dt, id.vars = c("species"), value.name = "tmp", variable.name = "plot")

# Replace authors symbols with which sampling period a species was observed
dt[ , tmp := c("historical", "modern", "historical+modern")[match(tmp, c("x", "o", ".")) ]][]

# Seperate out observations from each sampling period
dt[, c("tmp1", "tmp2") := data.table::tstrsplit(tmp, "\\+")][]

# Split records into long form
dt <- data.table::melt(dt, id.vars = c("species", "plot"), measure.vars = c("tmp1", "tmp2"), value.name = "period" )

# And year of each sampling period
dt[ , year := c(1975L, 1983L)[match(period, c("historical", "modern"))] ][]

# Remove absences from data
dt <- dt[ !is.na(period) ]




# Structure check ---------------------------------------------------------

# Have a look at the raw data
View(dt)

# Skim dt_long for structure check & summary
skim(dt)
## No missing values and variables are all of the correct type.

# Sort into BioTIME format
dt <- dt[ , .( Abundance = 1, Biomass = NA, # Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
               Family = NA, Genus = NA, Species = species,
               SampleDesc = paste(year, plot, sep = "_" ), Plot = paste(plot, sep = "_" ),
               Latitude = 25.216667, Longitude = -80.216667,
               DepthElevation = NA, Day = NA, Month = NA, Year = year,
               StudyID = "" ) ]
# lat/lon from Dustan & Halas (1897) Figure 1, converted to Degrees/Minutes to Decimal Degrees.
# BioTIME "Plot" is one of the authors fixed permanent transects, and "SampleDesc" is a transect visit in a given sampling year.






# Primary field checks -----------------------------------------------------

# 'skim' to look again at 'Primary fields'
skim(dt)

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
points + coord_fixed(xlim=c(-81,-79), ylim=c(24,26))
# And zoom out a bit...
points + coord_fixed(xlim=c(-114,-42), ylim=c(14,42))

# Correctly located








# Secondary field checks -----------------------------------------------------


# I've already sorted the "Plot" column (which is just the fixed transects).

# But have a look at "Plot" variable
unique(dt$Plot)





# Taxonomic field check ---------------------------------------------------

# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()
# All good except for "Madracis spp."

# Replace "spp" with "sp"
dt[ , ':=' ( Species = str_replace_all( Species, c("Madracis spp." = "Madracis sp") ) ) ][]

# Fit proper taxa level with the correct fields
dt[ , ':=' ( Genus = word(Species,1), Species = word(Species,2) ) ][]

# Check Genus/Species
sort(unique(dt$Genus))
sort(unique(dt$Species))

# Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names
str_which(dt$Species, 'idae$|eae$')



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
fwrite( dt, file = "./Dustan_1987/Dustan_1987_TC_complete/Dustan_1987_RawData_TC.csv" )




