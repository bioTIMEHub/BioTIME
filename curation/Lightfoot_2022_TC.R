
# Curation Script ---------------------------------------------------------

# Dataset: Lizard pitfall trap data from 11 NPP study locations at the Jornada Basin LTER site, 1989-2006
# Location: Jornada Basin LTER site, Chihuahaun desert, New Mexico, USA
# Curator: Tadhg Carroll
# Date: 26-Jul-2023

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
# Read in Original raw data from: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-jrn.210007001.38
dt <- fread( "./OriginalData_TC/Lightfoot_2022_OriginalData.csv" )

# Load Lizard names
dt_tax <- fread( "./OriginalData_TC/Lightfoot_2022_OriginalData_Lizardcodelist_edit_TC.txt" )

# I edited the "Lizardcodelist" text file which provides the key for species names, just by adding commas and deleting some
# header lines so it would load easily.
# I did not change any data in the file. Original unedited file provided as "Lightfoot_2022_OriginalData_Lizardcodelist.txt",
# edited file provided as "Lightfoot_2022_OriginalData_Lizardcodelist_edit_TC.txt".







# Overall/Structure check ---------------------------------------------------------

# Have a look at the raw data
View(dt)
View(dt_tax)

# What plots?
sort(unique(dt$plot))
## Good


# Do all species codes correspond between the two datasets?
unique(dt$spp[!(dt$spp %in% dt_tax$CODE)])
unique(dt_tax$CODE[!(dt_tax$CODE %in% dt$spp)])

# There are three codes in spp that are not in accompanying "Lizardcodelist.txt" file ("BUDE","NONE","SOSE").
## These comprise quite a lot of the data (~30.21%):

# Have a look at the data with spp codes not included
dt[ spp %in% c("BUDE","NONE","SOSE") ] %>% View()
dt[ spp %in% c("NONE") ] %>% View()


# The data file "lizrdpit.his.txt", a detailed historical list of field notes, documents what these codes refer to as follows:
## BUDE: bufo. debilis
## NONE: Seems to be a general error code (i.e. something went wrong with trap)
## SOSE: Sonora semiannulata=western ground

## So that's a toad, a snake, and an error code.

# 30.16% of the raw data are comprised of the "NONE" error code category.
## They need to be dropped for BioTIME.

# "0" = missing value for variable "pit" (see "lizrdpit.dsd" text file).

# Look at problem codes and missing pitfall traps noted
dt[ pc == 1 ] %>% View() # "Problem Code"
dt[ pit == 0 ] %>% View() # pit == missing
## Note that "pit == missing" is covered by the non-lizard problem code "NONE", so we don't need to account for it once we account for "NONE".


# Drop non-lizard data entries and join in species names
# Set keys by which to join the data.tables
setkey(dt, spp)
setkey(dt_tax, CODE)

# Join the species names into the raw data, excluding non-lizard codes
dt <- dt_tax[ dt[ !(spp %in% c("BUDE","NONE","SOSE")) ] ]
View(dt)

# Check codes after join
sort(unique(dt$CODE))
## All good


# Add variables "year" and "month" extracted from dates
dt[ , ':=' ( year = year(date), month = month(date) ) ][]


# Skim data for structure check & summary
skim(dt)

# There are some missing values in possibly relevant variables. Have a look at them.
dt[ is.na(`SCIENTIFIC NAME`) ] %>% View()
dt[ is.na(plot) ] %>% View()
dt[ is.na(pit) ] %>% View()

# Will need to drop records with "plot" = NA, as this will be the level of data aggregation,
## but missing "pit" values are ok (as can still aggregate by "plot").

# Don't drop records with "SCIENTIFIC NAME" = NA yet as some of them can be identified to Genus via "COMMON NAME"
## variable (put these into "SCIENTIFIC NAME" slot now, so I can get data into BioTIME format).

# Drop the five records with "plot" = NA
dt <- dt[ !(is.na(plot)) # and set `SCIENTIFIC NAME` to `COMMON NAME` where `SCIENTIFIC NAME` value is missing
          ][ ,  `SCIENTIFIC NAME` := ifelse( is.na(`SCIENTIFIC NAME`), `COMMON NAME`, `SCIENTIFIC NAME` ) ]
View(dt)

# Check name change worked
sort(unique(dt$`SCIENTIFIC NAME`))

# Skim data again for structure check & summary
skim(dt)

# No missing/NA values for relevant variables.


# Sort into BioTIME format
dt <- dt[ , .( Abundance = .N, Biomass = NA, # Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
               Family = NA, Genus = NA, Species = `SCIENTIFIC NAME`,
               SampleDesc = paste(year, month, zone, site, plot, sep = "_" ), Plot = paste(zone, site, plot, sep = "_" ),
               Latitude = mean(32.669000,32.488000), Longitude = mean(-106.865000,-106.713000),
               DepthElevation = NA, Day = NA, Month = month, Year = year,
               StudyID = "" ),
          , by = c("year","month", "zone", "site", "plot", "SCIENTIFIC NAME")][ , 7:20 ] # Subset cols 7:20 to remove Original Data grouping variables
View(dt)

# I've input lat/long as the site centroid of the bounding box the authors provided (N: 32.669, S: 32.488, E: -106.713, W: -106.865):
## (https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-jrn.210007001.38)
## This is slightly coarse at the plot level, but it's definitely a small enough area for the purposes of BioTIME.

# A BioTIME "Plot" is a concatenation of "zone", "site" and "plot" (the authors 'plot' being one of the permanently placed sampling grids of 16 pitfall
## traps within sites - the level at which I've aggregated the species Abundance data per sampling month to get Abundance counts).

# "SampleDesc" is a concatenation of "year","month", "zone", "site" and "plot" because each plot was sampled for a two week period in a given month during
## which the traps were consecutively left open and checked every three days or so (note that this was done every month between 1989 and 1991, and once
## every three months thereafter). "zone" is not technically needed in this concatenation, but it provides information on the vegetation type in which a
## plot is located with reference back to original study metadata (https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-jrn.210007001.38);
## this is also the case for the BioTIME "Plot" concatenation).







# Primary field checks -----------------------------------------------------

# 'skim' to look again at 'Primary fields'}
skim(dt)
## Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
## Fields are logical, real values, i.e. within possible limits


# Have a look at Abundance
hist( dt$Abundance, breaks = 42 )
## Nice reassuring SAD I'd say

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
points + coord_fixed(xlim=c(-109,-103), ylim=c(30,35))
# And zoom out a bit...
points + coord_fixed(xlim=c(-142,-67), ylim=c(1,64))

## Perfect







# Secondary field checks -----------------------------------------------------

# Already sorted secondary fields in compiling the data above.











# Taxonomic field check ---------------------------------------------------

# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()


# Have a look at data with uncertain IDs
dt[ Species %in% c("UNKNOWN Cnemidophorus","UNKNOWN lizard") ] %>% View()


# Remove "UNKNOWN lizard" records
dt <- dt[ Species != "UNKNOWN lizard" ]


# Specify Genus and Species
dt[ , ':=' ( Genus = ifelse( Species %like% "UNKNOWN", "Cnemidophorus", word(Species,1) ),
             Species = ifelse( word(Species,2) == "Cnemidophorus", "sp", word(Species,2) ) ) ][]
# Records for one taxonomic entity could be identified to Genus via "COMMON NAME" variable in original data,
## which read "UNKNOWN Cnemidophorus".

# 'Av a look
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

## - *Done*


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
fwrite( dt, file = "./Lightfoot_2022/Lightfoot_2022_TC_complete/Lightfoot_2022_RawData_TC.csv" )






