
# Curation Script ---------------------------------------------------------

# Dataset: Climate change transforms the functional identity of Mediterranean coralligenous assemblages
# Location: Mediterranean Sea
# Curator: Tadhg Carroll
# Date: 03-Aug-2023

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

# Load Original Data
dt <- base::readRDS( "./OriginalData_TC/Gomez-gras_2021_OriginalData.rds" )


# Melt data into long format
dt <- melt( dt, 1, 2:112, variable.name = "Species", value.name = "Abundance" )
## Warning is referring to different types of numeric variables. It's all good.


# Pull apart the concatenated variable that incorporates multiple variables
dt[, c("Year", "Site", "Habitat", "Plot") := data.table::tstrsplit(dt$V1, split = "_")
    ][ , ':=' ( Year = as.numeric(Year),
                Site = paste(Site,Habitat,sep="_"),
                Species = as.character(Species) ) ]


# **Input lat/lon and depth from Table S1.**
# lat          lon
# 42.379861    8.546164
# 42.379861    8.546164
# 42.379861    8.546164
# 42.379861    8.55
# 42.99125     6.383333

# lat/lon converted to Degrees/Minutes/Seconds to Decimal Degrees using online converter:
## (https://www.fcc.gov/media/radio/dms-decimal).

# 6°23'70.4"E changed to 6°23'E, as 70.4" is not in the valid range of 0-60 for seconds
## (so it may be only an approximate location, but the approximate location is correct as
##  per map in Figure S1 of Gomez-gras et al (2021)).

# Add lat/lon and depth from Table S1 of Gomez-gras et al (2021)
dt[ , ':=' ( lat = c( rep(42.379861, 4),
                      42.99125 )[ match( Site, c("Pzzu_cor","Passe_cor","Pzzu_par","Pzzinu_par","Gabin_par" ) )],
             lon = c( rep(8.546164,3),
                      8.55, 6.383333)[ match( Site, c("Pzzu_cor","Passe_cor","Pzzu_par","Pzzinu_par","Gabin_par" ) )],
             Depth_m = c(18,29,18,25,25)[ match( Site, c("Pzzu_cor","Passe_cor","Pzzu_par","Pzzinu_par","Gabin_par" ) )] ) ][]
# lat/lon and Depth (in metres) at Site level (not plots within Sites).





# Overall/Structure Checks -----------------------------------------------------

# Skim data for structure check & summary
skim(dt)
## No missing values and variables are all of the correct type.


# Sort into BioTIME format
dt <- dt[ Abundance > 0,
          # Keep only BioTIME relevant variables (and specify (missing) BioTIME column in correct order)
          .( Abundance = NA, Biomass = Abundance,
             Family = NA, Genus = NA, Species = Species,
             SampleDesc = paste( Site, Plot, Year, sep="_" ), Plot = NA,
             Latitude = lat, Longitude = lon,
             DepthElevation = Depth_m, Day = NA, Month = NA, Year = Year,
             StudyID = "" ) ]









# Primary fields ---------------------------------------------------------------

# 'skim' to look again at 'Primary fields'
skim(dt)
# Fields don’t contain blanks, zeroes, negative values, NAs, or NaNs
# Fields are logical, real values, i.e. within possible limits


# Have a little look at Biomass
hist( dt$Biomass, breaks = 142 )
# Biomasss (percent covers values) look good (none > 100%).


# Double-check Biomasss
min(dt$Biomass) # check the minimum (no zeroes) Y
sum(dt$Biomass=="") # no blanks Y

# Double-check Year
summary( dt[ , .( Year ) ] )




# Check the bounding area of recorded sample plots and the number of plots
dt[ , .( min_lat = min(Latitude), min_long = min(Longitude), max_lat = max(Latitude), max_long = max(Longitude),
         n_plot = length(unique(Plot)) ) ]

# Are they where they should be?}
world_map <- map_data('world') # check whether the GPS coordinates match expectations

world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)

# Plot the map
points + coord_fixed(xlim=c(5.5,8.5), ylim=c(41,43))
# And zoom out a bit...
points + coord_fixed(xlim=c(-14,24), ylim=c(24,64))

## Yup, matches the paper.




# Secondary fields -------------------------------------------------------------

# "DepthElevation" is the only Secondary variables (given in metres, extracted from Table S1.).






# Taxonomic fields -------------------------------------------------------------


# Have a look at the taxon names
data.table( Taxon = sort( unique(dt$Species) ) ) %>% View()
## They will take a little bit of sorting alright.


# First replace "." with " " separating Species Binomials
dt[ , ':=' ( Species = str_replace_all( Species, c("\\." = " ") ) ) ]



# Pull unique taxa out into seperate dt
dt_Sp <- data.table( Taxon = sort( unique(dt$Species) ) )
dt_Sp


# It seems there are a few different issues here. However, it looks like most of the potentially
## problematic taxa may have more than two words in their description.

# I can split these out and have a look at them separately.
## Split my number of words in taxon classification
dt_Sp_less_3 <- dt_Sp[ str_count(Taxon, '\\w+') <= 2,  ]
dt_Sp_more_2 <- dt_Sp[ str_count(Taxon, '\\w+') > 2,  ]



# I'll look at taxa with more than two words first (to make it easier on the eye)
dt_Sp_more_2$Taxon

# Specify Species/Genus/Family where possibly for "more_2" species
ID_Replacement_more_2 <-
  c( NA, "Cacospongia scalaris", "Dictyota dichotoma", "Dynamena Laomedea",
     NA, NA, "NA sp", "Falkenbergia sp",
     "Filograna implexa", NA, "Ircinia oros", NA,
     NA, NA, NA, NA,
     NA, NA, "Protula sp", NA )

ID_Family_more_2 <-
  c( NA, NA, NA, NA,
     NA, NA, "Corallinaceae", NA,
     NA, NA, NA, NA,
     NA, NA, NA, NA,
     NA, NA, NA, NA )

# Get these side-by-side with their data entries
dt_Sp_more_2[ , ':=' ( Sp_Replace = ID_Replacement_more_2, Fm_Replace = ID_Family_more_2 ) ][]


# Then have a look at taxa with two-word descriptions or fewer:
dt_Sp_less_3 %>% View()

# The following taxa are not as they should be for BioTIME
dt_Sp_less_3[ c(5, 10, 13, 17, 31, 56, 65, 82, 86) ]

# Specify replacements
ID_Replacement_less_3 <-
  c( NA, "Bryopsis sp", "Caulerpa cylindracea", "Chondria sp", NA,
     NA, NA, "Sargassum sp", "NA sp" )

ID_Family_less_3 <- c( rep( NA, 85), "Serpulidae", rep( NA, 5) )

# Get these side-by-side with their data entries
## First just copy the author taxa and add Family fix
dt_Sp_less_3[ , ':=' ( Sp_Replace = Taxon, Fm_Replace = ID_Family_less_3 ) ][]
## Then add replacements for the problematic taxa
dt_Sp_less_3[ c(5, 10, 13, 17, 31, 56, 65, 82, 86) ]$Sp_Replace <- ID_Replacement_less_3
dt_Sp_less_3




# Bind fixes together to one fix "key" dt}
dt_fix_Sp <- rbind( dt_Sp_less_3, dt_Sp_more_2 )
View( dt_fix_Sp )



# Finally, replace all 'issue taxa' in the dataset with the updated names

## Make a copied dt to hold to input fixed names
dt_name_fix <- dt

## Set "Key" variables to join by
setkey( dt_fix_Sp, Taxon )
setkey( dt_name_fix, Species )

# Join Updated taxonomic classifications to data
dt_name_fix <- dt_name_fix[ dt_fix_Sp ]

# Tidy up
dt_name_fix[ , ':=' ( Species = Sp_Replace, Sp_Replace = NULL,
                      Family = Fm_Replace, Fm_Replace = NULL ) ][]


# Run through regular BioTIME checks on updated Species Names

## Have a look at the Species binomial
dt_name_fix[ , .( Sp = sort(unique( Species )) ) ] %>% View()

# Specify Genus and Species
dt_name_fix[ , ':=' ( Genus = ifelse( Species %like% "NA", NA, word( Species, 1 ) ),
                      Species = word( Species, 2 ) ) ][]

# 'Av a look
sort(unique( dt_name_fix$Family ))
sort(unique( dt_name_fix$Genus ))
sort(unique( dt_name_fix$Species ))

# A different view
sort(unique( paste( dt_name_fix$Genus, dt_name_fix$Species, sep = " " ) ))

# Check that genus column doesn’t have family names (end in -eae or -idae) or species column has genus names
str_which(dt_name_fix$Species, 'idae$|eae$')

# Remove data that can not be resolved to Family
dt <- dt_name_fix[ !( is.na(Family) & is.na(Genus) & is.na(Species) ) ]



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

## - *Done*


# 9. If a record only has data until the genus level, it must have “sp” in species column.

## - *Done*




# Prepare for export -----------------------------------------------------------

# Check for any duplicate species records/Biomasss per sample
dt_check <- dt[ , .( n = .N ), by = c( "Family", "Genus", "Species", "SampleDesc" ) ]
dt_check[ , .( N = unique(n) ) ][  order(-N) ]
## All good



# Export curated data
fwrite( dt, file = "./Gomez-gras_2021/Gomez-gras_2021_TC_complete/Gomez-gras_2021_RawData_TC.csv" )




