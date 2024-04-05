
# Curation Script ---------------------------------------------------------

# Dataset: Point-count bird censusing: long-term monitoring of bird abundance and diversity in central Arizona-Phoenix, ongoing since 2000
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
require(lubridate)
require(geosphere)
require(auk)


rm( list=ls() ) # clear up the environment before starting




# Load and configure data --------------------------------------------------

getwd()
# Read in Original raw data for bird survey point counts
dt <- dt <- fread( "./OriginalData_TC/Warren_2022_OriginalData_1_BirdCounts.csv" )
# Read in Original raw data from survey locations
dt_survey_locations <- fread( "./OriginalData_TC/Warren_2022_OriginalData_2_SurveyLocations.csv" )


# Get the Spatial data integrated into the Point Counts Data

## Do the site codes match up completely between data.tables?
sum( !( dt$site_code %in% dt_survey_locations$site_code ) )
sum( !( dt_survey_locations$site_code %in% dt$site_code ) )
## Yup

## Have a look at variable names
names(dt)
names(dt_survey_locations)


# They tell us in the methods that some survey sites have been moved a little at some point during the time-series for various reasons.

# They also tell us that: "Survey sites that have moved have an associated end date corresponding to when the position moved or was
## recalibrated, either as a date or month and year if the exact day of repositioning is not known. Begin dates correspond to the earliest
## survey at that position or, in the case of sites that have moved (i.e., have an end date), the earliest survey after the end date."

# This makes it a bit tricky to merge the coordinates to the bird observations with the data in their current form, as the 'merge key'
## is the "site_code" column, which has two sets of coordinates for a subset of sites.


# We'll need to go through a few steps to sort this:

# First split out sites that have and have not changed location during the time-series

## Which sites have and haven't changed location?
which_duplicated <- dt_survey_locations$site_code[ duplicated( dt_survey_locations$site_code ) ]
which_not_duplicated <- dt_survey_locations$site_code[ !(duplicated( dt_survey_locations$site_code) ) ]

# Split them out into two datasets
## Survey sites that have changed locations at some point
dt_changed_locations <- dt_survey_locations[ site_code %in% which_duplicated ]
dt_changed_locations
## Survey sites that have remained in precisely the same position every year
dt_constant_locations <- dt_survey_locations[ !(site_code %in% which_duplicated) ]
dt_constant_locations


# For all the survey sites that have moved position (sites in "dt_changed_locations"), and therefore have two entries in the "dt_survey_locations"
## data.table, any row that has a 'non-NA' value for "end_date_year", corresponds to the location of the site *before* the change of location.
## The one exception to this is row 110 in "dt_survey_locations", which has a value for "end_date_year", but corresponds to the site *after* it
## moved (because it moved and then got discontinued in 2016).

#  Specify whether rows correspond to sites in their orignial or changed location
# Specify which is the original/new location for sites that have changed coordinates to some extent over the duration of the survey period
dt_changed_locations[ , ':=' ( location = ifelse( is.na(end_date_year), "moved", "original" ),
                               relocated = "Yes" ) ][]
dt_changed_locations$location[26] <- "moved" # Modify the one site the above fix does not work for

# Specify that the rest of the sites are in the original location
dt_constant_locations[ ,  ':=' ( location = "original", relocated = "No" ) ][]

# Join them back together with the new variable
dt_survey_locations_update <- rbind( dt_changed_locations, dt_constant_locations )
dt_survey_locations_update[ , site_code_modified := paste( site_code, location, sep = "_" ) ][]


# Now I need to get these onto 106 lines, one for each unique site, using dcast...
## Get back into wide form with 1 row per "site_code"
dt_survey_locations_update <-
  dcast( dt_survey_locations_update, site_code + location_type + relocated ~ location,
         value.var = c("lat","long",
                       "begin_date", "begin_date_month", "begin_date_year",
                       "end_date", "end_date_month", "end_date_year") )
dt_survey_locations_update


# How far have the relocated sites moved
dt_survey_locations_update[ relocated == "Yes",
                            .(dist = as.numeric( distm(c(long_original, lat_original),
                                                       c(long_moved, lat_moved), fun = distHaversine) ) ),
                            by = site_code ][order(-dist)]
## Not far at all in most cases. However, sites "Ave67_dwn_B1" and "Ave67_dwn_B1" have moved 205 & 306 metres respectively, which is quite a change.


# Now that I have one row per "site_code" in the locations data.table, I can merge with bird observations data.table.
setkey( dt, site_code )
setkey( dt_survey_locations_update , site_code )

dt_merge <-  dt[ dt_survey_locations_update ]
dt_merge

# For sites that have changed location but don't have an end date, compile an end date from the month/year they changed
dt_merge[ , end_date_original := as.character(end_date_original)
         ][ , end_date_original := ifelse( relocated == "Yes" & is.na(end_date_original),
                                  "2016-3-30", end_date_original )
           ][, ':=' ( end_date_original = ymd(end_date_original), survey_date = ymd(survey_date) ) ][]

# Specify coordinates taking into account sites reported to have been relocated at some point in time
dt_merge[ , ':=' ( Latitude = ifelse( relocated == "Yes" & survey_date >= end_date_original,
                                      lat_moved, lat_original ),
                   Longitude = ifelse( relocated == "Yes" & survey_date >= end_date_original,
                                       long_moved, long_original ),
                   Plot = ifelse( relocated == "Yes" & survey_date >= end_date_original,
                                  paste( site_code, "New", sep = "__"), site_code ))][]



# Have a look at the "Plot" names for unique survey coordinates
sort(unique(dt_merge$Plot))
## It seems that this redistribution doesn't work for at least some of the plots that are supposed to have changed location.
## There are 118 unique "Plot" values, rather than 121...

# These are the three sites it seems not to have worked for:
## - "Ave67_dwn_B1__New" (doesn't have a "original location" value)
## - "Ave67_mid_B1" (doesn't have a "New" value)
## - "U-21__New" (doesn't have a "original location" value)

# Why are the three sites not showing up as having changed location?
dt_merge[ site_code == "Ave67_dwn_B1", .( dates = sort(unique(survey_date))) ]
dt_merge[ site_code == "Ave67_mid_B1", .( dates = sort(unique(survey_date))) ]
dt_merge[ site_code == "U-21", .( dates = sort(unique(survey_date))) ]
## - "Ave67_dwn_B1": There are no recorded survey dates *before* the purported change of location (March 2016).
## - "Ave67_mid_B1": There are no recorded survey dates *after* the purported change of location (March 2016).
## - "U-21": There are no recorded survey dates *before* the purported change of location (March 2016).

# Does this hold in the original (unmerged) dataset, or have I done something wrong?
dt[ site_code == "Ave67_dwn_B1", .( dates = sort(unique(survey_date))) ]
dt[ site_code == "Ave67_mid_B1", .( dates = sort(unique(survey_date))) ]
dt[ site_code == "U-21", .( dates = sort(unique(survey_date))) ]
# It does hold up there too, there must just be something weird about the way it's reported in the original data.
## Maybe some records from these sites pre/post move didn't make it into the released data for some reason.


# Why are there 118 "Plots" but  only 117 unique sets of Coords (i.e., plot locations)?
# Get lat/lon for each "plot" as determined above
dt_merge[ , .( lat = Latitude[1], lon = Longitude[1] ), by = "Plot"
          ## Make concatenated lat_lon variable
         ][ , .( Plot, lat, lon, lat_lon = paste(lat,lon, sep = "_") )
          ## Find the two plots located in precisely the same place...
          ][ , .( Plot, lat, lon, lat_lon, dup_coords = duplicated(lat_lon)) ] %>% View()

## "Ave67_dwn_B1" and "Ave67_mid_B1" are supposed to be two separate sites, both of which have been given two distinct sets of
## coordinates each in the "46_bird_survey_locations.csv" file. However, one of the two sets of coordinates attributed to each of them
## is given for *both* sites ( that is, coordinate[ lat: 33.39764,	lon: -112.2040 ] ). So I really don't know which this is supposed to be...

# As we saw just above, the unique "Plots" attributed to each of these two sites from the dates on which they were surveyed are:
## - "Ave67_dwn_B1__New" (doesn't have a "original location" value)
## - "Ave67_mid_B1" (doesn't have a "New" value)

# Also:
## - "Ave67_dwn_B1": There are no recorded survey dates *before* the purported change of location (March 2016).
## - "Ave67_mid_B1": There are no recorded survey dates *after* the purported change of location (March 2016).

# This would suggest, that these were considered the same site, but were labeled as having changed when they moved...
## There are three unique site locations given for them in the "survey locations" data, but only two in the actual survey data.

# However, each of these sites is suggested to have moved quite a distance during relocation, whereas from the actual survey
## data there is only data from one spatial point between the two of them:
## It's really all quite strange...

#==# A Potential Solution**
# I think the best solution is to keep the BioTIME "Plot" entries for these two sites, as well as their associated "Latitude" & "Longitude"
## values, specified as NA, due to the lack of clarity.

## The data can still be used to specify point counts as unique samples because we can use recorder ID & survey date in conjunction with the
## site code to specify a unique sample in the bird observations data.

# Set problematic site names to NA
dt_merge[ , ':=' ( Plot = ifelse( Plot %in% c("Ave67_dwn_B1", "Ave67_mid_B1"), NA, Plot ),
                   Latitude = ifelse( Plot %in% c("Ave67_dwn_B1", "Ave67_mid_B1"), NA, Latitude ),
                   Longitude = ifelse( Plot %in% c("Ave67_dwn_B1", "Ave67_mid_B1"), NA, Longitude ) ) ][]



# One more problem to sort out:

# What represents a unique survey?

## It's denoted in the data as "survey_id"
length(unique(dt_merge$survey_id))

## But I've noticed that some sampling instances have more than one "survey_id"
## (I assume due to mistakes during data entry)

# Which surveys have more than one survey-ID?
dt_issue <-
  dt_merge[ , .( num_surveys = uniqueN(survey_id), unique_sid = list(survey_id) ),
            by = .( site_code, survey_date, observer, time_start, time_end )
  ][ num_surveys > 1 ]
dt_issue

# Extract the list of inconsistent survey IDs
issue_surveys <-
  unique( as.numeric( c( dt_issue$unique_sid[[1]],
                         dt_issue$unique_sid[[2]],
                         dt_issue$unique_sid[[3]],
                         dt_issue$unique_sid[[4]] ) ) )
issue_surveys

# Drop inconsistent survey-IDs from the Bird Counts data
dt <- dt_merge[ !( survey_id %in% issue_surveys ) ]
dt



# So now that the site locations and Survey_IDs are sorted out, we can finally move onto the more specific BioTIME checks...

# First, sort into BioTIME format
dt <- dt[ , .( Abundance = bird_count, Biomass = NA, # Keep only BioTIME relevant variables
               ## Keep species codes stored in "Genus" for now (to look at later when sorting taxonomic stuff)
               Family = NA, Genus = code, Species = common_name,
               SampleDesc = paste( site_code, survey_date, observer, time_start, time_end, sep = "_" ),
               Plot = Plot, Latitude = Latitude, Longitude = Longitude,
               DepthElevation = NA,
               Day = day( survey_date ), Month = month( survey_date ), Year = year(survey_date ),
               StudyID = "" ) ]
dt







# Overall/Structure check ------------------------------------------------------


# Skim data for structure check & summary
skim(dt)
## Missing "Plot" observations match missing lat/lons, as they should be (above).
## Will get to Abundance next.







# Primary fields ---------------------------------------------------------------

# Have a look at Abundance

## All records
hist( dt$Abundance, breaks = 242 )
## Counts <50
hist( dt[Abundance<50]$Abundance, breaks = 50 )

# Data with large counts
dt[ Abundance > 500 ] %>% View()
dt[ Abundance > 100 ] %>% View()

# Which species have very large counts?
dt[ Abundance > 500, .N, by = "Species" ][ order(-N) ] %>% View()
dt[ Abundance > 100, .N, by = "Species" ][ order(-N) ] %>% View()

# Some of these seem like outrageously large numbers, but a bit of googling, and repeated large counts of many of
## the same species, makes me think that they are probably ok.

# Drop 239 records with NA for Abundances
dt <- dt[ !(is.na(Abundance)) ]

# Double-check Abundances
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y



# Double-check Year
summary( dt[ , .( Year ) ] )

# Note from Data Source: "The first year of the project (2000) was generally a pilot year in which each site was
## visited approximately twice by a varying number of birders. The monitoring became more formalized beginning in 2001,
## and each site was visited in each of four seasons by three birders. The frequency of visits was reduced to three seasons
## in 2005, and to two season (spring, winter) beginning in 2006."

# That's nothing to say exclude the year 2000 I think, but people might want to take note for (e.g.) rarefaction, as you
## could end up throwing away a lot of data if not careful, given that the first year had fewer visits.


# Check the bounding area of recorded sample plots and the number of plots
dt[ , .( min_lat = min(Latitude, na.rm = T), min_long = min(Longitude, na.rm = T),
         max_lat = max(Latitude, na.rm = T), max_long = max(Longitude, na.rm = T),
         n_plot = length(unique(Plot)) ) ]

# Are they where they should be?
world_map <- map_data('world') # check whether the GPS coordinates match expectations

world <- ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

points <- world + geom_point( data=dt_merge, aes( x=Longitude, y=Latitude, colour = relocated ), shape=21 )

# Plot the map
points + coord_fixed(xlim=c(-112.84,-111.5), ylim=c(33,34))
# And zoom out a bit...
points + coord_fixed(xlim=c(-142,-67), ylim=c(1,64))








# Secondary fields ---------------------------------------------------------

# Already sorted secondary fields in compiling the data above.







# Taxonomic field check ---------------------------------------------------

# Have a look at the Species binomial
dt[ , .( Sp = sort(unique( Species )) ) ] %>% View()


# The authors provided common names rather than scientific names. However, given that it's birds, scientific names will be
## retrievable.

# Pull out the common names provided
dt_Sp <- dt[ , .( Sp_Common = sort(unique( Species )) ) ]
dt_Sp

# Use ebird package to get scientific names from common names
dt_Sp[ , Sp_Scientific := ebird_species( Sp_Common, type = c("scientific") ) ]
View(dt_Sp)

# Which Species did ebird not find for us?
dt_Sp[ is.na( Sp_Scientific) ]
dt_Sp[ Sp_Common %like% "Unidentified" ]

# 68 of the 308 species could not be ID'd using the "ebird_species()" function. Of these, 51 begin with "Unidentified",
## which are generally unresolved to species but should mostly be identifiable to Genus or Family level.

# The remaining 17 were species for which binomials did not match the common name used in the ebird database for
## various reasons, including different spellings.

#We'll have a look at the latter 17 first with Google searches:

# Which common names did ebird not recognise?
dt_Sp_NA <- dt_Sp[ is.na( Sp_Scientific) & !( Sp_Common %like% "Unidentified" ) ]
dt_Sp_NA

# Fixes for Common Names not recognised by ebird
Sp_NA_Search <-
  c( "Anas crecca", "Setophaga coronata", "Dendrocygna autumnalis", "Anser cygnoides", "Serinus canaria",
     "Columbina passerina", "Anas platyrhynchos", "Anser sp", "Junco hyemalis", "Setophaga coronata", "Junco hyemalis",
     "Empidonax difficilis", "Junco hyemalis", "Rallus obsoletus", "Artemisiospiza sp", "Junco hyemalis", "Empidonax difficilis" )

dt_Sp_NA[ , Sp_Scientific := Sp_NA_Search ## And see what ebird turns up as common names using scientific names found for these species
         ][ , Sp_Common_Ebird:= ebird_species( Sp_Scientific, type = c("common") )][]

# The ebird common names tally up nicely with the search results, but with the following notes:

# wrt "Audubon's Warbler":
## "At present, the American Ornithological Society and Clements considers the myrtle, Audubon's, and Goldman's warbler
## three subspecies of the yellow-rumped warbler (Setophaga coronata coronata and Setophaga coronata auduboni, and
## Setophaga coronata goldmani respectively) while the IOC World Bird List classifies the myrtle warbler, Audubon's,
## and Goldman's warbler as separate species (Setophaga coronata, Setophaga auduboni, and Setophaga goldmani)."

## - So I've lumped these and the "Myrtle Warbler" together

# The"Gray-headed Junco", the "Oregon Junco", the "Pink-sided Junco", and the "Slate-colored junco" have been combined
## as subpsecies of the same species (the "Dark-eyed junco": Junco hyemalis).

# Sage sparrow was the name of a species of sparrow that has since been reclassified as two species:
## - Sagebrush sparrow, Artemisiospiza nevadensis
## - Bell's sparrow, Artemisiospiza belli

# So I've left this classified to Genus level.








# Next, Species for which records start with "Unidentified"

# Which Species records start with "Unidentified"?
dt_Sp_Unidentified <- dt_Sp[ ( Sp_Common %like% "Unidentified" ) ]
dt_Sp_Unidentified

# Look up Genus and Family names for these taxonomic records:
ID_Species <-
  c( rep( "sp", 49 ), "coronata", "sp" )

ID_Genus <-
  c( "Accipiter", "NA", "NA", "Buteo", "Calidris", "Chordeiles", "NA", "Molothrus", "Columbidae", "Limnodromus",
     "NA", "NA", "NA", "Empidonax", "NA", "Colaptes", "NA", "NA", "NA", "NA",
     "NA", "NA", "NA", "NA", "NA", "NA", "Tyrannus", "NA", "Myiarchus", "NA",
     "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
     "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "Setophaga",
     "NA" )

ID_Family <-
  c( "NA", "NA", "NA", "NA", "NA", "NA", "Phalacrocoracidae", "NA", "NA", "NA",
     "Anatidae", "Ardeidae", "Ardeidae", "NA", "Falconidae", "NA", "Muscicapidae", "Polioptilidae", "Anatidae", "Laridae",
     "Accipitridae", "Ardeidae", "NA", "Trochilidae", "Threskiornithidae", "Corvidae", "NA", "NA", "NA", "NA",
     "NA", "NA", "NA", "NA", "Scolopacidae", "NA", "NA", "Recurvirostridae", "Hirundinidae", "Apodidae",
     "Thraupidae", "Mimidae", "Passerellidae", "Vireonidae", "NA", "NA", "NA", "Picidae", "Troglodytidae", "NA",
     "Scolopacidae" )



# Parse the updated Scientific Names back together into an overall "Key" to use with Common Names provided by authors.

## Species that ebird found (i.e. no searches needed)
dt_fix_default <- dt_Sp[ !(is.na( Sp_Scientific)) ]
dt_fix_default[ , Family_fix := NA ]

# Species that ebird did not find (without "Unidentified" in name)
dt_fix_NA <- dt_Sp_NA[ , 1:2 ]
dt_fix_NA[ , Family_fix := NA ]

# Species that ebird did not find (with "Unidentified" in name)
dt_fix_unidentified <- data.table( Sp_Common = dt_Sp_Unidentified$Sp_Common,
                                   Sp_Scientific = paste( ID_Genus, ID_Species, sep = " " ),
                                   Family_fix = ID_Family)

# Bind them all together
dt_fix_Sp <- rbind( dt_fix_default, dt_fix_NA, dt_fix_unidentified )
dt_fix_Sp


# Set character "NA" in Family_fix to proper NA
dt_fix_Sp[ , Family_fix := ifelse( Family_fix == "NA", NA, Family_fix ) ]
View(dt_fix_Sp)


# Finally, replace all the common names in the dataset with the updated scientific names.

# Make a copied dt to hold to input fixed names
dt_name_fix <- dt

# Set "Key" variables to join by
setkey( dt_fix_Sp, Sp_Common )
setkey( dt_name_fix, Species )

# Update Bird Count data with Scientific Names
## Join Scientific Names to data
dt_name_fix <- dt_name_fix[ dt_fix_Sp ]
## Tidy up
dt_name_fix[ , ':=' ( Species = Sp_Scientific, Sp_Scientific = NULL,
                      Family = Family_fix, Family_fix = NULL ) ][]




# Run through regular BioTIME checks on updated Species Names

# Have a look at the Species binomial
dt_name_fix[ , .( Sp = sort(unique( Species )) ) ] %>% View()


# Specify Genus and Species
dt_name_fix[ , ':=' ( Genus = ifelse( Species %like% "NA", NA, word( Species, 1 ) ),
                      Species = word( Species, 2 ) ) ][]

# 'Av a look}
sort(unique( dt_name_fix$Family ))
sort(unique( dt_name_fix$Genus ))
sort(unique( dt_name_fix$Species ))

# A different view
sort(unique( paste( dt_name_fix$Genus, dt_name_fix$Species, sep = " " ) ))
## Lovely

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

# Aggregate Counts for species within sampling instances
dt_Final <-
  dt_name_fix[ , .( Abundance = sum(Abundance), Biomass = NA,
                    Family = Family[1], Genus = Genus[1], Species = Species[1],
                    SampleDesc = SampleDesc[1], Plot = Plot[1], Latitude = Latitude[1], Longitude = Longitude[1],
                    DepthElevation = NA, Day = Day[1], Month = Month[1], Year = Year[1], StudyID = "" )
               , by = c("Genus", "Species", "SampleDesc" )][ , 4:17 ]
dt_Final


# Export curated data
fwrite( dt_Final, file = "./Warren_2022/Warren_2022_TC_complete/Warren_2022_RawData_TC.csv" )



