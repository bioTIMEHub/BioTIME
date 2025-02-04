# Curation Script ---------------------------------------------------------

# Dataset: alber_2022_LTER_Georgia_Coastal_Ecosystems_Molluscs_0.25
# Location: Georgia Coastal Ecosystems LTER
# Curator: Alban Sagouis
# Date: 27-06-2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
# require(maps)
# require(readxl)

# make sure your working directory is set before running these lines
dt <- utils::read.csv('data/alber_2022_LTER_Georgia_Coastal_Ecosystems_Molluscs/alber_2022_abundances.csv', header = FALSE, skip = 5, sep = "," ,quote =  '"', 
               col.names = c(
                 "Date",
                 "Year",
                 "Site_Name",
                 "Site",
                 "Zone",
                 "Plot",
                 "Location",
                 "Flag_Location",
                 "Location_Notes",
                 "Longitude",
                 "Flag_Longitude",
                 "Latitude",
                 "Flag_Latitude",
                 "Species",
                 "Mollusc_Count",
                 "Quadrat_Area",
                 "Mollusc_Density",
                 "Notes"    ), check.names = TRUE)

# Structure check ---------------------------------------------------------


dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
dt <- dt %>% dplyr::filter(Quadrat_Area == 0.25 & Mollusc_Count != 0) %>%
  dplyr::select(Date, Year, Location, Longitude, Latitude, Species, Mollusc_Count, Quadrat_Area) %>%
  dplyr::rename(Abundance = Mollusc_Count, Site = Location)

# Year, month and day must be integers or factors? Y
dt$Month <- base::as.integer(base::substr(x = dt$Date, 6, 7))
dt$Day <- base::as.integer(base::substr(x = dt$Date, 9, 10))
dt$Date <- NULL

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance == "") # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Manually input centroid coordinates from metadata if needed

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(dt$Species))
dt$Genus <- stringr::word(dt$Species, 1)
# dt$Species <- word(dt$Species, start = 2, end = -1)
dt$Species <- 'sp'

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that

# check family too
sort(unique(dt$Family))

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$Family <- NA
dt$Biomass <- NA
dt$Plot <- NA
dt$DepthElevation <- NA
dt$StudyID <- NA

dt$SampleDescription <- as.factor(with(dt, paste(Site, Year, sep = '_')))
length(levels(dt$SampleDescription))

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
# No duplicates, no need to aggregate

dt_merged <- dt %>% dplyr::group_by(Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = sum(Abundance)) %>% dplyr::ungroup() %>% dplyr::arrange(Year, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # no change in aggregating

# reorder columns by BioTIME format
dt <- dt[c('Abundance',
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
           'StudyID')] %>% dplyr::arrange(Year, Family, Genus, Species)
summary(dt)
str(dt)

# Export final ------------------------------------------------------------
dataset.name <- 'alber_2022_LTER_Georgia_Coastal_Ecosystems_Molluscs_0.25'
utils::write.csv(dt, paste0('data/alber_2022_LTER_Georgia_Coastal_Ecosystems_Molluscs/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt$Longitude, dt$Latitude)[grDevices::chull(dt$Longitude, dt$Latitude), ]) / 10^6
centroid <- apply(unique(dt[, c('Longitude','Latitude')]), 2, mean)
