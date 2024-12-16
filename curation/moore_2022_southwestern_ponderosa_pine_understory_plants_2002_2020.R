# Curation Script ---------------------------------------------------------

# Dataset: moore_2022_southwestern_ponderosa_pine_understory_plants_2002_2020
# Location: Southwestern ponderosa pine
# Curator: Alban Sagouis
# Date: 04.07.2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

dataset.name <- 'moore_2022_southwestern_ponderosa_pine_understory_plants_2002_2020'
# make sure your working directory is set before running these lines
dt <- utils:::read.csv(
  file = "data/moore_2022/Data/Ancillary_Data_CSVs/moore_2022_Density_Species_Tabular_Version.csv",
  header = TRUE, sep = ",",  stringsAsFactors = TRUE) %>% 
  dplyr::select(-Easting_NAD83_UTM_Zone_12, -Northing_NAD83_UTM_Zone_12, -X_Coord_In_Quadrat, -Y_Coord_In_Quadrat, -Sq_Cm)

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
length(levels(dt$Site)) == 1

# Primary field check -----------------------------------------------------
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
coords <- utils::read.csv(
  file = 'data/moore_2022/Data/Ancillary_Data_CSVs/moore_2022_Quadrat_Locations_and_Data.csv', 
  header = TRUE, sep = ",",  stringsAsFactors = TRUE) %>%
  dplyr::select(Quadrat, Latitude_NAD_1983, Longitude_NAD_1983, Elevation_m, Exclosure)

dt <- dt %>% dplyr::left_join(coords, by = 'Quadrat')

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt <- dt %>% dplyr::group_by(Site, Quadrat, Year, species, 
                             Latitude_NAD_1983, Longitude_NAD_1983,
                             Elevation_m, Exclosure) %>%
  dplyr::summarise(Abundance = dplyr::n()) %>%
  dplyr::ungroup()
min(dt$Abundance)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
all(dt$Year < 2023)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names

dt <- dt %>% dplyr::rename(Species = species) %>%
  dplyr::filter(!grepl('Unknown', Species, fixed = TRUE)) %>%
  tidyr::separate(Species, into = c('Genus','Species'), sep = ' ')

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
# check family too

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt <- dt %>% dplyr::rename(
  Latitude = Latitude_NAD_1983, Longitude = Longitude_NAD_1983,
  DepthElevation = Elevation_m)
dt$Family <- NA
dt$Biomass <- NA
dt$Plot <- dt$Quadrat
dt$StudyID <- NA
dt$Month <- NA
dt$Day <- NA

dt <- dt %>% dplyr::mutate(SampleDescription = paste(Quadrat, Exclosure, Year, sep = '_'))

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
dt_merged <- dt %>% dplyr::group_by(Biomass, Family, Genus, Species, SampleDescription, Latitude, Longitude, DepthElevation, Plot, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = sum(Abundance)) %>% dplyr::ungroup() %>% 
  dplyr::arrange(Year, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? yes

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
utils::write.csv(dt, paste0('data/moore_2022/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt$Longitude, dt$Latitude)[grDevices::chull(dt$Longitude, dt$Latitude), ]) / 10^6
centroid <- apply(unique(dt[, c('Longitude','Latitude')]), 2, mean, na.rm = FALSE)

