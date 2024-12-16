# Curation Script ---------------------------------------------------------

# Dataset: seipel_2022_MIREN_survey_of_plant_species_in_mountains
# Location: Southwestern ponderosa pine
# Curator: Alban Sagouis
# Date: 04.07.2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

dataset.name <- 'seipel_2022_MIREN_survey_of_mountain_plant_species'
# make sure your working directory is set before running these lines
dt <- utils:::read.csv(
  file = "data/seipel_2022/seipel_2022_MIRENplant_records_data_2007-2019.lat.long_v2_2212.csv",
  sep = ",", header = TRUE, encoding = "UTF-8",  stringsAsFactors = TRUE) %>% 
  dplyr::select(-Region, -Road, -Transect, -Cover, -Status)


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
length(levels(dt$Plot_id)) == 1

# Primary field check -----------------------------------------------------
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
all(dt$Latitude <= 90) && all(dt$Latitude >= -90)
all(dt$Longitude <= 180) && all(dt$Longitude >= -180)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
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

dt <- dt %>% dplyr::rename(Species = Accepted.Name.MIREN) %>%
  # dplyr::filter(!grepl('Unknown', Species, fixed = TRUE)) %>%
  tidyr::separate(Species, into = c('Genus','Species'), sep = ' ')

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
# check family too

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt <- dt %>% dplyr::rename(DepthElevation = Elevation)
dt$Abundance <- 1L
dt$Family <- NA
dt$Biomass <- NA
dt$StudyID <- NA
dt$Month <- NA
dt$Day <- NA

dt <- dt %>% dplyr::mutate(SampleDescription = paste(Plot_id, Year, sep = '_'))

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
dt_merged <- dt %>% dplyr::group_by(Biomass, Family, Genus, Species, SampleDescription, Latitude, Longitude, DepthElevation, Plot, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = 1L) %>% dplyr::ungroup() %>% 
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
utils::write.csv(dt, paste0('data/seipel_2022/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt$Longitude, dt$Latitude)[grDevices::chull(dt$Longitude, dt$Latitude), ]) / 10^6
centroid <- apply(unique(dt[, c('Longitude','Latitude')]), 2, mean, na.rm = FALSE)

