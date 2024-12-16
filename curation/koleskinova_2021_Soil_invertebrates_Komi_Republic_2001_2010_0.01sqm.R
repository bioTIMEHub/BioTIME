# Curation Script ---------------------------------------------------------

# Dataset: koleskinova_2021_Soil_invertebrates_Komi_Republic_2001_2010_0.01
# Location: Komi Republic
# Curator: Alban Sagouis
# Date: 04-07-2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

dataset.name <- 'koleskinova_2021_Soil_invertebrates_Komi_Republic_2001_2010_0.01sqm'

# make sure your working directory is set before running these lines
dt <- read.delim(file = "./data/koleskinova_2021/koleskinova_2021b_dwca-lpk-v1.3/koleskinova_2021_occurrence.txt", encoding = "UTF-8", header = TRUE, sep = '\t')
event <- read.delim(file = "./data/koleskinova_2021/koleskinova_2021b_dwca-lpk-v1.3/koleskinova_2021_event.txt", encoding = "UTF-8", header = TRUE, sep = '\t')

# selecting occurrences with the event table and adding coordinates and sampleSizeValue to ddata ----
event <- event %>%
  dplyr::filter(sampleSizeValue == 0.01) %>% 
  dplyr::select(eventID, locationID, year, month, day, decimalLatitude, decimalLongitude)

dt <- dt %>% dplyr::filter(occurrenceStatus == 'present') %>%
  dplyr::select(eventID, individualCount, family, genus, specificEpithet) %>%
  dplyr::right_join(event, unmatched = 'drop') %>% dplyr::select(-eventID) %>%
  dplyr::mutate(
    Treatment = stringi::stri_extract_first_regex(str = locationID, pattern = 'impact|buffer|background'),
    Site = base::gsub(
      pattern = '(<= |^.*buffer-|^.*impact-|^.*background-)(.{3,6})(=?-[0-9]{4}-)([0-9]{1,2})$', 
      x = locationID, replacement = '\\2-\\4', perl = TRUE)) %>%
  dplyr::rename(Abundance = individualCount,
                Family = family,
                Genus = genus,
                Species = specificEpithet,
                SampleDescription = locationID,
                Year = year,
                Month = month,
                Day = day,
                Latitude = decimalLatitude,
                Longitude = decimalLongitude)

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

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
all(dt$Year < 2020)
all(dt$Month <= 12L)
all(dt$day <= 31)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names

dt <- dt %>% dplyr::mutate(Species = dplyr::if_else(Species == '', 'sp', Species))
sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
# check family too
sort(unique(dt$Family))

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$Biomass <- NA
dt$Plot <- NA
dt$DepthElevation <- NA
dt$StudyID <- NA

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
dt_merged <- dt %>% dplyr::group_by(Biomass, Family, Genus, Species, SampleDescription, Latitude, Longitude, DepthElevation, Plot, Day, Month, Year, StudyID) %>% 
  dplyr::ungroup() %>% dplyr::arrange(Year, Family, Genus, Species)
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
utils::write.csv(dt, paste0('data/koleskinova_2021/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt$Longitude, dt$Latitude)[grDevices::chull(dt$Longitude, dt$Latitude), ]) / 10^6
centroid <- apply(unique(dt[, c('Longitude','Latitude')]), 2, mean, na.rm = FALSE)
