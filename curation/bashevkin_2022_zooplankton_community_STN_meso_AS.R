# Curation Script ---------------------------------------------------------

# Dataset: bashevkin_2022_zooplankton_community_STM_meso
# Location: Upper San Francisco Estuary
# Curator: Alban Sagouis
# Date: 04.07.2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

dataset.name <- 'bashevkin_2022_zooplankton_community_STM_meso'
# make sure your working directory is set before running these lines
dt <- utils::read.csv(file = 'data/bashevkin_2022_zooplankton_community/bashevkin_2022_zooplankton_community.csv',
                      sep = ",", header = TRUE, stringsAsFactors = TRUE)

dt <- dt %>% dplyr::select('Source','Station','Latitude','Longitude','Date','SampleID',
                           'Family','Taxname','CPUE','Undersampled', SizeClass) %>%
  dplyr::filter(Source == 'STM' & SizeClass == 'Meso' & !Undersampled & CPUE > 0)

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
length(levels(dt$Station)) == 1

# Primary field check -----------------------------------------------------
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

all(dt$Latitude <= 90) && all(dt$Latitude >= -90)
all(dt$Longitude <= 180) && all(dt$Longitude >= -180)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
# Pooling size classes together
dt <- dt %>% dplyr::rename(Abundance = CPUE) %>% 
  dplyr::group_by(Station, Latitude, Longitude, Date, SampleID, Family, Taxname) %>%
  dplyr::mutate(Abundance = sum(Abundance)) %>% dplyr::ungroup()
min(dt$Abundance)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
dt <-  dt %>% dplyr::mutate(Date = as.Date(Date, '%Y-%m-%d')) %>%
  dplyr::mutate(
    Year = as.integer(format(Date, '%Y')),
    Month = as.integer(format(Date, '%m')),
    Day = as.integer(format(Date, '%d'))
  )

all(dt$Year < 2023)
all(dt$Month <= 13)
all(dt$Day <= 31)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names

dt <- dt %>% 
  dplyr::rename(Species = Taxname) %>%
  dplyr::mutate(Species = gsub('_UnID', ' sp', Species)) %>%
  tidyr::separate(Species, into = c('Genus','Species'), sep = ' ')

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
anyNA(dt$Genus)
# check family too

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$DepthElevation <- NA
dt$Plot <- NA
dt$Biomass <- NA
dt$StudyID <- NA

dt <- dt %>% dplyr::rename(SampleDescription = SampleID)

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
dt_merged <- dt %>% dplyr::group_by(Biomass, Family, Genus, Species, SampleDescription, Latitude, Longitude, DepthElevation, Plot, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = sum(Abundance)) %>% dplyr::ungroup() %>% 
  dplyr::arrange(Year, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? yes

# reorder columns by BioTIME format
dt_merged <- dt_merged[c('Abundance',
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
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------
utils::write.csv(dt_merged, paste0('data/bashevkin_2022_zooplankton_community/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt_merged)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(na.omit(dt_merged$Longitude), na.omit(dt_merged$Latitude))[grDevices::chull(na.omit(dt_merged$Longitude), na.omit(dt_merged$Latitude)), ]) / 10^6
centroid <- apply(unique(dt_merged[, c('Longitude','Latitude')]), 2, mean, na.rm = TRUE)

