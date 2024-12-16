# Curation Script ---------------------------------------------------------

# Dataset: rennie_2017_UK_ECN_carabids_1992_2015
# Location: Southwestern ponderosa pine
# Curator: Alban Sagouis
# Date: 04.07.2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

dataset.name <- 'rennie_2017_UK_ECN_carabids_1992_2015'
# make sure your working directory is set before running these lines
dt <-  utils::read.csv(
  file = "data/rennie_2017_carabids/supporting-documents/rennie_2017_carabids_ECN_IG1.csv",
  sep = ",", header = TRUE, stringsAsFactors = TRUE)

# taxonomy
## this file was downloaded by hand from the Supporting information archive at https://doi.org/10.5285/8385f864-dd41-410f-b248-028f923cb281
tax <- striprtf::read_rtf(file = 'data/rennie_2017_carabids/supporting-documents/rennie_2017_carabids_IG_dataStructure.rtf',
                          verbose = TRUE, ignore_tables = FALSE)[125:699]
tax <- data.table::fread(input = paste(tax, collapse = '\n'), sep = '|',
                         stringsAsFactors = TRUE, header = FALSE)

dt <- dt %>% dplyr::mutate(Species = tax$V3[match(FIELDNAME, tax$V2, nomatch = NULL)]) %>%
  dplyr::mutate(
    Species = dplyr::if_else(
      is.na(Species), 
      as.character(FIELDNAME), 
      as.character(Species)),
    FIELDNAME = NULL,
    TYPE = NULL)
dt <- dt %>% dplyr::filter(!grepl('Q[1-8]', Species))

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
length(levels(dt$SITECODE)) == 1
length(levels(dt$LCODE)) == 1
length(levels(dt$TRAP)) ==  1

# Primary field check -----------------------------------------------------
# LAT LONG
coords <- as.data.frame(matrix(
  dimnames = list(c(), c('SITECODE', 'SITENAME', 'Latitude', 'Longitude')),
  byrow = TRUE, ncol = 4L, data = c(
    'T01', 'Drayton', '52°11`37.95"N','1°45`51.95"W',
    'T02', 'Glensaugh', '56°54`33.36"N', '2°33`12.14"W',
    'T03', 'Hillsborough', '54°27`12.24"N', '6° 4`41.26"W',
    'T04', 'Moor House – Upper Teesdale', '54°41`42.15"N', '2°23`16.26"W',
    'T05', 'North Wyke', '50°46`54.96"N', '3°55`4.10"W',
    'T06', 'Rothamsted', '51°48`12.33"N', '0°22`21.66"W',
    'T07', 'Sourhope', '55°29`23.47"N', '2°12`43.32"W',
    'T08', 'Wytham', '51°46`52.86"N', '1°20`9.81"W',
    'T09', 'Alice Holt', '51° 9`16.46"N', '0°51`47.58"W',
    'T10', 'Porton Down', '51° 7`37.83"N', '1°38`23.46"W',
    'T11', 'Y Wyddfa – Snowdon', '53° 4`28.38"N', '4° 2`0.64"W',
    'T12', 'Cairngorms', '57° 6`58.84"N', '3°49`46.98"W')
))
dt <- dt %>% dplyr::left_join(coords) %>%
  dplyr::mutate(Latitude = parzer::parse_lat(Latitude),
                Longitude = parzer::parse_lon(Longitude))

# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
all(dt$Latitude <= 90) && all(dt$Latitude >= -90)
all(dt$Longitude <= 180) && all(dt$Longitude >= -180)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

dt <- dt %>% dplyr::rename(Abundance = VALUE) %>%
  dplyr::filter(!is.na(Abundance) & Abundance != 0 & !Species %in% c('XX', '', 'UU'))
min(dt$Abundance)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
dt <-  dt %>% dplyr::mutate(SDATE = as.Date(SDATE, '%d-%b-%y')) %>%
  dplyr::mutate(
  Year = as.integer(format(SDATE, '%Y')),
  Month = as.integer(format(SDATE, '%m')),
  Day = as.integer(format(SDATE, '%d'))
)

all(dt$Year < 2023)
all(dt$Month <= 13)
all(dt$Day <= 31)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names

dt <- dt %>% tidyr::separate(Species, into = c('Genus','Species'), sep = ' ') %>%
  dplyr::mutate(Species = dplyr::if_else(is.na(Species), 'sp', Species))

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
# check family too

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$DepthElevation <- NA
dt$Plot <- NA
dt$Family <- NA
dt$Biomass <- NA
dt$StudyID <- NA

dt <- dt %>% dplyr::mutate(SampleDescription = paste(SITECODE, LCODE, TRAP, Year, Month, Day, sep = '_'))

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
utils::write.csv(dt_merged, paste0('data/rennie_2017_carabids/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt_merged)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt_merged$Longitude, dt_merged$Latitude)[grDevices::chull(dt_merged$Longitude, dt_merged$Latitude), ]) / 10^6
centroid <- apply(unique(dt_merged[, c('Longitude','Latitude')]), 2, mean, na.rm = FALSE)

