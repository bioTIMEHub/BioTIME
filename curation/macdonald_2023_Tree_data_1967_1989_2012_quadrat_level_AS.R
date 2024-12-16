# Curation Script ---------------------------------------------------------

# Dataset: macdonald_2023_Tree_data_1967_1989_2012_quadrat_level
# Location: pine forests of Banff and Jasper National Parks
# Curator: Alban Sagouis
# Date: 05.07.2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

dataset.name <- 'macdonald_2023_Tree_data_1967_1989_2012_quadrat_level'
# make sure your working directory is set before running these lines
dt <- utils::read.csv(
  file = 'data/macdonald_2023/macdonald_2023_Tree_data_1967_1989_2012_quadrat_level.csv',
  stringsAsFactors = TRUE, na.strings = '0', header = TRUE) %>% 
  dplyr::select(-c('TOT1.18','TOT3.18','TOT6.18','TOT9.18','TOT12.18','TOT15.18','Can.Cov',
                   'dead_SEE','dead_TRA','dead_1.3','dead_3.6','dead_6.9','dead_9.12',
                   'dead_12.15','dead_15.18','dead_24.27','dead_TOTAL','dead.BA','Comments'))


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
# Spatial data ----
coords <- data.frame(matrix(ncol = 3, byrow = TRUE, data = c(
  'Athabasca',   -117.9696549,  52.75510395,
  'Hector',      -116.2567177,  51.50370852,
  'Spray River', -115.39626631, 50.90481182,
  'Sunwapta',    -117.70472626, 52.5624097,
  'Whirlpool',   -117.92613834, 52.71421711),
  dimnames = list(c(), c('Site', 'Longitude', 'Latitude'))
))
dt <- dt %>% dplyr::left_join(coords, by = 'Site') %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude))
all(dt$Latitude <= 90) && all(dt$Latitude >= -90)
all(dt$Longitude <= 180) && all(dt$Longitude >= -180)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
# Melting species
dt <- dt %>% tidyr::pivot_longer(
  cols = !c('Site', 'Year_sampled','Q.2010','Q.1989','Q.1967','Longitude','Latitude'),
  values_to = 'Abundance', values_drop_na = TRUE, 
  names_to = 'Species')

min(dt$Abundance)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
dt <- dt %>% dplyr::rename(Year = Year_sampled)
all(dt$Year < 2023)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names
dt <- dt %>% dplyr::mutate(Species = base::substr(Species, 1, 2)) %>%
  dplyr::group_by(Q.1967, Site, Latitude, Longitude, Year, Species) %>%
  dplyr::summarise(Abundance = sum(Abundance)) %>% dplyr::ungroup()
  
dt <- dt %>% dplyr::mutate(Species = c('Abies lasiocarpa','Picea glauca','Pinus contorta','Picea mariana','Populus tremuloides')[match(Species, c('AL','PG','PC','PM','PT'))]) %>% 
  tidyr::separate(Species, into = c('Genus','Species'), sep = ' ')

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
# check family too

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$DepthElevation <- NA
dt$Family <- NA
dt$Biomass <- NA
dt$StudyID <- NA
dt$Month <- NA
dt$Day <- NA

dt <- dt %>% dplyr::mutate(
  Plot = paste(Site, Q.1967, sep = '_'),
  SampleDescription = paste(Site, Q.1967, Year, sep = '_'))

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
utils::write.csv(dt, paste0('data/macdonald_2023/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt$Longitude, dt$Latitude)[grDevices::chull(dt$Longitude, dt$Latitude), ]) / 10^6
centroid <- apply(unique(dt[, c('Longitude','Latitude')]), 2, mean, na.rm = FALSE)

