
# Curation Script ---------------------------------------------------------

# Dataset: alston_2021_tree_census_2009_2019
# Location: UHURU
# Curator: Alban Sagouis
# Date: 27-06-2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

# make sure your working directory is set before running these lines
dataset.name <- 'alston_2021_tree_census_2009_2019'
dt <- utils::read.csv('data/alston_2021/alston_2021_tree_census_2009_2019/alston_2021_TREE_CENSUS_DETAILED_2009-2019.csv', header = TRUE, sep = ",")

# Structure check ---------------------------------------------------------


dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
length(unique(dt$census)) == 1
length(unique(dt$site)) == 1
length(unique(dt$block)) == 1
length(unique(dt$treatment)) == 1
length(unique(dt$plot)) == 1
length(unique(dt$section)) == 1

dt <- dt %>% dplyr::select(-census, -treatment, -X.0.5m, -X0.5_1m, -X.1m, -X1_2m, -X2_3m, -X3_4m, -X.4m) %>%
  dplyr::filter(total != 0)


# Primary field check -----------------------------------------------------
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
coords <- utils::read.csv('data/alston_2021/alston_2021_tree_census_2009_2019/alston_2021_PLOT_COORDINATES.csv', header = TRUE, sep = ",")[, c('site','block','dd_long','dd_lat')]
coords <- coords %>% dplyr::mutate(
  site = stringi::stri_replace_all_fixed(str = site, pattern = c('SOUTH','CENTRAL','NORTH'),
                                         replacement = c('S','C','N'), vectorise_all = FALSE)) %>%
  dplyr::group_by(site, block) %>% 
  dplyr::summarise(Latitude = mean(dd_lat), Longitude = mean(dd_long)) %>% dplyr::ungroup()

dt <- dt %>% dplyr::left_join(coords, by = c('site','block')) %>%
  dplyr::select(-site, -block)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
dt <- dt %>% dplyr::rename(Abundance = total, Year = year, Plot = plot, Species = species)
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance == "") # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31


# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names
dt <- dt %>% dplyr::mutate(
  Genus = stringi::stri_extract_first_regex(pattern = '^[A-Za-z]*(?=_)', str = Species),
  Species = stringi::stri_extract_first_regex(pattern = '(?<=_)[a-z]*$', str = Species)
)

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that

# check family too

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$Family <- NA
dt$Biomass <- NA
dt$DepthElevation <- NA
dt$StudyID <- NA
dt$Month <- NA
dt$Day <- NA

dt$SampleDescription <- as.factor(with(dt, paste(Year, Plot, section, sep = '_')))
length(levels(dt$SampleDescription))

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
dt_merged <- dt %>% dplyr::group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = mean(Abundance)) %>% dplyr::ungroup() %>% 
  dplyr::arrange(Year, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? yep. 179 rows lost

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
utils::write.csv(dt_merged, paste0('data/alston_2021/alston_2021_tree_census_2009_2019/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt_merged)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt_merged$Longitude, dt_merged$Latitude)[grDevices::chull(dt_merged$Longitude, dt_merged$Latitude), ]) / 10^6
centroid <- apply(unique(dt_merged[, c('Longitude','Latitude')]), 2, mean)
