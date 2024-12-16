
# Curation Script ---------------------------------------------------------

# Dataset: alston_2021_UNDERSTORY_LGQUAD_2008_2019
# Location: UHURU
# Curator: Alban Sagouis
# Date: 27-06-2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

# make sure your working directory is set before running these lines
dataset.name <- 'alston_2021_UNDERSTORY_LGQUAD_2008_2019'
column_names <- utils::read.csv('data/alston_2021/alston_2021_UNDERSTORY_LGQUAD_2008_2019/alston_2021_UNDERSTORY_LGQUAD_2008_2019.csv', 
                                header = FALSE, sep = ",", nrows = 1)
dt <- utils::read.csv('data/alston_2021/alston_2021_UNDERSTORY_LGQUAD_2008_2019/alston_2021_UNDERSTORY_LGQUAD_2008_2019.csv', 
                                header = FALSE, sep = ",", skip = 4, col.names = column_names, stringsAsFactors = TRUE)


# Melting species into a long table ---------------------------------------
dt <- dt %>% 
  dplyr::select(-`NA.`, -survey, -treatment, -Bare_ground) %>%
  tidyr::pivot_longer(cols = Abutilon:Zornia_pratensis, names_to = 'Species', values_to = 'Abundance',
                      values_drop_na = TRUE) %>%
  dplyr::filter(Abundance != 0L) %>% dplyr::mutate(Abundance = 1L)
# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
length(levels(dt$site)) == 1
length(levels(dt$plot)) == 1

# Primary field check -----------------------------------------------------
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
coords <- utils::read.csv('data/alston_2021/alston_2021_UNDERSTORY_LGQUAD_2008_2019/alston_2021_PLOT_COORDINATES.csv', header = TRUE, sep = ",")[, c('site','block','dd_long','dd_lat')] %>% 
  dplyr::mutate(site = c("N","C","S")[match(site, c("NORTH","CENTRAL","SOUTH"))]) %>%
  dplyr::group_by(site, block) %>% 
  dplyr::summarise(Latitude = mean(dd_lat), Longitude = mean(dd_long)) %>% dplyr::ungroup()

dt <- dt %>% dplyr::left_join(coords, by = c('site','block')) %>%
  dplyr::select(-site, -block)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance == "") # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
dt <- dt %>% tidyr::separate(col = date, into = c('Month','Year'), sep = '_') %>%
  dplyr::mutate(Month = c(2L, 2L, 3L, 4L, 9L, 10L, 11L)[match(Month, c('february', 'February', 'March', 'April', 'September', 'October', 'November'))])

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names
is_genus <- !grepl('[._]', dt$Species)
is_subspecies <- grepl('_._', dt$Species, fixed = TRUE)
dt <- dt %>% dplyr::mutate(
  Genus = dplyr::if_else(is_genus, Species,
                         dplyr::if_else(is_subspecies, NA_character_, stringi::stri_extract_first_regex(pattern = '^[A-Za-z]*(?=_)', str = Species))),
  Species = dplyr::if_else(is_genus, 'sp',
                            dplyr::if_else(is_subspecies, Species, stringi::stri_extract_first_regex(pattern = '(?<=_)[a-z\\.0-9FBS]*$', str = Species)))
) %>% dplyr::mutate(Species = dplyr::if_else(is.na(Species), 'sp', Species))


sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
sort(unique(dt$Species)) # check genera too just in case the eyes didn't pick up on that
anyNA(dt$Species)
# check family too

# exclude subsampled species ----------------------------------------------
# Acacia spp. and Boscia angustifolia) and other overstory species (e.g., Opuntia stricta and Euphorbia sp.)
dt <- dt %>% dplyr::filter( !(Genus %in% c('Acacia','Euphorbia') | (Genus == 'Opuntia' & Species == 'stricta') | (Genus == 'Boscia' & Species == 'angustifolia')))

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$Family <- NA
dt$Biomass <- NA
dt$DepthElevation <- NA
dt$StudyID <- NA
dt$Day <- NA

dt <- dt %>% dplyr::rename(Plot = plot)
dt$SampleDescription <- as.factor(with(dt, paste(Year, Month, Plot, rebar, sep = '_')))
length(levels(dt$SampleDescription))

# aggregate abundance records that are same species, plot, and survey day.
anyDuplicated(dt)
dt_merged <- dt %>% dplyr::group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = mean(Abundance)) %>% dplyr::ungroup() %>% 
  dplyr::arrange(Year, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? no

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
utils::write.csv(dt, paste0('data/alston_2021/alston_2021_UNDERSTORY_LGQUAD_2008_2019/', dataset.name, '_rawdata_AS.csv'), row.names = FALSE)
clipr::write_clip(dt)


# Convex Hull for centroid ------------------------------------------------

convhull <- geosphere::areaPolygon(data.frame(dt$Longitude, dt$Latitude)[grDevices::chull(dt$Longitude, dt$Latitude), ]) / 10^6
centroid <- apply(unique(dt[, c('Longitude','Latitude')]), 2, mean)
