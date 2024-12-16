# Curation Script ---------------------------------------------------------

# Dataset: alves_2022_Belizean_Barrier_Reef
# Location: Belizean Barrier Reef
# Curator: Alban Sagouis
# Date: 30-06-2023

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)

# make sure your working directory is set before running these lines
dataset.name <- 'alves_2022_Belizean_Barrier_Reef'
dt_og <- read.csv('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/v2/AS/alves_2022_Long.Master.Species.Groups.csv', header = T)
dt <- dt_og


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# Primary field check -----------------------------------------------------
# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

coords <- read.csv('/Users/cherchow/Dropbox/towards BioTIME v2/originalData/v2/AS/alves_2022_Belize_site_coord_protection.csv', header = T) %>%
  dplyr::select(-Site_Label, -Fishing_level) 

mean_Latitude <- mean(coords$Latitude, na.rm = TRUE)
mean_Longitude <- mean(coords$Longitude, na.rm = TRUE)

dt <- dt %>% dplyr::left_join(coords, by = 'Site') %>%
  mutate(Latitude = dplyr::if_else(is.na(Latitude), mean_Latitude, Latitude),
         Longitude = dplyr::if_else(is.na(Longitude), mean_Longitude, Longitude))

colnames(dt)
dt <- dt %>% group_by(Year, Site, Transect) %>% mutate(quadratN = length(unique(Image.Code))) %>% 
  filter(Cover > 0, Specific.Type == 'Hard.coral') %>% 
  select(Cover, ID, Site, Image.Code, Transect, Longitude, Latitude, Year, quadratN)

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

# divide per quadrat coverage to be relative to the transect
# photo quadrat number generally consistent?
summary(dt) # yes
dt <- dt %>% mutate(Biomass = Cover / (quadratN * 100)) %>% # divide by the total number of photo quadrats
  ungroup %>% group_by(Year, Site, Transect, Latitude, Longitude, ID) %>% 
  summarise(Biomass = sum(Biomass)) # pool to transect level

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# Y

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
# check the species list for misspellings or non-BioTIME taxonomic convention names

require(stringr)
dt$ID <- str_replace_all(dt$ID, '\\.', ' ')
sort(unique(dt$ID))
dt <- dt %>% filter(ID != 'Scleractinia')
replace <- c('species' = 'sp',
             '\\scomplex' = '')
dt$ID <- str_replace_all(dt$ID, replace)
dt$ID[str_detect(dt$ID, '\\s', negate = T)] <- paste(dt$ID[str_detect(dt$ID, '\\s', negate = T)], 'sp', sep = ' ')
sort(unique(dt$ID))

# validate species
require(worrms)
sp <- dt %>% distinct(ID) %>% filter(!str_detect(ID, '\\ssp$'))
sp_worms <- wm_records_names(sp$ID, marine_only = T)
dt %>% filter(str_detect(ID, 'complenata'))
dt$ID <- str_replace(dt$ID, 'complenata', 'complanata')

sp <- dt %>% distinct(ID) %>% filter(!str_detect(ID, '\\ssp$'))
sp_worms <- wm_records_names(sp$ID, marine_only = T) %>% bind_rows
sp_worms <- sp_worms %>% filter(!str_detect(valid_name, 'myriaster'))
# myriaster refers to a deepwater species and shallow water M. mirabilis is now auratenra

sp <- sp_worms %>% select(valid_name, family, genus) %>% bind_cols(sp, .)
dt <- sp %>% select(ID, valid_name) %>% left_join(dt, ., by = "ID")
dt <- ungroup(dt)
dt %>% distinct(ID, valid_name) %>% View
dt$valid_name[is.na(dt$valid_name)] <- dt$ID[is.na(dt$valid_name)]
dt$ID <- NULL

dt$Genus <- word(dt$valid_name, 1)
dt$Species <- word(dt$valid_name, 2, -1)
dt %>% distinct(Genus, Species, valid_name) %>% View

fixes <- data.frame(family = c('Mussidae', 'Agariciidae', 'Montastraeidae', 'Faviidae'),
                    genus = c('Pseudodiploria', 'Helioseris', 'Montastraea', 'Scolymia'))
dt <- sp %>% distinct(family, genus) %>% bind_rows(., fixes) %>% 
  left_join(dt, ., by = c('Genus' = 'genus'))
dt %>% distinct(family, Genus, Species, valid_name) %>% View

# Prepare raw data --------------------------------------------------------

# dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
# aggregate abundance records that are same species, plot, and survey day.
dt$SampleDescription <- with(dt, paste(Year, Site, Transect, sep = '_'))

dt_merged <- dt %>% rename(Family = family) %>% 
  arrange(Year, SampleDescription, Family, Genus, Species) %>% 
  mutate(Abundance = NA, DepthElevation = NA, Plot = NA, Day = NA, Month = NA) %>% 
  select(Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, 
           Latitude, Longitude, DepthElevation, Day, Month, Year)

summary(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, '/Users/cherchow/Dropbox/towards BioTIME v2/NewStudies/ready/AS/alves_2022_Belizean_Barrier_Reef_rawdata_AS_CC.csv')
clipr::write_clip(dt_merged)

# Convex Hull for centroid ------------------------------------------------

require(sf)
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
clipr::write_clip(centroid[c(2,1)]) # copy as lat-long
st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()
