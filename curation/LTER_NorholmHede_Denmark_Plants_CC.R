

# Curation Script ---------------------------------------------------------

# Dataset: LTER Norholm Hede Denmark Plants
# Location: Denmark
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(lubridate)
require(ggplot2)
require(maps)
require(readxl)

rm(list=ls())
data.name <- 'LTER_NorholmHede_Denmark_Plants'
curator <- 'CC'

# make sure your working directory is set before running these lines
dt <- read_excel('Originals/HaasePilotto/S045.xlsx', sheet=1, skip=3, col_names=T, na='')
View(dt)
n_distinct(dt$Site) # check to make sure it is just one site.
dt$Site <- NULL # omit

# add additional data provided about the site
dt$Latitude <- 55.680062
dt$Longitude <- 8.604672
dt$DepthElevation <- 15
View(dt) # looks ok

# rename things
names(dt)[c(1,2,4)] <- c('Year', 'Plot', 'Biomass')

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 649 records
str(dt) # check structure

summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# Year, month and day must be integers or factors?
dt$Year <- as.integer(dt$Year)
summary(dt$Year) # reasonable range

# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
dt$Plot <- as.factor(dt$Plot)
n_distinct(dt$Plot)
unique(dt$Plot)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Biomass) > 0 # no zeroes? Y
sum(dt$Biomass == "") == 0 # no blanks? Y
str_detect(dt$Biomass, '\\s') %>% sum() == 0 # no blank spaces? Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt$Year) # looks good to me

# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(min(dt$Longitude)-5, max(dt$Longitude)+5), ylim=c(min(dt$Latitude)-5, max(dt$Latitude)+5))
points_zoom # all looks good

rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon) + 0) == 0
str_detect(dt$Taxon, '^\\s$') %>% sum() == 0 # no blank spaces? Y

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus <- word(dt$Taxon, 1)
dt$Genus %>% str_detect(., 'idae$|ini$') %>% sum(. + 0)
dt$Species <- dt$Taxon # make a copy
# Check the species records
sort(unique(dt$Species))
# replace uncertain species with slashes
dt$Species <- dt$Species %>% str_replace_all(., 'sp.\\s*$', 'sp')
# regex to detect any repeating species epithets separated by slashes
# Genus species/species.../species
# manual inspection scan for potential typos/dupes
sort(unique(dt$Species %>% tolower())) # check without case sensitivity for genera
dt$Species <- word(dt$Species, start=2, end=-1) # keep only specific epithet
sort(unique(dt$Genus))
dt$Genus <- str_to_title(dt$Genus) # fix the lowercase issue
sort(unique(dt$Genus)) # now check for misspellings
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate biomass records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Plot, Genus, Species)
nrow(dt) - nrow(dt_merged) # any change in aggregating? 8 records aggregated
# now create empty columns needed to fit to template
dt_merged$Abundance <- ''
dt_merged$Month <- ''
dt_merged$Day <- ''
dt_merged$Family <- ''
dt_merged$StudyID <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Plot, sep='_')))
length(levels(dt_merged$SampleDescription)) # 50 samples

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
                         'StudyID')] %>% arrange(Year, Plot, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(data.name, '_rawdata_', curator, '.csv'), row.names=F)

