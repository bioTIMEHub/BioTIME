

# Curation Script ---------------------------------------------------------

# Dataset: LTER Muggelsee Zooplankton
# Location: Germany
# Curator: Cher Chow
# Date: 14-Jan-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
dt <- read_excel('Originals/ILTER_Muggelsee_plankton.xlsx', sheet=2, skip=2, col_names=T, na='')


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? NA here
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt$Site <- NULL # remove site column since it's all the same
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
names(dt)[3] <- 'Abundance'
min(dt$Abundance) # check the minimum (no zeroes)
# problem: there are NAs
dt <- dt %>% filter(!is.na(Abundance))
min(dt$Abundance) # rerun check
sum(dt$Abundance=="") # no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

dt$Year <- year(dt$`Sampling date`)
dt$Month <- month(dt$`Sampling date`)
# no day
dt$`Sampling date` <- NULL
summary(dt[,3:4])

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

dt$Latitude <- 52.44
dt$Longitude <- 13.65

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points

points_zoom <- points + coord_fixed(xlim=c(0,20), ylim=c(40,60))
points_zoom

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon)+0)
# misspellings check, but not taxonomic cleaning
sort(unique(dt$Taxon))
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$Taxon
dt$Family <- ''
dt$Family[str_which(dt$Taxon, 'idae$|oda$')] <- dt$Taxon[str_which(dt$Taxon, 'idae$|oda$')] #  move any family names
dt$Species[str_which(dt$Taxon, 'idae$|oda$')] <- ''
sort(unique(dt$Family))
# first, records with undefined species epithets into the BioTIME format of sp without period
dt$Species <- dt$Species %>% str_replace_all(., 'spp.$', 'sp')
sort(unique(dt$Species))
replace_sp <- c(# pattern = replacement
  'girodi/brightwelli' = 'sp1',
  'Bosmina Eubosmina' = 'Bosmina (Eubosmina)',
  'Bosmina juvenil' = 'Bosmina sp',
  'Cyclops s.l.' = 'Cyclops sp',
  'Cyclops strenuus / abyssorum' = 'Cyclops sp1',
  '\\scopepodites|\\snauplii' = '',
  'Daphnia juvenile' = 'Daphnia sp',
  'Diaphanosoma brachyurum juvenile' = 'Diaphanosoma brachyurum',
  'Dreissena polymorpha larvae' = 'Dreissena polymorpha',
  'Eubosmina coregoni coregoni' = 'Eubosmina coregoni',
  'Eubosmina coregoni gibbera' = 'Eubosmina coregoni',
  'Eubosmina coregoni thersites' = 'Eubosmina coregoni',
  'Eubosmina longicornis berolinensis' = 'Eubosmina longicornis',
  "Keratella cochlearis cochlearis" = 'Keratella cochlearis',    
  "Keratella cochlearis hispida"  = 'Keratella cochlearis',          
  "Keratella cochlearis robusta" = 'Keratella cochlearis',           
  "Keratella cochlearis tecta" = 'Keratella cochlearis',
  'Leptodora juvenile' = 'Leptodora sp',
  'Leptodora meta nauplii' = 'Leptodora meta',
  'Polyarthra small' = 'Polyarthra sp',
  'Polyarthra dolichoptera-vulgaris-Group' = 'Polyarthra sp1',
  'Polyarthra major-euryptera Group' = 'Polyarthra sp2',
  'Polyarthra-vulgaris-dolichoptera-Gruppe' = 'Polyarthra sp3',
  ' Group$' = ' agg.',
  ' Gruppe$' = ' agg.',
  '-Group$' = ' agg.',
  '-Gruppe$' = ' agg.',
  '\\, frei' = '',
  '\\, sessil' = ''
)
# remove anything not identified to Family level
dt$Species <- dt$Species %>% str_replace_all(., replace_sp) # fix uncertain species conventions
sort(unique(dt$Species))

dt$Family[str_detect(dt$Species, 'ciliates')] <- 'Ciliophora'
dt$Family[str_detect(dt$Species, 'rotifers|Rotifers')] <- 'Rotifera'
dt$Family[str_detect(dt$Species, 'Peritricha|peritricha')] <- 'Peritrichia'
dt$Family[str_detect(dt$Species, 'Copepod')] <- 'Copepoda'
dt$Family[str_detect(dt$Species, 'Turbellaria')] <- 'Turbellaria'
dt$Family[str_detect(dt$Species, 'Harpacticoida')] <- 'Harpacticoida'
dt$Family[str_detect(dt$Species, 'Cyclopoid')] <- 'Cyclopoid'
dt$Family[str_detect(dt$Species, 'Calanoid')] <- 'Calanoid'
dt %>% filter(Family != '') %>% View
dt %>% filter(is.na(Species)) %>% View
dt$Species[dt$Family != ''] <- ''

sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2, end=-1) # everything but the first word
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Family, Genus, Species)
nrow(dt) - nrow(dt_merged) # check if there's any difference

# add in blank columns
dt_merged$Biomass <- ''
dt_merged$SampleDescription <- ''
dt_merged$Plot <- ''
dt_merged$DepthElevation <- 34
dt_merged$Day <- ''
dt_merged$StudyID <- ''

dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, sep='_')))
length(levels(dt_merged$SampleDescription)) # check number of sampling events

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
                         'StudyID')]

View(dt_merged) # final check :)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_Muggelsee_Zooplankton_rawdata_CC.csv', row.names=F)
