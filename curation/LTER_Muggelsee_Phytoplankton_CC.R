

# Curation Script ---------------------------------------------------------

# Dataset: LTER Muggelsee Phytoplankton
# Location: Berlin, Germany
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
dt <- read_excel('Originals/ILTER_Muggelsee_plankton.xlsx', sheet=3, skip=2, col_names=T, na='', col_types=c('skip','date','text','numeric','numeric'))


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? NA here
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt$Site <- NULL # remove site column since it's all the same
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
names(dt)[3] <- 'Abundance'
names(dt)[4] <- 'Biomass'
sum(dt$Abundance == "", na.rm=T) # no blanks
sum(dt$Biomass == "", na.rm=T) # no blanks
summary(dt)

View(dt %>% filter(Biomass == 0)) # check the 0s in biomass
# these are 0s because they have density instead of biomass as a record, so just fill with NA
dt$Biomass[which(dt$Biomass == 0)] <- NA

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

dt$Year <- year(dt$`Sampling date`)
dt$Month <- month(dt$`Sampling date`)
dt$Day <- day(dt$`Sampling date`)
# no day
dt$`Sampling date` <- NULL
summary(dt[,4:5])

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

dt$Latitude <- as.numeric(rep(52.44, dim(dt)[1]))
dt$Longitude <- as.numeric(rep(13.65, dim(dt)[1]))

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
dt$Species <- dt$Taxon # make a copy in case
dt$Family <- rep('', dim(dt)[1])
# first, records with undefined species epithets into the BioTIME format of sp without period
dt$Species <- dt$Species %>% str_replace_all(., 'spp.$', 'sp')
replace_sp <- c(# pattern = replacement
  'Aulacoseira granulata var. angustissima f. spiralis' = 'Aulacoseira granulata',
  ' cysts$' = '',
  'Chroococcus limneticus (col.)' = 'Chroococcus limneticus',
  ' var. variabile' = '',
  'Fragilaria ulna angustissima Sippen' = 'Ulnaria delicatissima',
  'Fragilaria ulna var. acus' = 'Ulnaria acus',
  'Rhodomonas minuta/lacustris' = 'Rhodomonas sp'
)
# remove anything not identified to Family level
dt <- dt %>% filter(!str_detect(Species, 'Chlorophyceae|Chrysoflagellata|Cyanobacteria cells|centric Diatoms|Chlorophycea cells'))
dt$Species <- dt$Species %>% str_replace_all(., replace_sp) # fix uncertain species conventions
sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2, end=-1) # everything but the first word
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-c(Abundance, Biomass))) %>% 
  summarise(Abundance=sum(Abundance), Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Month, Species)
dim(dt)[1]-dim(dt_merged)[1] # check if there's any difference

# add in blank columns
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
                         'StudyID')] %>% arrange(Year, Month, Genus, Species)

View(dt_merged) # final check :)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_Muggelsee_Phytoplankton_rawdata_CC.csv', row.names=F)



