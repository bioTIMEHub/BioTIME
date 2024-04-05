

# Curation Script ---------------------------------------------------------

# Dataset: LTER Solling Spruce, Germany Vegetation
# Location: Solling, Germany
# Curator: Cher Chow
# Date: 20-Jan-2021

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
# clear old items
rm(list=ls(all=TRUE))
dt <- read_excel('./Originals/HaasePilotto/S036.xlsx', sheet=2, skip=3, col_names=T, na='')
dt$Site <- NULL

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
names(dt)[3:6] <- paste0('Plot', 1:4) # rename the plots
dt <- dt %>% pivot_longer(cols=starts_with('Plot'), names_to = "Plot", values_to="Biomass") # pivot to long format
dt <- dt %>% filter(!is.na(Biomass))
str(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? NA here
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
dt$Plot <- as.factor(dt$Plot)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES or BIOMASS
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(dt$Biomass == "") # no blanks
summary(dt)

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

dt$Year <- year(dt$Date)
dt$Month <- month(dt$Date)
dt$Day <- day(dt$Date)
dt$Date <- NULL
summary(dt[,5:7])

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.

dt$Latitude <- 51.7647222222222
dt$Longitude <- 9.57972222222222

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(0,20), ylim=c(40,60))


# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon)+0)
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$Taxon # make a copy in case
sort(unique(dt$Taxon))
replace_sp <- c(
  'filix femina' = 'filix-femina',
  'spec.' = 'sp'
)
dt$Species <- dt$Species %>% str_replace_all(., replace_sp)
sort(unique(dt$Species))
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, 2, end=-1) # everything but the first word
sort(unique(dt$Genus))
dt$Taxon <- NULL

# Prepare raw data --------------------------------------------------------
# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup()
dim(dt)[1]-dim(dt_merged)[1] # check if there's any difference

# add in blank columns
dt_merged$DepthElevation <- 508
dt_merged$StudyID <- ''
dt_merged$Abundance <- ''
dt_merged$Family <- ''

dt_merged %>% distinct(Year, Month, Plot, Layer) %>% 
  arrange(Year, Month, Plot, Layer) %>% View
dt_merged %>% group_by(Plot) %>% summarise(nyears = n_distinct(Year))
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Plot, Layer, sep='_')))
length(levels(dt_merged$SampleDescription)) # check number of sampling events
View(dt_merged) # final check :)

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
                         'StudyID')] # will remove Layer too

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_ICP_SollingGermany_Spruce_Vegetation_rawdata_CC.csv', row.names=F)
clipr::write_clip(dt_merged)

