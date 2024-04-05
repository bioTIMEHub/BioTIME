## ellison 2017a
require(dplyr)
require(stringr)
require(lubridate)

dataset_id <- 'HarvardDuke_ForestAnts_EllisonDunn'
load(file = 'sDiv/BioTIMEx/data/raw data/ellison_2017a/ddata')

# Structure check ---------------------------------------------------------

dim(ddata) # check dimensions, 10319 records originally
str(ddata) # check structure

# 15 experimental plots per forest site = cham
# 12 plots with experimental heating chambers, 3 control without chambers
# 4 pitfall traps per plot = subs
n_distinct(ddata$cham) # check that it lines up with methods described

# treatments nested under control vs heat treatments, but also varying levels of heat = target.delta
sort(unique(ddata$method)) # we'll need to split dataset into winkler method sampling and pitfall sampling

# simple column reorganise
# only use records from controls as this is experimental
# 3 chamberless control and 3 chamber control plots
# pretreat covers all

# need to identify the plot IDs that are controls
controls <- ddata %>% distinct(site, cham, warming) %>% 
  filter(str_detect(warming, 'Control|control')) %>% 
  mutate(warming = word(warming, 1), Plot = paste(site, cham, sep = "_"))
# site_plot should be enough and also pools pitfall traps together
ddata <- ddata %>% mutate(Plot = paste(site, cham, sep = "_")) %>% 
  filter(Plot %in% controls$Plot, is.na(genus) == F) %>% 
# only the plots that we identified as control plots, including pretreatment stage
  select(method, n, genus, species, site, Plot, year, month, day) %>%
   rename(Abundance = n)

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(ddata$Abundance == "", na.rm=T) # no blanks
summary(ddata)

View(ddata %>% filter(Abundance == 0)) # check the 0s in biomass
View(ddata %>% filter(is.na(Abundance))) # 3329 records of NAs in abundance and taxon
# eliminate 0 or NA records
ddata <- ddata %>% filter(!Abundance == 0, !is.na(Abundance))
2299 - nrow(ddata) # 4 rows gone

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(ddata) # dates all good

# LAT LONG
# have to add it in manually from paper, but they make sense

source('https://gist.githubusercontent.com/cherfychow/5473d5ba1d4ceb2fab68722373ad5ed1/raw/d77a8acf4688f751567fa3e4db49df870b3a27bf/dms%2520to%2520decimal%2520degree')

# make a key data frame
sites <- data.frame(site = c('HF', 'DF'),
                    Latitude = c('42 31 48', '35 52 0'),
                    Longitude = c('72 11 24', '79 59 45'))
sites$Latitude <- angle2dec(sites$Latitude)
sites$Longitude <- angle2dec(sites$Longitude) * -1 # remember west is negative

ddata <- left_join(ddata, sites, by="site") # match up

# visual check
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) +
   geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
   coord_fixed() +
   labs(x='Longitude', y='Latitude') +
   theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=sites, aes(x=Longitude, y=Latitude), shape=21)
points
points + coord_fixed(xlim=c(-90, -60), ylim=c(20,60))


# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(ddata$genus)+0)
sum(is.na(ddata$species)+0)
ddata %>% filter(is.na(species)) %>% View # check them out
ddata$species[is.na(ddata$species)] <- 'sp'
ddata$Family <- 'Formicidae'

# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
sort(unique(as.character(ddata$genus))) # manual check genera
ddata %>% filter(genus == 'Unknown') %>% View

replace_gen <- c( # pattern = replacement
   '\\s$' = ''
)
ddata$genus <- str_replace_all(ddata$genus, replace_gen)

sort(unique(paste(ddata$genus, ddata$species, sep=' '))) # check species names
ddata$species <- paste(ddata$genus, ddata$species, sep=' ')
replace_sp <- c(# pattern = replacement
   'cf' = 'sp',
   '^\\s' = '',
   'picea\\?' = 'sp',
   '\\(group\\)' = 'agg.',
   'sp_DFmorph' = 'sp',
   'rudis\\/picea' = 'sp1',
   'Formica subscericea' = 'Formica subsericea',
   'Camponotus chromoides' = 'Camponotus chromaiodes',
   'Camponotus pennsylvanica' = 'Camponotus pennsylvanicus'
)
ddata$species <- ddata$species %>% str_replace_all(., replace_sp) # fix uncertain species conventions
sort(unique(ddata$species)) # check species names
ddata$species <- word(ddata$species, 2, -1)
colnames(ddata) <- str_to_title(colnames(ddata)) # rename to match BioTIME

# Prepare raw data --------------------------------------------------------

# add in blank columns
ddata$SampleDescription <- with(ddata, paste(Year, Month, Plot, sep = "_"))
ddata$Site <- NULL
ddata$DepthElevation <- ''
ddata$Biomass <- ''
ddata$StudyID <- ''

ddata <- ddata %>% filter(str_detect(Method, 'Pitfall')) %>% select(!Method)
# only keep pitfall records, since winkler did not have enough for a time series

dt_merged <- ddata %>% group_by_at(vars(-Abundance)) %>%
   summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, Plot, Genus, Species)
nrow(ddata) - nrow(dt_merged) # check if there's any difference, 788 rows "lost"]

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
                                 'StudyID')] %>% arrange(Year, Month, Plot, Genus, Species) # reorder back

n_distinct(dt_merged$SampleDescription) # check number of sampling events
View(dt_merged) # final check :)

# Export final raw data ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset_id, '-pitfall', '_rawdata.csv'), row.names=F)

# Calculate spatial metadata ----------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long



