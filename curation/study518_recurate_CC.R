
## STUDY_ID 518
## Drivers of bird species richness within moist high-altitude grasslands in eastern South Africa
## Recurate for fixes of BioTIME v1
## Curator: CC

require(dplyr)
require(stringr)
require(lubridate)

rm(list = ls())

dataset_id <- 'study518_recurate_CC'
dt <- readxl::read_excel('Originals/id518_doi_10.5061_dryad.m9p07__v1/avibaseline_PLOS.xlsx', sheet = 1)

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions, 1190 records originally
str(dt) # check structure

dt %>% distinct(Transect, Area) %>% View # are these distinct to transect number, yes
dt <- dt[-c(2,6,11:16)]
dt$Month <- month(dt$Date)
dt$Day <- day(dt$Date)
dt$Date <- NULL
colnames(dt) <- str_to_title(colnames(dt))
# the columns of 50m, 100m, and 100m+ don't seem to be cumulative, so I'll sum them for total counts over transect
dt$Abundance <- rowSums(dt[5:7], na.rm = T)
# check then remove the original columns
dt <- dt[-(5:7)]

str(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? Y
dt$Transect <- as.factor(dt$Transect)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct. NA. split already.
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
summary(dt)
sum(dt$Abundance == "", na.rm=T) # no blanks
dt %>% filter(is.na(Abundance))
dt %>% filter(Abundance == 0) %>% head %>% View
dt <- dt %>% filter(!Abundance == 0)
# eliminate 0 or NA records
1190 - nrow(dt) # 173 zeroes removed

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
# dates all good

# LAT LONG
# NA for this dataset

# Taxonomic field check ---------------------------------------------------

# need to coerce common names to scientific names
# check if there is a single scientific name that fits

sort(unique(dt$Species))
# match with the appendix supplied with the cited paper
species <- read.csv('Originals/id518_doi_10.5061_dryad.m9p07__v1/S1appendix_species.csv', header = T)
species$Species <- str_remove_all(species$Species, '\\,') # remove comma to match dt

n_distinct(species$Species)
n_distinct(dt$Species) # one to one key match?
# need visual checking, make a dataframe for manual checks
sp_check <- data.frame(dt = sort(unique(dt$Species)))
sp_check$key <- c(species$Species, rep(NA, 93-74))
# write_clip(sp_check)
sp_check <- read.csv('Originals/id518_doi_10.5061_dryad.m9p07__v1/sp_check_CC.csv', header = T)
# to fix common names in dt
dt_sp <- c('pale\\-crowned' = 'Pale-crowned',
           'white-necked' = 'White-necked',
           '\\sGrassland' = '',
           '\\sGrassveld' = '')
dt$Species <- str_replace_all(dt$Species, dt_sp)
sum(unique(dt$Species) %in% sp_check$dt + 0)
unique(dt$Species)[which(unique(dt$Species) %in% sp_check$dt == F)] # which one isn't represented
sp_check$dt <- str_replace_all(sp_check$dt, dt_sp)
sp_check <- sp_check[-65,]

# make a full key for matching
species <- full_join(species, sp_check, by=c('Species' = 'key'))
tempsp <- paste(word(species$dt[is.na(species$Scientific.name)], 2, -1), 
                                                    str_replace_all(word(species$dt[is.na(species$Scientific.name)], 1), '\\-', ' '), sep = ' ')
rm(sp_check)
# A MESS

require(taxize) # use taxize to fill in the missing scientific names
comm2sci <- comm2sci(tempsp, db = 'eol')
# tie breakers using eBird
comm2sci[[2]] <- comm2sci[[2]][2]
comm2sci[[7]] <- comm2sci[[7]][1]
comm2sci[[8]] <- comm2sci[[8]][2]
comm2sci[["Black Stork"]] <- comm2sci[["Black Stork"]][2]
comm2sci[["Barn Swallow"]] <- comm2sci[["Barn Swallow"]][2]
comm2sci[[18]] <- comm2sci[[18]][1]
comm2sci[[22]] <- comm2sci[[22]][2]
names(comm2sci) <- 1:23
comm2sci[c(3,9,11,13,15)] <- NA
comm2sci <- data.frame(Scientific.name = unlist(comm2sci), dt = species$dt[is.na(species$Scientific.name)])
comm2sci[3,1] <- 'Chrysococcyx caprius'
comm2sci[9,1] <- 'Plectropterus gambensis'
comm2sci[11,1] <- 'Bostrychia hagedash'
comm2sci[13,1] <- 'Macronyx capensis'
comm2sci[15,1] <- 'Columba arquatrix'
comm2sci$Species <- NA
species$Scientific.name[is.na(species$Scientific.name)] <- comm2sci$Scientific.name

dt <- left_join(dt, species[2:3], by=c('Species' = 'dt'))
dt %>% filter(is.na(Scientific.name)) %>% View # check if any got missed by the join

sort(unique(dt$Scientific.name)) # AND NOW WE CLEAN

sp_correct <- c('Eupodotus senegalensis' = 'Eupodotis senegalensis',
                'Mirafra Africana' = 'Mirafra africana')

dt$Scientific.name <- str_replace_all(dt$Scientific.name, sp_correct)
rm(comm2sci, dt_sp, sp_correct, tempsp, species)
dt <- dt %>% select(!Species)
dt$Genus <- word(dt$Scientific.name, 1)
dt$Species <- word(dt$Scientific.name, 2, -1)
dt$Scientific.name <- NULL
dt$Survey <- word(dt$Survey, 1)

# Prepare raw data --------------------------------------------------------

dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, Transect, Genus, Species)
nrow(dt) - nrow(dt_merged) # check if there's any difference, 6 rows "lost"]

# add in blank columns
dt_merged$SampleDescription <- with(dt_merged, paste(Year, Survey, Transect, sep = "_"))
dt_merged$DepthElevation <- ''
dt_merged$Biomass <- ''
dt_merged$StudyID <- ''
dt_merged$Plot <- ''
dt_merged$Family <- ''
dt_merged$Latitude <- ''
dt_merged$Longitude <- ''

n_distinct(dt_merged$SampleDescription) # check number of sampling events

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

# Export final raw data ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset_id, '_rawdata.csv'), row.names=F)
write_clip(dt_merged)



