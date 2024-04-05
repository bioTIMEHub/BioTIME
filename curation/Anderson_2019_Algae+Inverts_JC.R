# Curation Script ---------------------------------------------------------

# Dataset: Anderson_2019 (Long term monitoring of the rocky intertidal on Wizard Island in Barkley Sound)
# Location: Barkley Sound
# Contact: Luke Andersson research.coord@bamfield.msc.com 
# Curator: James Cant
# Date: 03-Jul-2023

# ------------------------------------------------------------------

# load the necessary packages
library(data.table)
library(reshape2)
library(stringr)
library(qdapRegex)
require(tidyverse)
require(maps)
require(dplyr)
require(sf)
require(clipr)

###########################
# STEP 1: Download and open dataset
###########################

# clear workspace
rm(list=ls())

# assign book-keeping details.
data_inverts_name <- 'Anderson_2019_Inverts'
data_algae_name <- 'Anderson_2019_Algae' # This dataset consists of two separate assemblages assessed independently.
curator <- 'JC'

# READ IN DATA ---------------------------------------------------
# identify the folder that the data is located in
mypath <- 'E:/BioTime/Anderson 2019/'
# identify folder for saving
fileSave <- 'E:/BioTime/CuratedData/'
# define file name
# There are four data-sets associated with this study. However, only two of those data-sets meet BIOTIME criteria in that they follow a complete assemblage (Invertebrate and algae) over multiple years. 
# The remaining two data-sets either don't follow a complete assemblage or represent a single year survey which involved a unique methodology so cannot be combined with the other datasets. 
file1 <- 'Anderson2019_BMSC_Wizard_InvertebrateDensity_SmallQuadrat_2001-2009.tab'
file2 <- 'Anderson2019_BMSC_Wizard_AlgalCover_1997-2009.tab'
# Open datasets
dat_inverts <- fread(
  file = paste0(mypath, file1),
  header = TRUE, sep = "\t"
)

dat_algae <- fread(
  file = paste0(mypath, file2),
  header = TRUE, sep = "\t"
)

###########################
# STEP 2: Clean and reformat data variables
###########################

# Reshape data-frames into Long-format --------------------------------
dat_inverts <- melt(dat_inverts, id.vars = c("Date", "Site", "Exposure", "Transect #", "Tidal Height"),
            variable.name = 'Species', value.name = 'Abundance')

dat_algae <- melt(dat_algae, id.vars = c("Date", "Site", "Exposure", "Transect #", "Tidal Height"),
                    variable.name = 'Species', value.name = 'Biomass')

### Clean Abundance variables ------------------------------
dat_inverts <- dat_inverts[!(is.na(dat_inverts$Abundance)),] # remove NAs
dat_algae <- dat_algae[!(is.na(dat_algae$Biomass)),]
dat_inverts <- dat_inverts[dat_inverts$Abundance > 0,] # and zeros
dat_algae<- dat_algae[dat_algae$Biomass > 0,]
# check to confirm
min(dat_inverts$Abundance) > 0; min(dat_algae$Biomass) > 0 # no zeroes?
sum(dat_inverts$Abundance == "" | dat_inverts$Abundance == ' ') == 0; sum(dat_algae$Biomass == "" | dat_algae$Biomass == ' ') == 0 # no blanks?
dat_inverts$Abundance <- as.numeric(dat_inverts$Abundance); dat_algae$Biomass <- as.numeric(dat_algae$Biomass)

### Clean Site/plot details ---------------------------------
names(dat_inverts)[4] <- names(dat_algae)[4] <- 'Transect'
names(dat_inverts)[5] <- names(dat_algae)[5] <- 'TidalHeight'
dat_inverts$Site <- as.factor(dat_inverts$Site); dat_algae$Site <- as.factor(dat_algae$Site) 
dat_inverts$Exposure <- as.factor(dat_inverts$Exposure); dat_algae$Exposure <- as.factor(dat_algae$Exposure) 
dat_inverts$Transect <- as.factor(dat_inverts$Transect); dat_algae$Transect <- as.factor(dat_algae$Transect) 
dat_inverts$TidalHeight <- as.factor(dat_inverts$TidalHeight); dat_algae$TidalHeight <- as.factor(dat_algae$TidalHeight) 

### Clean Date info ------------------------------------
dat_inverts$Day <- as.factor(sub("-.*", "", dat_inverts$Date)); dat_algae$Day <- as.factor(sub("-.*", "", dat_algae$Date)) # Extract day
dat_inverts$Year <- as.factor(sub(".*-", "", dat_inverts$Date)); dat_algae$Year <- as.factor(sub(".*-", "", dat_algae$Date)) # Extract Year
levels(dat_inverts$Year); levels(dat_algae$Year)
levels(dat_inverts$Year) <- c('2001','2002','2003','2007','2009'); levels(dat_algae$Year) <- c('2001','2002','2003','2007','2009','1997') 
dat_inverts$Month <- as.factor(unlist(ex_between(dat_inverts$Date, '-', '-'))); dat_algae$Month <- as.factor(unlist(ex_between(dat_algae$Date, '-', '-'))) # Extract Month
levels(dat_inverts$Month); levels(dat_algae$Month)
levels(dat_inverts$Month) <- levels(dat_algae$Month) <- c('08','07')
# remove un-needed variable
dat_inverts$Date <- NULL; dat_algae$Date <- NULL

### Clean Taxonomy -------------------------------------
## INVERTEBRATES
# Ensure species names all in Latin
sort(unique(dat_inverts$Species))
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Amphipoda'] <- 'Amphipoda sp'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Chlorostoma (tegula) funebralis'] <- 'Tegula funebralis'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Colonial tunicate'] <- 'Ascidiacea sp'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Cottidae spp (sculpins)'] <- 'Cottidae sp'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Decorator crab spp'] <- 'Majoidea sp'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Juvenile limpet (<5mm)'] <- 'Patellogastropoda sp'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Lithopoma (Astraea) gibberosa'] <- 'Astraea gibberosa'
levels(dat_inverts$Species)[levels(dat_inverts$Species) == 'Pholadidae spp (piddock clam)'] <- 'Pholadidae sp'
# Separate out genera
dat_inverts$Genus <- word(dat_inverts$Species, 1)
sort(unique(dat_inverts$Genus))
# Insert Family variable and relocate any inappropriate genus entries
dat_inverts$Family <- dat_inverts$Genus
dat_inverts[str_which(dat_inverts$Family, 'idae$|eae$', negate = T),]$Family <- NA
dat_inverts[dat_inverts$Genus == 'Patellogastropoda',]$Family <- 'Patellogastropoda'
dat_inverts[!(is.na(dat_inverts$Family)),c('Genus')] <- NA
dat_inverts$Family <- as.factor(dat_inverts$Family)
dat_inverts$Genus <- as.factor(dat_inverts$Genus)
# Separate out species
dat_inverts$Species <- word(dat_inverts$Species, 2)
sort(unique(dat_inverts$Species))
dat_inverts$Species[dat_inverts$Species == 'spp'] <- 'sp'
dat_inverts$Species <- as.factor(dat_inverts$Species)
# Manually check to confirm
sort(unique(dat_inverts$Species))
sort(unique(dat_inverts$Genus))
sort(unique(dat_inverts$Family))

## ALGAE
# Ensure species names all in Latin
sort(unique(dat_algae$Species))
levels(dat_algae$Species)[levels(dat_algae$Species) == 'Encrusting coralline algae'] <- 'Corallinales sp'
levels(dat_algae$Species)[levels(dat_algae$Species) == 'Filamentous rhodophyta'] <- 'Rhodophyta sp'
# Remove Bare rock entries
dat_algae <- dat_algae[dat_algae$Species != 'Bare rock',]
# Separate out genera
dat_algae$Genus <- word(dat_algae$Species, 1)
sort(unique(dat_algae$Genus))
# Insert Family variable and relocate any inappropriate genus entries
dat_algae$Family <- dat_algae$Genus
dat_algae[str_which(dat_algae$Family, 'idae$|eae$', negate = T),]$Family <- NA
dat_algae[dat_algae$Genus == 'Corallinales',]$Family <- 'Corallinales'
dat_algae[dat_algae$Genus == 'Rhodophyta',]$Family <- 'Rhodophyta'
dat_algae[!(is.na(dat_algae$Family)),c('Genus')] <- NA
dat_algae$Family <- as.factor(dat_algae$Family)
dat_algae$Genus <- as.factor(dat_algae$Genus)
# Separate out species
dat_algae$Species <- word(dat_algae$Species, 2)
sort(unique(dat_algae$Species))
dat_algae$Species[dat_algae$Species == 'spp'] <- 'sp'
dat_algae$Species <- as.factor(dat_algae$Species)
# Manually check to confirm
sort(unique(dat_algae$Species))
sort(unique(dat_algae$Genus))
sort(unique(dat_algae$Family))

# ADD SPATIAL INFORMATION ---------------------------------------------------
# No elevation data provided
dat_inverts$DepthElevation <- dat_algae$DepthElevation <- NA
# The study provides a boundary box for the study location so it is nessecary to identify a central GPS location
dt_coords <- data.frame(Longitude = c(-125.160843, -125.158203, -125.160843, -125.158203),
                        Latitude = c(48.8592, 48.8592, 48.857391, 48.857391))
# Convert data points into point spatial object
dt_coord <- dt_coords %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
# add latitude and longitude to data
dat_inverts$Longitude <- rep(centroid[1], dim(dat_inverts)[1]); dat_algae$Longitude <- rep(centroid[1], dim(dat_algae)[1])
dat_inverts$Latitude <- rep(centroid[2], dim(dat_inverts)[1]); dat_algae$Latitude <- rep(centroid[2], dim(dat_algae)[1])

# Check the coordinates match with the expected location of the data
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dat_inverts %>% distinct(Longitude, Latitude), aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-140,-100), ylim=c(30,60))
points_zoom # all looks good
# clean memory
rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# PREPARE RAW DATA --------------------------------------------------------

# aggregate abundance records that are same species, site, transect, and survey day.
dt_inverts_merged <- dat_inverts %>% group_by(Latitude, Longitude, DepthElevation, Family, Genus, Species, Month, Year, Day, TidalHeight, Transect, Exposure, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Month, Day, Site, Exposure, Transect, TidalHeight, Family, Genus, Species)
dt_algae_merged <- dat_algae %>% group_by(Latitude, Longitude, DepthElevation, Family, Genus, Species, Month, Year, Day, TidalHeight, Transect, Exposure, Site) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Month, Day, Site, Exposure, Transect, TidalHeight, Family, Genus, Species)
nrow(dat_inverts) - nrow(dt_inverts_merged) # any change in aggregating?
nrow(dat_algae) - nrow(dt_algae_merged)
# now create empty columns needed to fit to template
dt_inverts_merged$Biomass <- rep('', nrow(dt_inverts_merged)); dt_algae_merged$Abundance <- rep('', nrow(dt_algae_merged))
dt_inverts_merged$Plot <- rep('', nrow(dt_inverts_merged)); dt_algae_merged$Plot <- rep('', nrow(dt_algae_merged))
dt_inverts_merged$StudyID <- rep('', nrow(dt_inverts_merged)); dt_algae_merged$StudyID <- rep('', nrow(dt_algae_merged))
dt_inverts_merged$SampleDescription <- as.factor(with(dt_inverts_merged, paste(Latitude, Longitude, Site, Exposure, Transect, TidalHeight, Year, Month, Day, sep='_')))
dt_algae_merged$SampleDescription <- as.factor(with(dt_algae_merged, paste(Latitude, Longitude, Site, Exposure, Transect, TidalHeight, Year, Month, Day, sep='_'))) 
length(levels(dt_inverts_merged$SampleDescription)); length(levels(dt_algae_merged$SampleDescription))

# Remove variables that don't fit BIOTIME format (after storing necessary details in sample description)
dt_inverts_merged$Site <- NULL; dt_inverts_merged$Exposure <- NULL; dt_inverts_merged$Transect <- NULL; dt_inverts_merged$TidalHeight <- NULL
dt_algae_merged$Site <- NULL; dt_algae_merged$Exposure <- NULL; dt_algae_merged$Transect <- NULL; dt_algae_merged$TidalHeight <- NULL

# reorder columns to match BioTIME format
## INVERTEBRATES
dt_inverts_merged <- dt_inverts_merged[c('Abundance',
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
                                         'StudyID')] %>% arrange(Year, Month, Day, Family, Genus, Species)
## ALGAE
dt_algae_merged <- dt_algae_merged[c('Abundance',
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
                                     'StudyID')] %>% arrange(Year, Month, Day, Family, Genus, Species)

# final check :)
summary(dt_inverts_merged); summary(dt_algae_merged)
str(dt_inverts_merged); str(dt_algae_merged)

# Export final ------------------------------------------------------------

write.csv(dt_inverts_merged, paste0(fileSave, data_inverts_name, '_rawdata_', curator, '.csv'), row.names=F)
write.csv(dt_algae_merged, paste0(fileSave, data_algae_name, '_rawdata_', curator, '.csv'), row.names=F)
write_clip(dt_inverts_merged)
write_clip(dt_algae_merged)

# -------------------------------------------- End of Code ------------