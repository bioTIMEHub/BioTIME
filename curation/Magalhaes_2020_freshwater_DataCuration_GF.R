# Curation Script --------------------------------------------------------------

# Dataset: Magalhaes_2020_Freshwater
# Location: Muriaé Ornamental Aquaculture Center, Brazil
# Curator: Garrett Fundakowski
# Date started: 21-07-2023
# Last updated: 21-07-2023


# Set up -----------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)
require(stringr)
require(lubridate)
require(pdftools)
require(pdftables)

# clear up the environment before starting
rm(list=ls())

# set the working directory via a pop-up window
setwd(file.choose() %>% dirname())

# convert the pdf to a csv
# csv <- convert_pdf('./Magalhaes2020Supplementary.pdf', 'Magalhaes2020Supplementary.csv', api_key = "API code")
# ^this line of code is not reproducible as it requires a user-specific API key to use the pdftables package

# read in the data from the csv
dt <- read_csv('./Magalhaes2020Supplementary.csv', skip = 25, col_names = TRUE, n_max = 49)
coords <- read_excel('./SiteInfo.xlsx', col_names = TRUE)


# Fixes before merging  -----------------------------------------------
# rename columns for ease
colnames(dt)[2] <- '2003 Boa Vista'
colnames(dt)[3] <- '2015 Boa Vista'
colnames(dt)[4] <- '2004 Pinheiros'
colnames(dt)[5] <- '2015 Pinheiros'
colnames(dt)[6] <- '2005 Santo Antonio'
colnames(dt)[7] <- '2015 Santo Antonio'
colnames(dt)[8] <- '2006 Chato'
colnames(dt)[9] <- '2015 Chato'
colnames(dt)[10] <- '2006 Gaviao'
colnames(dt)[11] <- '2015 Gaviao'

# go from wide format to long
dt <- dt %>% pivot_longer(cols = 2:11, names_to = "Event", values_to = "Abundance")

# split event into year and plot
dt$Year <- word(dt$Event, 1)
dt$Plot <- word(dt$Event, start=2, end=-1)

# Convert given coords to decimal ----------------------------------------------
angle2dec <- function(angle) {
  angle <- as.character(angle)
  
  if (sum(str_detect(angle, '\\s*[:upper:]$')+0) > 0) {
    angle <- str_remove(angle, '\\s*[:upper:]$')
    warning("You passed coordinates containing N, E, S, or W. Stringr may have accounted for it but double check your outputs")
  }
  
  # use punctuation marks and symbols that aren't decimal points to split degrees minutes seconds apart
  x <- do.call(rbind, str_split(angle, '([^\\.] & [:punct:]\\s*)|([^\\.] & [:symbol:]\\s*)|(?<=[:digit:])\\s+(?=[:digit:])'))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

# convert to decimal
coords$Latitude <- angle2dec(str_replace_all(coords$LatDMS, "\\'|\\°|\"|[:symbol:]|\\'|\\s*[:upper:]$", ' '))
coords$Longitude <- angle2dec(str_replace_all(coords$LongDMS, "\\'|\\°|\"|[:symbol:]|\\'|\\s*[:upper:]$", ' '))
coords$Latitude <- -coords$Latitude # transform to N
coords$Longitude <- -coords$Longitude # transform to E

# remove unnecessary columns
coords$LatDMS <- NULL
coords$LongDMS <- NULL

# merge into dt
dt <- left_join(dt, coords, by = c("Plot" = "Site"))


# Structure check --------------------------------------------------------------
dim(dt) # 490 x 7
str(dt) 
summary(dt)

# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$Abundance) # TRUE
# Date should be POSIXct? 
# Year, month and day must be integers or factors?
is.numeric(dt$Year) | is.factor(dt$Year) # TRUE
# Secondary fields such as trawl, plot, transect etc must be factors or integers? 
# NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format.
# NA
# Taxonomic fields must be characters or factors? 
is.factor(dt$SPECIES) | is.character(dt$SPECIES) # TRUE


# Structure fix ----------------------------------------------------------------
# get month as separate variable
dt$Year <- as.factor(dt$Year)

str(dt)


# Primary field check ----------------------------------------------------------
# ABUNDANCE
# No negative values, zeroes, or NAs in abundance/biomass fields.
# From a quick look, there are 0s
# Set 0 values to NAs
dt["Abundance"][dt["Abundance"] == 0] <- NA
# Remove NAs
dt <- dt[!is.na(dt$Abundance),]
#Check
min(dt$Abundance) > 0 # TRUE
sum(dt$Abundance == "") == 0 # TRUE

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31
summary(dt$Year) # looks good

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt$Latitude) # looks good
summary(dt$Longitude) # looks good

#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)
points_zoom <- points + coord_fixed(xlim=c(-65,-25), ylim=c(-40,0))
points_zoom # looks good
points1 <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points_zoom1 <- points1 + coord_fixed(xlim=c(-42.5,-42),ylim=c(-20.75,-21.25))
points_zoom1 # looks good


# Secondary field --------------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
dt$Plot <- as.factor(dt$Plot)
summary(dt$Plot)
length(levels(dt$Plot)) # 5


# Taxonomic field check --------------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# Misspellings check, but not taxonomic cleaning
sort(unique(dt$SPECIES)) # Note: some have cf.

# Separate taxon names - SPECIES is 'Genus (cf.) species'
dt$Genus <- word(dt$SPECIES, 1)
dt$Species <- word(dt$SPECIES, start=-1) # this will ignore the cf.'s

dt[dt$Species=="sp.", "Species"] <- "sp"

# check for mispellings
sort(unique(dt$Genus))
sort(unique(dt$Species))


# Prepare raw data --------------------------------------------------------
# Remove unnecessary columns
dt$Event <- NULL
dt$SPECIES <- NULL

dt <- dt %>% arrange(Year, Genus, Species)
# now create empty columns needed to fit to template
dt$StudyID <- rep('', dim(dt)[1])
dt$SampleDescription <- rep('', dim(dt)[1])
dt$DepthElevation <- rep('', dim(dt)[1])
dt$Biomass <- rep('', dim(dt)[1])
dt$Day <- rep('', dim(dt)[1])
dt$Month <- rep('', dim(dt)[1])
dt$Family <- rep('', dim(dt)[1])

# save the dataset name as an object
dataset.name <- 'Magalhaes_2020_freshwater'
# fill in sampling event with unique info
dt$SampleDescription <- as.factor(with(dt, paste(Year, Plot, sep='_')))
length(levels(dt$SampleDescription)) # 10 samples

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? no

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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)

# final check
View(dt_merged)
summary(dt_merged)
str(dt_merged)


# Export final ------------------------------------------------------------

setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(dataset.name, '_rawdata_GF.csv'), row.names=F)

### Spatial Geometry Calculations for BioTIME datasets --------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long
# -20.9574212430323, -42.3371851376576


# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area) # 70.6863424354888 sq km 

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-42.5,-42), ylim = c(-20.75,-21.25)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()
