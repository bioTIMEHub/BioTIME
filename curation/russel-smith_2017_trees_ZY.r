#########################################################
################## Adapted fronm ########################
################ russel-smith_2017_shrubs.r #############
#########################################################

rm(list = ls())
# setwd("../")
setwd("./russell-smith_2017_trees/")

###Data manually downloaded from:
### https://datacommons.anu.edu.au/DataCommons/rest/records/anudc:5836/data/
###  Spatial data manually downloaded from:
###  https://datacommons.anu.edu.au/russell-smith_2017/dataCommons/rest/records/anudc:5837/data/
###  Login for Australian National University needed. Data accessible after login without further requests.

#Raw Data ----
##Loading data ----
datafiles <- c(
   "./data/tpsk_trees_1994+_p831t1066.csv",
   "./data/tpsl_trees_1994+_p831t1124.csv",
   "./data/tpsn_trees_1994+_p831t1129.csv"
)

datafiles_dates <- c(
   "./data/tpsk_visit_date_1994+_p831t1067.csv",
   "./data/tpsl_visit_date_1994+_p831t1125.csv",
   "./data/tpsn_visit_date_1994+_p831t1153.csv"
)

datafiles_spatial <- c(
   "./spatial/tpsk_plot_details_spatial_coordinates_p894t1154.csv",
   "./spatial/tpsl_plot_details_spatial_coordinates_p894t1155.csv",
   "./spatial/tpsn_plot_details_spatial_coordinates_p894t1156.csv"
)



ddata <- data.table::rbindlist(
   fill = TRUE,
   lapply(
      datafiles,
      FUN = function(x)
         data.table::fread(file = x)
   ),
   use.names = TRUE, idcol = FALSE
)

dates <- data.table::rbindlist(
   lapply(
      datafiles_dates,
      FUN = function(x)
         data.table::fread(file = x)
   ),
   use.names = TRUE, idcol = FALSE
)

spatial <- data.table::rbindlist(
   lapply(
      datafiles_spatial,
      FUN = function(x)
         data.table::fread(file = x)
   ),
   use.names = TRUE, idcol = FALSE, fill = TRUE
)

#Raw Data ----
##merge data and dates ----
ddata <- dates[ddata, on = c("park", "plot", "visit")]
ddata <- ddata[,.N, by = .(park, plot, visit, genus_species, date)]

data.table::setnames(ddata, c("park", "plot","genus_species", "N"), c("Site","Plot","Taxon", "Biomass"))

##format spatial data to have common identifier with species data ----
spatial[, Site := c("Kakadu","Litchfield","Nitmiluk")[match(substr(plot, 1, 3), c("KAK","LIT","NIT"))]]
spatial[, Plot := stringi::stri_extract_all_regex(str = plot, pattern = "[0-9]{2,3}")
][, Plot := as.integer(sub("^0+(?=[1-9])", "", Plot, perl = TRUE))]

data.table::setnames(spatial, c("plot", "latitude", "longitude"), c("plot_id", "Latitude", "Longitude"))

spatial[, ":="(
  est_date = NULL,
  district = NULL,
  group = NULL
)]

## merging ddata with spatial ----
ddata <- spatial[ddata, on = c("Site","Plot")]
##community data ----

ddata[, ":="(
   Year = format(date, "%Y"),
   Month = format(date, "%m"),
   Day = format(date, "%d"),
   
   visit = NULL,
   date = NULL
)]

##remove NA values in year because of missing dates in original data ----
ddata <- ddata[!is.na(Year)]
ddata <- ddata[!is.na(plot_id)]


df <- ddata
## data type check

# Abundance or biomass: numeric
# Coordinates: numeric
# Dates: POSIXct or 
# Year, month, day columns,: integers or factors
# Plot: factors or integers
# DepthElevation: numeric or factors (if they're a treatment category)
# Taxonomic: characters or factors

# Abundance and/or biomass, here: Height/DBH as Biomass
is.numeric(df$Biomass) # TRUE


# Year, month and day must be integers or factors
# | means or
is.factor(df$Year) | is.integer(df$Year) # FALSE
is.factor(df$Month) | is.integer(df$Month) # FALSE
is.factor(df$Day) | is.integer(df$Day) # FALSE


# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year

# Taxonomic fields must be characters or factors?
is.factor(df$Taxon) | is.character(df$Taxon) # TRUE


## fixes
df$Year <- as.factor(df$Year)
df$Month <- as.factor(df$Month)
df$Day <- as.factor(df$Day)


## fields
# Abundance/Density
min(df$Biomass) > 0 # no zeroes?
sum(df$Biomass=="") > 0 # no blanks


# Year < 2021, month < 12, day < 31
summary(df[,5])
summary(df[,6])
summary(df[,7])


require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)
require(sp)

## map

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=df %>% distinct(Latitude, Longitude, Site), 
                             aes(x=Longitude, y=Latitude, fill = Site), shape=21)

points + coord_fixed(xlim=c(120,140), ylim=c(-20,-10))


## nomenclature

# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(df$Taxon, 'idae$|eae$')
# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(df$Taxon)) %>% word(., start=2, end=-1) %>% unique()
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check

sort(unique(word(df$Taxon, 1))) # check genera


df <- df %>% 
  mutate(Genus = word(Taxon, 1)) %>%
  mutate(Species = word(Taxon, 2)) %>%
  mutate(Taxon = NULL)


### prepare and export
df$Abundance <- rep('', nrow(df)) 
df$DepthElevation <- rep('', nrow(df))
df$StudyID <- rep('', nrow(df))
df$Family <- rep('', nrow(df))

# aggregate abundance records that are same species, plot, and survey day.

df_merged <- df %>% group_by_at(vars(-Biomass)) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(df)[1]-dim(df_merged)[1] # any change in aggregating?

## save dataset
# save the dataset name as an object so we save some typing
dataset_name <- 'russel-smith_2017_trees'
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
df_merged$SampleDescription <- as.factor(with(df_merged, paste(Site, Plot, plot_id, Year, sep='_')))

length(levels(df_merged$SampleDescription)) # how many sampling events?

df_merged <- df_merged[c('Abundance',
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
head(df_merged) # final check!


## Export and spreadsheet prep

dir <- "./Curated/"
write.csv(df_merged, paste0(dir, dataset_name, '_ZY_rawdata.csv'), row.names=F, na='') # replace your initials here



### area coordinate

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
df_coord <- df_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- df_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# calculate area
st_area(df_coord %>% st_convex_hull()) * 0.000001
# 0.5667917 sq km

# Plot the geometries -----------------------------------------------------

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = df_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = df_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim=c(120,140), ylim=c(-20,-10)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()