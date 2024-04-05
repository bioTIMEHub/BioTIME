

# Curation Script ---------------------------------------------------------

# Dataset: LTER Lower Seascheldt Birds
# Location: Scheldt estuary, Belgium
# Curator: Cher Chow
# Date: 15-Dec-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(ggplot2)
require(maps)
require(stringr)
require(lubridate)
require(readxl)

# make sure your working directory is set before running these lines
dt <- read_excel('Originals/HaasePilotto/S004.xlsx', sheet=1, skip=2, col_names=T, na='')

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? N
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? N
unique(dt$Site) # there's only one site for the entire dataset, so Plot will be revalued as blank
str(dt)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case)
dt$Year <- year(parse_date_time(dt$`Sampling date`, '%Y/%m'))
dt$Month <- month(parse_date_time(dt$`Sampling date`, '%Y/%m'))
dt$`Sampling date` <- NULL
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # no blanks

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[,5:6]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
dt$Latitude <- 51.300151
dt$Longitude <- 4.285094
dt$Site <- NULL

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=4.285094, y=51.300151), shape=21)
points

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(40,60))
points_zoom

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Genus <- word(dt$Taxon, 1)
colnames(dt)[1] <- 'Species'

# first, records with undefined species epithets into the BioTIME format of sp without period
dt$Species <- dt$Species %>% str_replace_all(., 'sp.$', 'sp')
sort(unique(dt$Species))
sort(unique(dt$Genus))
dt$Species <- word(dt$Species, start=2, end=-1)

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by_at(vars(-Abundance)) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup()
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? no
# now create empty columns needed to fit to template
dt_merged$Biomass <- ''
dt_merged$Family <- ''
dt_merged$Plot <- ''
dt_merged$DepthElevation <- ''
dt_merged$Day <- ''
dt_merged$StudyID <- ''
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Year, Month, sep='_')))
length(levels(dt_merged$SampleDescription)) # 301 samples

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
                         'Year')] %>% arrange(Year, Month, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, 'LTER_INBO_LowerSeascheldt_Waterbirds_rawdata_CC.csv', row.names=F)



# Convex Hull for centroid ------------------------------------------------


##load libraries
require(sp)
require(rgeos)
require(clipr)
clipr::write_clip(dt_merged)


##1. Convert data points into point spatial object
points204<- SpatialPoints(cbind(dt_merged$Longitude,dt_merged$Latitude))

##2. Calculate convex hull, area and centroid
convhull204<-gConvexHull(points204)
clipr::write_clip(gArea(convhull204))  ##get area

###get centroid
centroid204<-gCentroid(convhull204)    

##to get the coordinates
centroid204@coords
clipr::write_clip((centroid204@coords[c(2,1)]))

