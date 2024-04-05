
# Curation Script ---------------------------------------------------------

# Dataset: Bee species abundance and composition in three ecosystem types at the Sevilleta National Wildlife Refuge, New Mexico, USA
# Location:  LTER Sevilleta National Wildlife Refuge, New Mexico, USA
# Curator: Mike McWilliam
# Date: 30-Jun-2023

# Set up ------------------------------------------------------------------
rm(list=ls())
library("maps")
library("ggplot2")
library("reshape2")

raw <- read.csv("wright_2021_Original/SEVBeeData2002-2019.csv")
head(raw)

# Put in long format 
dt<-melt(raw, id.vars=c("month", "year", "start_date", "end_date", "complete_sampling_year", "complete_sampling_month", "ecosystem", "transect", "direction", "color"), value.name="Abundance", variable.name="species_code", na.rm=TRUE, factorsAsStrings=TRUE)
head(dt)
nrow(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- as.numeric(dt$Abundance)
dt$Biomass <- ""

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum (no zeroes) 
dt <- subset(dt, Abundance > 0)
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct 

colnames(dt)[colnames(dt)=="year"] <- 'Year' # just a rename
dt$Year <- as.factor(dt$Year)
colnames(dt)[colnames(dt)=="month"] <- 'Month' # just a rename
dt$Month <- as.factor(dt$Month)
dt$Day <- format(as.Date(dt$start_date, "%Y-%m-%d"), "%d")
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

unique(dt$ecosystem) #Creosote (C), Grass (G), Blue Grama (B)
dt$Latitude <- ifelse(dt$ecosystem=="B", 34.3364, ifelse(dt$ecosystem=="C", 34.3329, ifelse(dt$ecosystem=="G", 34.3362, NA)))
dt$Longitude <- ifelse(dt$ecosystem=="B", -106.6345, ifelse(dt$ecosystem=="C",  -106.7358, ifelse(dt$ecosystem=="G", -106.7212, NA)))
head(dt)

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 15
lw_ratio <- 0.75

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_point(data=points, aes(x=Longitude, y=Latitude), col="red", shape=21)+
  #coord_fixed()+ 
coord_cartesian(c(mid[2]-a, mid[2]+a), c(mid[1]-a*lw_ratio, mid[1]+a*lw_ratio))+
  labs(x='Longitude', y='Latitude')+
  theme_bw() + theme(panel.grid=element_blank(), aspect.ratio=lw_ratio)
world

# Taxonomic fields ---------------------------------------------------
# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

# match species with species list data
spp <- read.csv("wright_2021_Original/SEVBeeSpeciesList2002-2019.csv")
head(spp)
dt[,c("Family", "Genus", "Species")] <- spp[match(dt$species_code, spp$code), c("family", "genus", "species")]

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Family))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc are factors/integers? 
# elevation or depth will normally be numeric unless in a treatment format. 

dt$DepthElevation <- ifelse(dt$ecosystem=="B", 1616, ifelse(dt$ecosystem=="C", 1615, ifelse(dt$ecosystem=="G", 1670, NA)))

#  ecosystem / transect 
dt$Plot <- dt$transect # dt$ecosystem
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- ""

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Family", "Genus", "Species","Year","Month", "Day","start_date", "Plot", "Abundance", "Biomass","Latitude", "Longitude", "DepthElevation", "StudyID")], sum)

dim(dt)[1]-dim(dt_merged)[1] # changes after aggregating

dt_merged$species_code <- paste(dt_merged$Genus, dt_merged$Species)
taxa_plot <- unique(dt_merged$species_code)[c(4,5, 10)]
plot_plot <-unique(dt_merged$Plot)[1]
to_plot <- dt_merged[dt_merged$species_code %in% taxa_plot & dt_merged$Plot %in% plot_plot,]
#sort(to_plot$start_date)

ggplot(to_plot, aes(x=as.Date(start_date), y=Abundance, group=Plot, col=Plot))+
geom_point()+geom_line()+
facet_wrap(~species_code)


dataset.name <- 'wright_2021'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Month, Day, Plot, sep='_'))) 

length(levels(dt_merged$SampleDescription)) # 54 samples

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
                         'StudyID')] 
                         
head(dt_merged) # final check 
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset.name, '_rawdata_MM.csv'), row.names=F)

## CC fixes start (dt_merged is the same as dt)

dt$Species[str_which(dt$Species, '^[:digit:]+$')] # check filter for specific epithets that are just numbers
dt$Species[str_which(dt$Species, '^[:digit:]+$')] <- paste0('sp', dt$Species[str_which(dt$Species, '^[:digit:]+$')])
# add sp in front

# View taxa again
dt %>% distinct(Family, Genus, Species) %>% arrange(Family, Genus, Species) %>% View

dt %>% distinct(Family, Genus, Species) %>% 
  filter(str_detect(Species, '[:punct:]')) %>% 
  arrange(Family, Genus, Species) %>% View

dt$Species <- str_remove_all(dt$Species, '\\?') # remove question marks
# handle affinis
dt$Species[str_which(dt$Species, 'aff\\_[:alnum:]+$')] %>% unique # check filter for aff with no number designation
dt$Species[str_which(dt$Species, 'aff\\_[:alnum:]+$')] <- dt$Species[str_which(dt$Species, 'aff\\_[:alnum:]+$')] %>% 
  str_replace(., '\\_', '. ') %>% paste('sp1', ., sep = ' ')
# now nr. and cf.
dt$Species[str_which(dt$Species, '(nr|cf)\\_[:alnum:]+$')]
dt$Species[str_which(dt$Species, '(nr|cf)\\_[:alnum:]+$')] <- dt$Species[str_which(dt$Species, '(nr|cf)\\_[:alnum:]+$')] %>% 
  str_replace(., '\\_', '. ') %>% paste('sp1', ., sep = ' ')

dt$Species <- str_replace_all(dt$Species, 'aff_daleae_2', 'sp2 aff. daleae') # special number designation
dt$Species <- str_remove(dt$Species, '\\_nomen\\_nudum$') # remove, described by Gardner and Gibbs 2023

# check now
dt %>% distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus, Species) %>% View

dt$Species <- str_replace_all(dt$Species, 'n\\_sp', 'sp1')

dt %>% distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus, Species) %>% View

sort(unique(dt$Species)) # still some weird working names in capitals just for Lassioglossum
dt %>% distinct(Genus, Species) %>% 
  filter(Genus == 'Lasioglossum') %>% View
replace <- c('^A$' = 'sp59',
             '^TEX$' = 'sp60', 
             '^TEX01$' = 'sp61')

dt$Species <- str_replace_all(dt$Species, replace)
dt %>% distinct(Family, Genus, Species) %>% 
  arrange(Family, Genus, Species) %>% View

dt$SampleDescription <- str_remove_all(dt$SampleDescription, '^34.3329_-106.7358_')
dt$Plot <- ''

## CC fixes end

# Convex Hull for centroid ------------------------------------------------

# load libraries
library(sf)
library("clipr")
library("dplyr")

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

centroid 
area