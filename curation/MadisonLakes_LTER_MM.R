
# Curation Script ---------------------------------------------------------

# Dataset: North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current
# Location:  WI USA
# Curator: Mike McWilliam
# Date: 17 / 7 / 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")
	
dt <- read.csv("MadisonLakes_LTER_Original/ntl88_v13.csv")
head(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- as.numeric(dt$cells_per_ml)
#dt$Biomass <- as.numeric(dt$biovolume_conc)
dt$Biomass <- as.numeric(dt$biomass_conc)

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum
dt <- subset(dt, Abundance > 0) # no zeros
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 
min(dt$Biomass) # check the minimum
dt <- subset(dt, Biomass > 0) # no zeros
min(dt$Biomass)
sum(dt$Biomass=="") # no blanks 



############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct

head(dt)
dt$Year <- format(as.Date(dt$sampledate, "%Y-%m-%d"), "%Y")
dt$Month <- format(as.Date(dt$sampledate, "%Y-%m-%d"), "%m")
dt$Day <- format(as.Date(dt$sampledate, "%Y-%m-%d"), "%d")
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

unique(dt$lakeid)

dt$Latitude <- ifelse(dt$lakeid=="MO", 43.0534, 
ifelse(dt$lakeid=="ME", 43.1113, 
ifelse(dt$lakeid=="FI", 43.28715, 
ifelse(dt$lakeid=="WI", 43.0581, NA))))
dt$Longitude <- ifelse(dt$lakeid=="MO", -89.3626, 
ifelse(dt$lakeid=="ME", -89.4255, 
ifelse(dt$lakeid=="FI", -89.65305, 
ifelse(dt$lakeid=="WI", -89.41815, NA))))

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 15
lw_ratio <- 1

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

head(dt)
sort(unique(dt$taxa_name))
dt$taxa_name <- trimws(dt$taxa_name) # trims white space
sort(unique(dt$taxa_name))
remove <- c("", "Miscellaneous", "Colonial ", "Colonial", "Chlorophyta", "Cyanophyta", "Cryptophyta", "Chrysophyceae", "Chroococcaceae", "Pyrrhophyta", "Chlorococcaceae", "Nostocales", "Bacteria (photosynthetic)")
dt <- dt[!dt$taxa_name %in% remove, ]

spp <- unique(dt[,c("taxa_name", "genus")])
spp$taxa <- spp$taxa_name
spp$taxa <- gsub("sp. ", "sp.", spp$taxa)
spp$taxa <- gsub("cf. ", "cf._", spp$taxa)
spp$taxa <- gsub("cf ", "cf_", spp$taxa)
spp$taxa <- gsub(" var. ", "_var_", spp$taxa)
spp$taxa <- gsub(" (single)", "", spp$taxa)
spp$taxa <- gsub("capucina capicina", "capucina_capicina", spp$taxa)
spp$taxa <- gsub("(palmeloid stage)", "sp.", spp$taxa)
spp$taxa <- gsub("gracile tenue", "gracile_tenue", spp$taxa)
spp$taxa <- gsub("capucina subsp.", "capucina_subsp.", spp$taxa)
spp$match <- ifelse(spp$taxa==spp$genus, spp$taxa, NA)
spp$match <- ifelse(spp$taxa==paste(spp$genus,""), spp$taxa, spp$match)
spp$match[!is.na(spp$match)] <- paste(spp$match[!is.na(spp$match)], "sp.")
spp

spp$match2 <- spp$match
spp$match2 <- ifelse(is.na(spp$match2), spp$taxa, spp$match2)
dt$Taxon <- spp$match2[match(dt$taxa_name, spp$taxa_name)]
spp

dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Taxon, 2)
dt$Family <- ""

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))

# Secondary fields ---------------------------------------------------

head(dt)

# trawl, plot, transect etc / # elevation or depth 

dt$DepthElevation <- dt$depth_range

head(dt)
unique(dt$sta)
dt$Plot <- dt$lakeid
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.

match_cols <- c("Family", "Genus", "Species","Year","Month", "Day", "Plot", "sampledate", "Taxon", "Latitude", "Longitude", "DepthElevation", "StudyID")
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Abundance", match_cols)], sum)
dt_biomass <- aggregate(Biomass ~ ., data = dt[,c("Biomass", match_cols)], sum)

dt_merged$x <- apply( dt_merged[ , match_cols ] , 1 , paste , collapse = "-" )
dt_biomass$x <- apply( dt_biomass[ , match_cols ] , 1 , paste , collapse = "-" )
dt_merged$Biomass <- dt_biomass$Biomass[match( dt_merged$x, dt_biomass$x )]

head(dt_merged)

ggplot(dt_merged[dt_merged$Taxon%in% unique(dt_merged$Taxon)[1:5],], aes(x=Abundance, y=Biomass, colour=Taxon))+
geom_point()+scale_y_log10()+scale_x_log10()#+facet_wrap(~Taxon)

ggplot(dt_merged[dt_merged$Taxon %in% unique(dt$Taxon)[1:6],], aes(x=as.Date(sampledate, "%Y-%m-%d"), y=Abundance, group=Plot, col=Plot))+
geom_line()+
facet_wrap(~Taxon, scales="free_y")

dim(dt)[1]-dim(dt_merged)[1] # any change after aggregating 

dataset.name <- 'MadisonLakes_LTER'
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
                         
head(dt_merged) # final check :)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset.name, '_rawdata_MM.csv'), row.names=F)

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
