## recurate 57 - update data

# clear environment
rm(list =ls())

require(tidyverse)
require(readxl)
require(maps)


# make sure your working directory is set before running these lines
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/Rypel")


##### clean whole dataset and split ####
dt <- read.csv('ntl7_v11.csv')

# fix species
sp <- read_xlsx('species_names.xlsx')
sp2 <- read.csv("sp_left.csv")

sp <- sp[,c("LTER spname", "Species Name")]
names(sp) <- c("spname", "GenusSpecies")
#change hybrids into sp epithets
sp$GenusSpecies[sp$GenusSpecies=="Esox lucius x Esox masqinongy"] <- "Esox sp"
sp$GenusSpecies[sp$GenusSpecies== "Lepomis cyanellus x Lepomis macrochirus"] <- "Lepomis sp1"
sp$GenusSpecies[sp$GenusSpecies== "Lepomis cyanellus Lepomis gibbosus"] <- "Lepomis sp2"
sp$GenusSpecies[sp$GenusSpecies== "Lepomis macrochirus x Lepomis gibbosus"] <- "Lepomis sp3"

sp$Genus <- word(sp$GenusSpecies, 1) # separate genus to its own column
sp$Species <- word(sp$GenusSpecies, start=2) # species to its own column
sp$Family <- NA

spname <- c(sp$spname, sp2$spname)
Family <- c(sp$Family, sp2$Family)
Genus <- c(sp$Genus, sp2$Genus)
Species <- c(sp$Species, sp2$Sp)
sp_all <- cbind(spname,Family,Genus,Species)


dt<- merge(dt,sp_all, by ="spname", all.x = TRUE)
dt$Abundance <- dt$total_caught/dt$effort


# add coords from metadata
unique(dt$lakeid)
dt$Latitude <- NA
dt$Longitude <- NA

dt$Latitude[dt$lakeid == "AL"] <- (46.0481 + 46.0252)/2
dt$Longitude[dt$lakeid == "AL"] <- (-89.6458 - 89.6124)/2
dt$Latitude[dt$lakeid == "BM"] <- (46.0273 + 46.0051)/2
dt$Longitude[dt$lakeid == "BM"] <- (-89.6335 - 89.5935)/2
dt$Latitude[dt$lakeid == "CR"] <- (46.0047 + 45.9989)/2
dt$Longitude[dt$lakeid == "CR"] <- (-89.6191 - 89.6082)/2
dt$Latitude[dt$lakeid == "SP"] <- (46.0158 + 46.0024)/2
dt$Longitude[dt$lakeid == "SP"] <- (-89.7045 - 89.6945)/2
dt$Latitude[dt$lakeid == "TR"] <- (46.079 + 46.0131)/2
dt$Longitude[dt$lakeid == "TR"] <- (-89.7038 - 89.6464)/2
dt$Latitude[dt$lakeid == "CB"] <- (46.008 + 46.0071)/2
dt$Longitude[dt$lakeid == "CB"] <- (-89.6068 - 89.6057)/2
dt$Latitude[dt$lakeid == "TB"] <- (46.0417 + 46.0407)/2
dt$Longitude[dt$lakeid == "TB"] <- (-89.6869 - 89.6854)/2
dt$Latitude[dt$lakeid == "FI"] <- (43.2922 + 43.2821)/2
dt$Longitude[dt$lakeid == "FI"] <- (-89.6622 - 89.6439)/2
dt$Latitude[dt$lakeid == "ME"] <- (43.146 + 43.0766)/2
dt$Longitude[dt$lakeid == "ME"] <- (-89.4837 - 89.3673)/2
dt$Latitude[dt$lakeid == "MO"] <- (43.0905 + 43.0451)/2
dt$Longitude[dt$lakeid == "MO"] <- (-89.4001 - 89.3251)/2
dt$Latitude[dt$lakeid == "WI"] <- (43.0581 + 43.0487)/2
dt$Longitude[dt$lakeid == "WI"] <- (-89.4317 - 89.4046)/2

summary(dt)
View(dt)

# delete unidentified fish
dt <- dt[dt$spname!= "UNIDENTIFIED" & dt$spname!="LARVALFISH",] # only 45 records

names(dt)[2] <- "Plot"
names(dt)[3] <- "Year"

#split by method
dt_seine <- dt[dt$gearid == "BSEINE",]
dt_trammel <- dt[dt$gearid == "TRAMML",]
dt_fyk <- dt[dt$gearid == "FYKNET",]
dt_cray <- dt[dt$gearid == "CRAYTR" & dt$Plot != "CB" & dt$Plot != "TB",]
dt_min <- dt[dt$gearid == "MINNOW",]
dt_elfish <- dt[dt$gearid == "ELFISH",]
dt_gill <- dt[dt$gearid %in% c("VGN","VGN089","VGN038",
                               "VGN019", "VGN051", "VGN064", 
                               "VGN025", "VGN032", "VGN127"),]




##### dt_seine #####
dt <- dt_seine
dt_name <- "seine"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
               "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # good

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

# coords 44.32424 -89.56503
# area 9163.243 [km^2]




##### dt_cray #####
dt <- dt_cray
dt_name <- "crayfish_traps"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
             "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # good

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)



##### dt_elfish #####
dt <- dt_elfish
dt_name <- "electric_fishing"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
             "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # good

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area



##### dt_fyk #####
dt <- dt_fyk
dt_name <- "fyke_nets"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
             "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # good

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area


##### dt_gill #####
dt <- dt_gill
dt_name <- "gill_nets"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
             "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # pooled nets

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area


##### dt_min #####
dt <- dt_min
dt_name <- "minnow_traps"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
             "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # good

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area


##### dt_trammel #####
dt <- dt_trammel
dt_name <- "trammel_nets"

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

# only keep relevant column
dt <- dt[, c("Plot","Year", "Abundance", "Latitude", 
             "Longitude", "Family","Genus", "Species")]
# Abundance
# no zeros
dim(dt[dt$Abundance==0,])
dt<- dt[dt$Abundance>0,]
dim(dt)
# Year is fine
# no biomass nor elevation
# Lon lat are fine

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points

# seems right
detach('package:maps')

#secondary fields

sort(unique(dt$Plot))

### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$Genus)
unique(dt$Species)

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$Day <-rep('', nrow(dt))
dt$Month <-rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # good

# save the dataset name as an object so we save some typing
dataset_name <- paste0("study57_",dt_name)
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(with(dt_merged,
                                             paste(Plot, Year, sep = '_')
))
length(levels(dt_merged$SampleDescription)) # how many sampling events?
# Now we'll reorder the columns to the proper format:


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
head(dt_merged) # final check!
dt_merged$Plot<- NA

## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt_merged, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='') # replace your initials here
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area



