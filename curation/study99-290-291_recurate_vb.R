# Curation Script ---------------------------------------------------------

# Dataset:  study 99 split
# Curator: Viviana Brambilla

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(stringr)
require(lubridate)
require(readxl)
require(sf)

##### dwca-csiro_eac_eddy_1978_1984 #####
# CSIRO, Catch records from eddy studies off the New South Wales coast, Australia 1979-1980

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/watts/dwca-csiro_eac_eddy_1978_1984-v1.8/")

sb <- read.delim("occurrence.txt")

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(occurrenceRemarks) %>% summarize(n()) 


#subset the data to only include the data for BioTIME
dt<-sb %>% select(eventID, individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)
dt$Year <- as.numeric(substr(dt$eventDate, 1,4))
dt$Month <- as.numeric(substr(dt$eventDate, 6,7))
dt$Day <- as.numeric(substr(dt$eventDate, 9,10))

summary(dt)

#check what it looks like
plot(dt$Year, dt$individualCount)


#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())

#all seems coherent

# fix species names

sort(unique(dt$scientificName))
dt$Genus<- word(dt$scientificName,1)
dt$Family<- NA
dt$Family[str_detect(dt$Genus, "idae")]<- dt$Genus[str_detect(dt$Genus, "idae")]
dt$Genus[str_detect(dt$Genus, "idae")] <- NA
dt$Species[str_detect(dt$Genus, "idae")] <- NA

sum(!is.na(dt$Family))/nrow(dt)
# another 19.9% are only at the family level
sort(unique(dt$Family))
#take punctuation out - fine to keep the first family of the groups as that maintains records as of different taxa
dt$Family <- gsub('[[:punct:] ]+','',dt$Family)
sort(unique(dt$Family))

sort(unique(dt$Genus))
dt[dt$Genus=="" & is.na(dt$Family),] 
dt <- dt[dt$scientificName != "",]

(nrow(dt[dt$Genus == "unknown",]) + nrow(dt[dt$Genus == "unknown/other",]))/nrow(dt)
# 41.6% of occurrences are not Ided
sort(unique(dt$Genus)) #all looks good

dt$Species <- NA
dt$Species <- word(dt$scientificName,2)
dt$Species[str_detect(dt$scientificName, "idae")] <- NA
sort(unique(dt$Species))
unique(dt$scientificName[dt$Species == "(no"])
dt$Species[dt$Species == "(no"] <- NA
unique(dt$scientificName[dt$Species == "[SS0704,"])
unique(dt$scientificName[dt$Genus == "Gennadas"])
dt$Species[dt$Species == "[SS0704,"] <- "sp2"
dt$Species[dt$scientificName == "Gennadas sp. 1 [CAAB old list, 2000]"] <- "sp1"

unique(dt$scientificName[dt$Species == "cf."])
unique(dt$scientificName[dt$Genus == "Parasergestes"])
dt$Species[dt$scientificName == "Parasergestes spp."] <- "spp"
dt$Species[dt$scientificName == "Parasergestes cf. vigilax 'bilobe'"] <- "sp1"
dt$Species[dt$scientificName == "Parasergestes cf. vigilax 'trilobe'"] <- "sp2"

unique(dt$scientificName[dt$Species == "sp."])
unique(dt$scientificName[dt$Genus == "Cerataspis"]) #all sp.1 are unique olso if sp.
dt$Species[dt$Species == "sp."] <- "sp"

unique(dt$scientificName[dt$Species == "spp."])
dt$Species[dt$Species == "sp"] <- "spp"

unique(dt$scientificName[dt$Species == "near"])
dt$Species[str_detect(dt$scientificName, "near")] <- word(dt$scientificName[str_detect(dt$scientificName, "near")],3)

dt$Species[dt$Genus == "Caridea" & !is.na(dt$Genus)] <- "sp"
dt$Family[dt$Species == "fish" & !is.na(dt$Species)] <- "fish"
dt$Species[dt$Species == "fish" & !is.na(dt$Species)] <- NA
dt$Family[dt$Genus == "unknown/other" & !is.na(dt$Genus)] <- "unknown/other"
dt$Genus[dt$Genus == "unknown/other" & !is.na(dt$Genus)] <- NA

dt$Species <- as.factor(dt$Species)
dt$Genus <- as.factor(dt$Genus)
dt$Family <- as.factor(dt$Family)

summary(dt)

#View(dt[is.na(dt$Species),])

#rename and delete unnecessary columns
dt<-rename(dt, Abundance = individualCount)
dt<-rename(dt, Longitude = decimalLongitude)
dt<-rename(dt, Latitude = decimalLatitude)
dt<-rename(dt, Plot = eventID)
dt$scientificName<- NULL
dt$eventDate<- NULL

dt$Plot <- as.factor(dt$Plot)

# add depth 
event <- read.delim('event.txt', header = T)
event <- event[,c("eventID", "minimumDepthInMeters",
                  "maximumDepthInMeters")]
dt <- left_join(dt,event, join_by(Plot == eventID) )
dt$DepthElevation <- - (dt$maximumDepthInMeters-dt$minimumDepthInMeters)/2
dt$maximumDepthInMeters<- NULL
dt$minimumDepthInMeters<- NULL

summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Primary field check 

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks
dt <- dt[!is.na(dt$Abundance),]

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# Taxonomic field check 

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))

dt[is.na(dt$Species),] %>% View()


# Prepare raw data 
dt$Abundance
dt$Biomass <- NA
dt$StudyID <- NA


# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # 50 got pooled

# save the dataset name as an object so we save some typing
#CSIRO, Catch records from eddy studies off the New South Wales coast, Australia 1979-1980

dataset_name <- "study99_CSIRO_eddy_studies_NSW"
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(dt_merged$Plot)
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

write.csv(dt_merged, paste0(dataset_name, '_rawdata_recurate_VB.csv'), row.names=F, na='') # replace your initials here
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

##### dwca-csiro_fish_survey_1978_1979-v1.10#####
# CSIRO, fish surveys by the Courageous, 1978-1979

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/watts/dwca-csiro_fish_survey_1978_1979-v1.10/")

sb <- read.delim("occurrence.txt")

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(occurrenceRemarks) %>% summarize(n())

#subset the data to only include the data for BioTIME
dt<-sb %>%   filter(occurrenceRemarks == "Occurrence Type: Catch composition Catch method: Stern Trawl" ) %>%
  select(eventID, individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)

dt$Year <- as.numeric(substr(dt$eventDate, 1,4))
dt$Month <- as.numeric(substr(dt$eventDate, 6,7))
dt$Day <- as.numeric(substr(dt$eventDate, 9,10))

summary(dt)

#check what it looks like
plot(dt$Year, dt$individualCount)


#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n())

#all seems coherent

# fix species names

sort(unique(dt$scientificName))
dt$Genus<- word(dt$scientificName,1)
dt$Species <- word(dt$scientificName,2)
dt$Family<- NA
dt$Family[str_detect(dt$Genus, "idae")]<- dt$Genus[str_detect(dt$Genus, "idae")]
dt$Genus[str_detect(dt$Genus, "idae")] <- NA
dt$Species[str_detect(dt$scientificName, "idae")] <- NA

sum(!is.na(dt$Family))/nrow(dt)
# 2.7% are only at the family level
sort(unique(dt$Family))
#take punctuation out - fine to keep the first family of the groups as that maintains records as of different taxa
dt$Family <- gsub('[[:punct:] ]+','',dt$Family)
sort(unique(dt$Family))

sort(unique(dt$Genus))
dt[dt$Genus=="" & is.na(dt$Family),] 

dt[dt$Genus=="White" & !is.na(dt$Genus),] 
dt$Species[dt$Genus=="White" & !is.na(dt$Genus)] <- "obesus"
dt$Genus[dt$Genus=="White" & !is.na(dt$Genus)] <- "Triaenodon"
dt[dt$Genus=="(Bearded" & !is.na(dt$Genus),] 
dt$Species[dt$Genus=="(Bearded" & !is.na(dt$Genus)] <- "flavescens"
dt$Genus[dt$Genus=="(Bearded" & !is.na(dt$Genus)] <- "Perca"


sort(unique(dt$Species))

unique(dt$scientificName[dt$Species == "&" ])
unique(dt$scientificName[dt$Genus == "Astacidea" ])
unique(dt$scientificName[dt$Genus == "Penaeoidea" ])
dt$Species[dt$Species == "&"] <- "spp"


sort(unique(dt$scientificName[dt$Species == "sp."]))
dt$Species[dt$scientificName == "Coelorinchus sp. 1"] <- "sp1"
dt$Species[dt$scientificName == "Coelorinchus sp. 3"] <- "sp3"
dt$Species[dt$Species == "sp." & !is.na(dt$Species)] <- "sp"

unique(dt$scientificName[dt$Species == "cf"])
unique(dt$scientificName[dt$Genus == "Lepidotrigla"])
dt$Species[dt$scientificName == "Lepidotrigla cf bispinosa [Gomon, pers comm]"] <- "bispinosa"
dt$Species[dt$scientificName == "Lepidotrigla sp. 2 [in Sainsbury et al, 1985]"] <- "sp1"
dt$Species[dt$scientificName == "Lepidotrigla cf japonica"] <- "japonica"
unique(dt$scientificName[dt$Genus == "Antigonia"])  
dt$Species[dt$scientificName == "Antigonia cf malayana (WA1) [of G. Yearsley]"] <- "malayana"
unique(dt$scientificName[dt$Genus == "Chaunax"])  
dt$Species[dt$scientificName == "Chaunax cf breviradius"] <- "breviradius"
unique(dt$scientificName[dt$Genus == "Saurida"])  
dt$Species[dt$scientificName == "Saurida cf filamentosa"] <- "filamentosa"
unique(dt$scientificName[dt$Genus == "Satyrichthys"])  
dt$Species[dt$scientificName == "Satyrichthys cf moluccense"] <- "moluccense"

unique(dt$scientificName[dt$Species == "near"])

dt$Species <- as.factor(dt$Species)
dt$Genus <- as.factor(dt$Genus)
dt$Family <- as.factor(dt$Family)

summary(dt)

#View(dt[is.na(dt$Species),])

#rename and delete unnecessary columns
dt<-rename(dt, Abundance = individualCount)
dt<-rename(dt, Longitude = decimalLongitude)
dt<-rename(dt, Latitude = decimalLatitude)
dt<-rename(dt, Plot = eventID)
dt$scientificName<- NULL
dt$eventDate<- NULL

dt$Plot <- as.factor(dt$Plot)

# add depth 
event <- read.delim('event.txt', header = T)
event <- event[,c("eventID", "minimumDepthInMeters",
                  "maximumDepthInMeters")]
dt <- left_join(dt,event, join_by(Plot == eventID) )
dt$DepthElevation <- - (dt$maximumDepthInMeters-dt$minimumDepthInMeters)/2
dt$maximumDepthInMeters<- NULL
dt$minimumDepthInMeters<- NULL

summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Primary field check 

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks
dt<- dt[!is.na(dt$Abundance),]

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# Taxonomic field check 

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))

dt[is.na(dt$Species),] %>% View()


# Prepare raw data 
dt$Abundance
dt$Biomass <- NA
dt$StudyID <- NA


# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # ok

# save the dataset name as an object so we save some typing
#CSIRO, fish surveys by the Courageous, 1978-1979

dataset_name <- "study99_CSIRO_courageous_78-79"
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(dt_merged$Plot)
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

write.csv(dt_merged, paste0(dataset_name, '_rawdata_recurate_VB.csv'), row.names=F, na='') # replace your initials here
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




##### dwca-csiro_fish_survey_1980-v2.9#####
# CSIRO, fish surveys by the Soela, 1980s

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/watts/dwca-csiro_fish_survey_1980-v2.9/")

sb <- read.delim("occurrence.txt")

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n()) # to pool 
sb %>% group_by(lifeStage) %>% summarize(n())
sb %>% group_by(occurrenceRemarks) %>% summarize(n()) 
dt_sub<-sb %>% select(eventID, occurrenceRemarks,individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)
plot(dt_sub$decimalLatitude, dt_sub$individualCount, col = as.factor(dt_sub$occurrenceRemarks))

#subset the data to only include the data for BioTIME
dt<-sb %>%   filter(occurrenceRemarks != "Occurrence Type: Catch measurement" ) %>%
  select(eventID, individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)
dt$Year <- as.numeric(substr(dt$eventDate, 1,4))
dt$Month <- as.numeric(substr(dt$eventDate, 6,7))
dt$Day <- as.numeric(substr(dt$eventDate, 9,10))

summary(dt)

#check what it looks like
plot(dt$Year, dt$individualCount)
tail(sort(dt$individualCount))

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n())


# fix species names

sort(unique(dt$scientificName))
dt$Genus<- word(dt$scientificName,1)
dt$Species <- word(dt$scientificName,2)
dt$Family<- NA
dt$Family[str_detect(dt$Genus, "idae")]<- dt$Genus[str_detect(dt$Genus, "idae")]
dt$Genus[str_detect(dt$Genus, "idae")] <- NA
dt$Species[str_detect(dt$scientificName, "idae")] <- NA

sum(!is.na(dt$Family))/nrow(dt)
# 7.1% are only at the family level
sort(unique(dt$Family))
#take punctuation out - fine to keep the first family of the groups as that maintains records as of different taxa
dt$Family <- gsub('[[:punct:] ]+','',dt$Family)
sort(unique(dt$Family))

sort(unique(dt$Genus))
dt[dt$Genus=="" & is.na(dt$Family),] 

sort(unique(dt$Species))
unique(dt$scientificName[dt$Species == "&" ])
unique(dt$scientificName[dt$Genus == "Astacidea" ])
unique(dt$scientificName[dt$Genus == "Penaeoidea" ])
dt$Species[dt$Species == "&"] <- "spp"


sort(unique(dt$scientificName[dt$Species == "sp."]))
dt$Species[dt$Species == "sp." & !is.na(dt$Species)] <- "sp"

sort(unique(dt$scientificName[dt$Species == "spp."]))
dt$Species[dt$Species == "sp." & !is.na(dt$Species)] <- "spp"

unique(dt$scientificName[dt$Species == "cf"])
dt$Species[dt$Species == "cf"&!is.na(dt$Species)] <- word(dt$scientificName[dt$Species == "cf"&!is.na(dt$Species)],3)

unique(dt$scientificName[dt$Species == "near"])

dt$Species <- as.factor(dt$Species)
dt$Genus <- as.factor(dt$Genus)
dt$Family <- as.factor(dt$Family)

summary(dt)

#View(dt[is.na(dt$Species),])

#rename and delete unnecessary columns
dt<-rename(dt, Abundance = individualCount)
dt<-rename(dt, Longitude = decimalLongitude)
dt<-rename(dt, Latitude = decimalLatitude)
dt<-rename(dt, Plot = eventID)
dt$scientificName<- NULL
dt$eventDate<- NULL

dt$Plot <- as.factor(dt$Plot)

# add depth 
event <- read.delim('event.txt', header = T)
event <- event[,c("eventID", "minimumDepthInMeters",
                  "maximumDepthInMeters")]
dt <- left_join(dt,event, join_by(Plot == eventID) )
dt$DepthElevation <- - (dt$maximumDepthInMeters-dt$minimumDepthInMeters)/2
dt$maximumDepthInMeters<- NULL
dt$minimumDepthInMeters<- NULL

summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Primary field check 

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks
dt<- dt[!is.na(dt$Abundance),]

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

dt[is.na(dt$Latitude),]

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# Taxonomic field check 

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))

dt[is.na(dt$Species),] %>% View()


# Prepare raw data 
dt$Abundance
dt$Biomass <- NA
dt$StudyID <- NA


# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # looks good

# save the dataset name as an object so we save some typing
#CSIRO, fish surveys by the Soela, 1980s

dataset_name <- "study99_CSIRO_Soela_80s"
# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(dt_merged$Plot)
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

write.csv(dt_merged, paste0(dataset_name, '_rawdata_recurate_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% drop_na() %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area





##### dwca-csiro_nw_shelf_demersal-v1.9 #####
#CSIRO, North West Shelf Demersal Marine Resources Study 1980-1997

dataset_name <- "study99_CSIRO_Demersal_Marine_Resources_80-97"

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/watts/dwca-csiro_nw_shelf_demersal-v1.9/")

sb <- read.delim("occurrence.txt")

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n()) # to pool 
sb %>% group_by(lifeStage) %>% summarize(n())
sb %>% group_by(occurrenceRemarks) %>% summarize(n()) 
plot(sb$individualCount~sb$organismQuantity)

dt_sub<-sb %>% select(eventID, occurrenceRemarks,individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)
plot(dt_sub$decimalLatitude, dt_sub$individualCount, col = as.factor(dt_sub$occurrenceRemarks))



#subset the data to only include the data for BioTIME
dt<-sb %>%   filter(occurrenceRemarks != "Occurrence Type: Catch measurement" & occurrenceRemarks != "Occurrence Type: Catch specimen") %>%
  select(eventID, individualCount, organismQuantity, scientificName,decimalLatitude, decimalLongitude, eventDate)

# dt<-sb %>%   filter(occurrenceRemarks == "Occurrence Type: Catch measurement") %>%
#   select(eventID, individualCount, organismQuantity, scientificName,decimalLatitude, decimalLongitude, eventDate)



dt$Year <- as.numeric(substr(dt$eventDate, 1,4))
dt$Month <- as.numeric(substr(dt$eventDate, 6,7))
dt$Day <- as.numeric(substr(dt$eventDate, 9,10))

summary(dt)

#check what it looks like
plot(dt$Year, dt$individualCount, col = as.factor(dt_sub$occurrenceRemarks))
tail(sort(dt$individualCount))

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n())


# fix species names

sort(unique(dt$scientificName))
dt$Genus<- word(dt$scientificName,1)
dt$Species <- word(dt$scientificName,2)
dt$Family<- NA
dt$Family[str_detect(dt$Genus, "idae")]<- dt$Genus[str_detect(dt$Genus, "idae")]
dt$Genus[str_detect(dt$Genus, "idae")] <- NA
dt$Species[str_detect(dt$scientificName, "idae")] <- NA

sum(!is.na(dt$Family))/nrow(dt)
# 6% are only at the family level
sort(unique(dt$Family))
#take punctuation out - fine to keep the first family of the groups as that maintains records as of different taxa
dt$Family <- gsub('[[:punct:] ]+','',dt$Family)
sort(unique(dt$Family))

sort(unique(dt$Genus))
dt[dt$Genus=="" & is.na(dt$Family),] 

sort(unique(dt$Species))
unique(dt$scientificName[dt$Species == "&" ])
unique(dt$scientificName[dt$Genus == "Astacidea" ])
unique(dt$scientificName[dt$Genus == "Penaeoidea" ])
dt$Species[dt$Species == "&"] <- "spp"


sort(unique(dt$scientificName[dt$Species == "sp."]))
dt$Species[dt$Species == "sp." & !is.na(dt$Species)] <- "sp"

sort(unique(dt$scientificName[dt$Species == "spp."]))
dt$Species[dt$Species == "sp." & !is.na(dt$Species)] <- "spp"

unique(dt$scientificName[dt$Species == "cf"])
dt$Species[dt$Species == "cf"&!is.na(dt$Species)] <- word(dt$scientificName[dt$Species == "cf"&!is.na(dt$Species)],3)

unique(dt$scientificName[dt$Species == "near"])

dt$Species <- as.factor(dt$Species)
dt$Genus <- as.factor(dt$Genus)
dt$Family <- as.factor(dt$Family)

summary(dt)

#View(dt[is.na(dt$Species),])

#rename and delete unnecessary columns
dt<-rename(dt, Abundance = individualCount)
dt<-rename(dt, Longitude = decimalLongitude)
dt<-rename(dt, Latitude = decimalLatitude)
dt<-rename(dt, Plot = eventID)
dt$scientificName<- NULL
dt$eventDate<- NULL

dt$Plot <- as.factor(dt$Plot)

# add depth 
event <- read.delim('event.txt', header = T)
event <- event[,c("eventID", "minimumDepthInMeters",
                  "maximumDepthInMeters")]
dt <- left_join(dt,event, join_by(Plot == eventID) )
dt$DepthElevation <- - (dt$maximumDepthInMeters-dt$minimumDepthInMeters)/2
dt$maximumDepthInMeters<- NULL
dt$minimumDepthInMeters<- NULL

summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Primary field check 

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks
dt<- dt[!is.na(dt$Abundance),]

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# Taxonomic field check 

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))

dt[is.na(dt$Species),] %>% View()


# Prepare raw data 
dt$Abundance
dt$Biomass <- NA
dt$StudyID <- NA


# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # a lot got pulled!

# save the dataset name as an object so we save some typing
#CSIRO, fish surveys by the Soela, 1980s

# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(dt_merged$Plot)
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

write.csv(dt_merged, paste0(dataset_name, '_rawdata_recurate_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% drop_na() %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area




##### ddwca-csiro_jack_mackeral_1975_1978-v1.12 #####
#CSIRO, Jack Mackerel (Trachurus declivis) surveys, Tasman Sea, Australia (1978)

dataset_name <- "study99_CSIRO_Tasmania_Pelagic_survey_75-78"

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/watts/dwca-csiro_jack_mackeral_1975_1978-v1.12/")

sb <- read.delim("occurrence.txt")

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n()) # to pool 
sb %>% group_by(lifeStage) %>% summarize(n())
sb %>% group_by(occurrenceRemarks) %>% summarize(n()) 
plot(sb$individualCount~sb$organismQuantity)

dt_sub<-sb %>% select(eventID, occurrenceRemarks,individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)
plot(dt_sub$decimalLatitude, dt_sub$individualCount, col = as.factor(dt_sub$occurrenceRemarks))



#subset the data to only include the data for BioTIME
dt<-sb %>% select(eventID, individualCount, scientificName,decimalLatitude, decimalLongitude, eventDate)
dt$Year <- as.numeric(substr(dt$eventDate, 1,4))
dt$Month <- as.numeric(substr(dt$eventDate, 6,7))
dt$Day <- as.numeric(substr(dt$eventDate, 9,10))

summary(dt)

# only one year - take out!

##### dwca-csiro_orange_roughy_1987_1990-v2.10 #####
#CSIRO, Abundance and distribution of orange roughy in southeast Australian waters 1987-1979

dataset_name <- "study99_CSIRO_orange_roughy_87-79"

# clear environment
rm(list =ls())

# make sure your working directory is set
setwd("~/iCloud Drive (Archive)/Documents/BioTIME/Update datasets/watts/dwca-csiro_orange_roughy_1987_1990-v2.10/")

sb <- read.delim("occurrence.txt")

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n()) # to pool 
sb %>% group_by(lifeStage) %>% summarize(n())
sb %>% group_by(occurrenceRemarks) %>% summarize(n()) 
plot(sb$individualCount~sb$organismQuantity)

dt_sub<-sb %>% select(eventID, occurrenceRemarks,individualCount,scientificName,decimalLatitude, decimalLongitude, eventDate)
plot(dt_sub$decimalLatitude, dt_sub$individualCount, col = as.factor(dt_sub$occurrenceRemarks))



#subset the data to only include the data for BioTIME
dt<-sb %>%   filter(occurrenceRemarks != "Occurrence Type: Catch measurement" & occurrenceRemarks != "Occurrence Type: Catch specimen") %>%
  select(eventID, individualCount, organismQuantity, scientificName,decimalLatitude, decimalLongitude, eventDate)

dt<-sb %>%   filter(occurrenceRemarks == "Occurrence Type: Catch composition Catch method: Stern Trawl") %>%
  select(eventID, individualCount, organismQuantity, scientificName,decimalLatitude, decimalLongitude, eventDate)


dt$Year <- as.numeric(substr(dt$eventDate, 1,4))
dt$Month <- as.numeric(substr(dt$eventDate, 6,7))
dt$Day <- as.numeric(substr(dt$eventDate, 9,10))

summary(dt)



#check what it looks like
plot(dt$Year, dt$individualCount)
tail(sort(dt$individualCount))

dt[dt$individualCount %in%tail(sort(dt$individualCount)),]
sb[sb$individualCount %in%tail(sort(dt$individualCount)),]
sort(dt$individualCount[dt$scientificName == "Deania calceus"])

#check out the differences in sampling method
sb %>% group_by(associatedOccurrences) %>% summarize(n())
sb %>% group_by(organismQuantityType) %>% summarize(n())
sb %>% group_by(samplingProtocol) %>% summarize(n())
sb %>% group_by(sex) %>% summarize(n())


# fix species names

sort(unique(dt$scientificName))
dt$Genus<- word(dt$scientificName,1)
dt$Species <- word(dt$scientificName,2)
dt$Family<- NA
dt$Family[str_detect(dt$Genus, "idae")]<- dt$Genus[str_detect(dt$Genus, "idae")]
dt$Genus[str_detect(dt$Genus, "idae")] <- NA
dt$Species[str_detect(dt$scientificName, "idae")] <- NA

sum(!is.na(dt$Family))/nrow(dt)
# 6% are only at the family level
sort(unique(dt$Family))
#take punctuation out - fine to keep the first family of the groups as that maintains records as of different taxa
dt$Family <- gsub('[[:punct:] ]+','',dt$Family)
sort(unique(dt$Family))

sort(unique(dt$Genus))
dt[dt$Genus=="" & is.na(dt$Family),] 

sort(unique(dt$Species))
unique(dt$scientificName[dt$Species == "&" ])
unique(dt$scientificName[dt$Genus == "Penaeoidea" ])
dt$Species[dt$Species == "&"] <- "spp"


sort(unique(dt$scientificName[dt$Species == "sp."]))
dt$Species[dt$Species == "sp." & !is.na(dt$Species)] <- "sp"
dt$Species[dt$scientificName == "Trachonurus sp. cf villosus" & !is.na(dt$Species)] <- "villosus"

unique(dt$scientificName[dt$Species == "cf"])
unique(dt$scientificName[dt$Genus == "Alepocephalus"])
dt$Species[dt$Species == "cf"&!is.na(dt$Species)] <- "sp"

unique(dt$scientificName[dt$Species == "near"])

dt$Species <- as.factor(dt$Species)
dt$Genus <- as.factor(dt$Genus)
dt$Family <- as.factor(dt$Family)

summary(dt)

#View(dt[is.na(dt$Species),])

#rename and delete unnecessary columns
dt<-rename(dt, Abundance = individualCount)
dt<-rename(dt, Longitude = decimalLongitude)
dt<-rename(dt, Latitude = decimalLatitude)
dt<-rename(dt, Plot = eventID)
dt$scientificName<- NULL
dt$eventDate<- NULL

dt$Plot <- as.factor(dt$Plot)

# add depth 
event <- read.delim('event.txt', header = T)
event <- event[,c("eventID", "minimumDepthInMeters",
                  "maximumDepthInMeters")]
dt <- left_join(dt,event, join_by(Plot == eventID) )
dt$DepthElevation <- - (dt$maximumDepthInMeters-dt$minimumDepthInMeters)/2
dt$maximumDepthInMeters<- NULL
dt$minimumDepthInMeters<- NULL

summary(dt)

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Primary field check 

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes)
sum(dt$Abundance=="") # should have no blanks
dt<- dt[!is.na(dt$Abundance),]

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
unique(dt$Longitude)
unique(dt$Latitude)

dt[is.na(dt$Latitude),]

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  #coord_fixed(xlim = c(-52, -47), ylim = c(-25,-20)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# yes, correct

# Taxonomic field check 

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

# clean family names
sort(unique(dt$Family))
summary(as.factor(dt$Family))

sort(unique(dt$Genus))
summary(as.factor(dt$Genus))
sum(is.na(dt$Genus))

sort(unique(dt$Species))
summary(as.factor(dt$Species))
sum(is.na(dt$Species))

dt[is.na(dt$Species),] %>% View()


# Prepare raw data 
dt$Abundance
dt$Biomass <- NA
dt$StudyID <- NA


# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Biomass,Family,Genus, Species,
                             Plot,Latitude, Longitude,DepthElevation,
                             Day, Month,Year,StudyID) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] # a lot got pulled!

# save the dataset name as an object so we save some typing
#CSIRO, fish surveys by the Soela, 1980s

# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <-as.factor(dt_merged$Plot)
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

write.csv(dt_merged, paste0(dataset_name, '_rawdata_recurate_VB.csv'), row.names=F, na='') # replace your initials here
write_clip(dt_merged)
#Excel to fill out the curation spreadsheet. 

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% drop_na() %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()
# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)] # copy as lat-long
# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area



