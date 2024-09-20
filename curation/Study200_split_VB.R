## Check 200 ####
## curator: VB
## revision1 stage

library(dplyr)
library(ggplot2)
library(clipr)
require(stringr)
require(sf)

#### read database

BT <- readRDS("queryBTv2_April_2024.rds")
meta <- read.csv("metadataBTv2_April_2024.csv")

names(BT)
names(meta)

st_id <- 200

mt200 <- meta[meta$STUDY_ID == st_id,]
raw200 <- BT[BT$STUDY_ID == st_id,]
mt200$SAMPLE_DESC_NAME
mt200$TITLE
sort(unique(raw200$SAMPLE_DESC))
mt200$COMMENTS
mt200$CURATOR
mt200$METHODS

st_id <- 191

mt191 <- meta[meta$STUDY_ID == st_id,]
raw191 <- BT[BT$STUDY_ID == st_id,]
mt191$SAMPLE_DESC_NAME
mt191$TITLE
sort(unique(raw191$SAMPLE_DESC))
mt191$COMMENTS
mt191$CURATOR
mt191$METHODS


raw200 %>% mutate_if(is.character,as.factor) %>% summary()
72344/nrow(raw200) #71% species level
(72344+12199)/nrow(raw200) #83% genus level

raw191 %>% mutate_if(is.character,as.factor) %>% summary()
23725/nrow(raw191) #46% species level
(23725+12619)/nrow(raw191) #71% genus level


# As different methods were used, wend back to raw data and split by method.
setwd("~/Library/CloudStorage/Dropbox/My Dropbox/BioTIME/study200")

occ <- read.csv("Occurrence.csv")
names(occ) 
occ <- occ %>% select(occurrenceremarks, dynamicproperties, locality, country, 
                      waterbody, year, month, day, eventdate, basisofrecord, 
                      collectioncode, datasetid, species, genus,family, order, 
                      class, phylum,bathymetry, depth, originalscientificname, 
                      scientificname, decimallongitude, decimallatitude)
occ$weight <- as.numeric(word(word(occ$dynamicproperties, 1, sep = ";"), 2, sep = "="))
occ$count <-as.numeric( word(word(occ$dynamicproperties, 3, sep = ";"), 2, sep = "="))
occ$sample_size2 <- (word(occ$dynamicproperties, 2, sep = ";"))
occ$count[str_detect(occ$sample_size2,"observedindividualcount")] <- 
  as.numeric(word(occ$sample_size2[str_detect(occ$sample_size2,"observedindividualcount")], 2, sep = "="))
occ$sample_size2[str_detect(occ$sample_size2,"observedindividualcount")] <- NA
occ$sample_size <- as.numeric(word(word(occ$sample_size2,2, sep = "="), 1, sep = " "))

summary(as.factor(occ$occurrenceremarks))

plot(occ$count~occ$year, col = as.factor(occ$occurrenceremarks))
plot(occ$weight~occ$year, col = as.factor(occ$occurrenceremarks))

occ %>% select(occurrenceremarks, count, weight, sample_size)
occ %>% group_by(occurrenceremarks) %>% mutate(n_events = n_distinct(paste(decimallatitude, decimallongitude,
                                                                           year, month, day, depth, locality)),
                                               n_sample_sizes = n_distinct(sample_size),
                                               n_event_loc = n_distinct(paste(decimallatitude, decimallongitude)),
                                               n_event_depths = n_distinct(depth),
                                               n_event_dates = n_distinct(paste(year, month,day)), 
                                               year_start = min(year),
                                               year_finish = max(year)) %>% 
  select(occurrenceremarks,n_events, n_event_loc,
         n_event_depths, n_event_dates, year_start, year_finish) %>% unique() # %>% write.csv("study191and200raw.csv")

occ$sd1 <- paste(occ$decimallatitude, occ$decimallongitude,
                 occ$year, occ$month, occ$day, occ$depth, occ$locality, sep = "_")
length(unique(occ$sd1))
occ$sd2 <- paste(occ$decimallatitude, occ$decimallongitude,
                 occ$year, occ$month, occ$day, occ$depth, sep = "")
length(unique(occ$sd2))

occ <- occ %>% 
  select(occurrenceremarks, count, weight, phylum, class, order, family, genus, species, sd1,locality, decimallatitude, decimallongitude, depth, day, month, year) %>%
  rename(Abundance = count,
        Biomass = weight,
        Family = family,
        Genus = genus,
        Species = species,
        SampleDescription = sd1,
        Plot = locality,
        Latitude = decimallatitude,
        Longitude = decimallongitude,
        DepthElevation = depth,
        Day = day,
        Month = month,
        Year = year,
        Phylum = phylum,
        Order = order,
        Class = class)

SpeciesX<- str_split_fixed(occ$Species, " ",3)[,3]
SpeciesX[SpeciesX == ""]<- str_split_fixed(occ$Species[SpeciesX == ""], " ",3)[,2]
ck <- cbind(occ$Species, SpeciesX)
occ$Species <- SpeciesX
occ <- as.data.frame(occ %>% mutate(StudyID = "191_200"))
summary(occ)
summary(occ[occ$Abundance==0,])
summary(occ[occ$Biomass==0,])
occ <- occ[occ$Abundance != 0 | occ$Biomass != 0,]
occ$DepthElevation <- -occ$DepthElevation
occ$Plot <- NA
occ$Species[occ$Species == ""]
occ <-  occ%>% mutate_if(is.character,as.factor)
summary(occ)

unique(occ$occurrenceremarks)

occ %>% group_by(occurrenceremarks) %>% mutate(n_events = n_distinct(SampleDescription),
                                               n_event_loc = n_distinct(paste(Latitude, Longitude)),
                                               n_event_depths = n_distinct(DepthElevation),
                                               n_event_dates = n_distinct(paste(Year, Month, Day)), 
                                               year_start = min(Year),
                                               year_finish = max(Year),
                                               length = year_finish - year_start) %>% 
  select(occurrenceremarks,n_events, n_event_loc,
         n_event_depths, n_event_dates, year_start, year_finish, length) %>% unique() %>% View() #write.csv("study191and200raw.csv")



# dt1 Gear Type: OTTER TRAWL ####

dt1 <- occ[occ$occurrenceremarks == "Gear Type: OTTER TRAWL",]

dt1 <- dt1[,c("Abundance",
                    "Biomass",
                    "Family",
                    "Genus",
                    "Species",
                    "SampleDescription",
                    "Plot",
                    "Latitude",
                    "Longitude",
                    "DepthElevation",
                    "Day",
                    "Month",
                    "Year",
                    "StudyID",
                    "Phylum",
                    "Order",
                    "Class")]

dt1$StudyID <- "191-200_split1"
summary(dt1)

write.csv(dt1, "NEFSC Benthic Database (OBIS-USA) - Otter Trawl_recurate191-200_split1_VB.csv") 
clipr::write_clip(dt1)

# dt2 Gear Type: SMITH MCINTYRE GRAB ####

dt2 <- occ[occ$occurrenceremarks == "Gear Type: SMITH MCINTYRE GRAB",]

dt2 <- dt2[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]


dt2$StudyID <- "191-200_split2"
summary(dt2)

write.csv(dt2, "NEFSC Benthic Database (OBIS-USA) - Smith Mcintyre Grab_recurate191-200_split2_VB.csv") 
clipr::write_clip(dt2)

# dt3 Gear Type: SCALLOP DREDGE ####

dt3 <- occ[occ$occurrenceremarks == "Gear Type: SCALLOP DREDGE",]

dt3 <- dt3[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt3$StudyID <- "191-200_split3"
summary(dt3)

write.csv(dt3, "NEFSC Benthic Database (OBIS-USA) - Scallop Dredge_recurate191-200_split3_VB.csv") 
clipr::write_clip(dt3)

# dt4 Gear Type: 1-METER NAT. DREDGE ####

dt4 <- occ[occ$occurrenceremarks == "Gear Type: 1-METER NAT. DREDGE",]

dt4 <- dt4[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt4$StudyID <- "191-200_split4"
summary(dt4)

write.csv(dt4, "NEFSC Benthic Database (OBIS-USA) - 1m nat. dredge_recurate191-200_split4_VB.csv") 
clipr::write_clip(dt4)

#dt5 Gear Type: RING OR STRAMIN NET ####

dt5 <- occ[occ$occurrenceremarks == "Gear Type: RING OR STRAMIN NET",]

dt5 <- dt5[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt5$StudyID <- "191-200_split5"
summary(dt5)

write.csv(dt5, "NEFSC Benthic Database (OBIS-USA) - ring or stramin net_recurate191-200_split5_VB.csv") 
clipr::write_clip(dt5)

# dt6 Gear Type: ISAAC-KIDD MID-WATER ####

dt6 <- occ[occ$occurrenceremarks == "Gear Type: ISAAC-KIDD MID-WATER",]

dt6 <- dt6[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt6$StudyID <- "191-200_split6"
summary(dt6)

write.csv(dt6, "NEFSC Benthic Database (OBIS-USA) - isaac-kidd mid-water_recurate191-200_split6_VB.csv") 
clipr::write_clip(dt6)


# dt7 Gear Type: 1-METER SLED NET ####

dt7 <- occ[occ$occurrenceremarks == "Gear Type: 1-METER SLED NET",]

dt7 <- dt7[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt7$StudyID <- "191-200_split7"
summary(dt7)

write.csv(dt7, "NEFSC Benthic Database (OBIS-USA) - 1m sled net 191-200_split7_VB.csv") 
clipr::write_clip(dt7)


# dt8 Gear Type: BOTTOM SKIMMER ####

dt8 <- occ[occ$occurrenceremarks == "Gear Type: BOTTOM SKIMMER",]

dt8 <- dt8[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt8$StudyID <- "191-200_split8"
summary(dt8)

write.csv(dt8, "NEFSC Benthic Database (OBIS-USA) - bottom skimmer 191-200_split8_VB.csv") 
clipr::write_clip(dt8)



# dt9 Gear Type:DIGBY DRAG ####

dt9 <- occ[occ$occurrenceremarks == "Gear Type: DIGBY DRAG",]

dt9 <- dt9[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt9$StudyID <- "191-200_split9"
summary(dt9)
write.csv(dt9, "NEFSC Benthic Database (OBIS-USA) - digby drag 191-200_split9_VB.csv") 
clipr::write_clip(dt9)


# dt10 Gear Type:DIP NET ####

dt10 <- occ[occ$occurrenceremarks == "Gear Type: DIP NET",]

dt10 <- dt10[,c("Abundance",
              "Biomass",
              "Family",
              "Genus",
              "Species",
              "SampleDescription",
              "Plot",
              "Latitude",
              "Longitude",
              "DepthElevation",
              "Day",
              "Month",
              "Year",
              "StudyID",
              "Phylum",
              "Order",
              "Class")]

dt10$StudyID <- "191-200_split10"
summary(dt10)
write.csv(dt10, "NEFSC Benthic Database (OBIS-USA) - dip net 191-200_split10_VB.csv") 
clipr::write_clip(dt10)



# dt11 Gear Type:van veen ####

dt11 <- occ[occ$occurrenceremarks == "Gear Type: VAN VEEN  BCF 0.2M2",]

dt11 <- dt11[,c("Abundance",
                "Biomass",
                "Family",
                "Genus",
                "Species",
                "SampleDescription",
                "Plot",
                "Latitude",
                "Longitude",
                "DepthElevation",
                "Day",
                "Month",
                "Year",
                "StudyID",
                "Phylum",
                "Order",
                "Class")]

dt11$StudyID <- "191-200_split11"
summary(dt11)
write.csv(dt11, "NEFSC Benthic Database (OBIS-USA) - van veen 191-200_split11_VB.csv") 
clipr::write_clip(dt11)


# nahah Gear Type:DIETZ LAFOND SAMPLER ####

dt12 <- occ[occ$occurrenceremarks == "Gear Type: DIETZ LAFOND SAMPLER",]

dt12 <- dt12[,c("Abundance",
                "Biomass",
                "Family",
                "Genus",
                "Species",
                "SampleDescription",
                "Plot",
                "Latitude",
                "Longitude",
                "DepthElevation",
                "Day",
                "Month",
                "Year",
                "StudyID",
                "Phylum",
                "Order",
                "Class")]

dt12$StudyID <- "191-200_split12"
summary(dt12)
#too little taxonomic resolution 


# dt12 Gear Type:QUAHOG DREDGE ####

dt12 <- occ[occ$occurrenceremarks == "Gear Type: QUAHOG DREDGE",]

dt12 <- dt12[,c("Abundance",
                "Biomass",
                "Family",
                "Genus",
                "Species",
                "SampleDescription",
                "Plot",
                "Latitude",
                "Longitude",
                "DepthElevation",
                "Day",
                "Month",
                "Year",
                "StudyID",
                "Phylum",
                "Order",
                "Class")]

dt12$StudyID <- "191-200_split12"
summary(dt12)
write.csv(dt12, "NEFSC Benthic Database (OBIS-USA) - quahog dredge 191-200_split12_VB.csv") 
clipr::write_clip(dt12)


#dt 1 ####

summary(dt1)
(nrow(dt1) - 11332)/nrow(dt1) # 44% at species
(nrow(dt1) - 5361)/nrow(dt1) #74% genus level 

clipr::write_clip(dt1)


# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt1 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt1, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

# dt 2 ####
summary(dt2)
(nrow(dt2) - 24606)/nrow(dt2) # 82% at species
(nrow(dt2) - 15185)/nrow(dt2) #89% genus level 

clipr::write_clip(dt2)


# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt2 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

#dt 3 ####
summary(dt3)
(nrow(dt3) - 2984)/nrow(dt3) # 49% at species
(nrow(dt3) - 1391)/nrow(dt3) #76% genus level 

clipr::write_clip(dt3)

# 1. Convert data points into point spatial object
dt_coord <- dt3 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()


# dt 4 ####
summary(dt4)
(nrow(dt4) - 8452)/nrow(dt4) # 46% at species
(nrow(dt4) - 5024)/nrow(dt4) #68% genus level 

clipr::write_clip(dt4)

# 1. Convert data points into point spatial object
dt_coord <- dt4 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

# dt 5 - discard ####
summary(dt5) #keep families?
(nrow(dt5) - 2777)/nrow(dt5) # 22% at species
(nrow(dt5) - 2244)/nrow(dt5) # 37% genus level 
(nrow(dt5) - 1393)/nrow(dt5) # 61% order level 
(nrow(dt5) - 794)/nrow(dt5) # 78% class level 

# dt 6 - discard ####
summary(dt6) # too little resolution
(nrow(dt6) - 431)/nrow(dt6) # 26% at species
(nrow(dt6) - 362)/nrow(dt6) #38% genus level 

# dt 7 ####
summary(dt7)
(nrow(dt7) - 1670)/nrow(dt7) # 55% at species
(nrow(dt7) - 1184)/nrow(dt7) #68% genus level 

clipr::write_clip(dt7)

# 1. Convert data points into point spatial object
dt_coord <- dt7 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)
gplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()


# dt 8 - discard ####
summary(dt8)
(nrow(dt8) - 873)/nrow(dt8) # 35% at species
(nrow(dt8) - 577)/nrow(dt8) #57% genus level 

# dt 9 ####
summary(dt9)
(nrow(dt9) - 1584)/nrow(dt9) # 65% at species
(nrow(dt9) - 960)/nrow(dt9) #79% genus level 

clipr::write_clip(dt9)

# 1. Convert data points into point spatial object
dt_coord <- dt9 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)
gplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

#dt 11 - discard ####
summary(dt11)
(nrow(dt11) - 595)/nrow(dt11) # 34% at species
(nrow(dt11) -452)/nrow(dt11) # 50% genus level 

# dt 10 ####
summary(dt10)
(nrow(dt10) - 28)/nrow(dt10) # 94% at species
(nrow(dt10) - 15)/nrow(dt10) # 97% genus level 

clipr::write_clip(dt10)

# 1. Convert data points into point spatial object
dt_coord <- dt10 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)
gplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

# dt 12 ####
summary(dt12)
(nrow(dt12) - 242)/nrow(dt12) # 41% at species
(nrow(dt12) - 93)/nrow(dt12) # 77% genus level 
clipr::write_clip(dt12)

# 1. Convert data points into point spatial object
dt_coord <- dt12 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid[c(2,1)]
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area
write_clip(area)
gplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt2, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()





