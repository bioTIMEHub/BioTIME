## Check 98 and 280 ####
## curator: VB
## revision1 stage

library(dplyr)
library(ggplot2)
library(clipr)
require(stringr)
require(sf)
require("XML")
require("methods")

#### read database

BT <- readRDS("queryBTv2_April_2024.rds")
meta <- read.csv("metadataBTv2_April_2024.csv")

names(BT)
names(meta)

st_id <- 98

mt98 <- meta[meta$STUDY_ID == st_id,]
raw98 <- BT[BT$STUDY_ID == st_id,]
mt98$SAMPLE_DESC_NAME
mt98$TITLE
sort(unique(raw98$SAMPLE_DESC))
mt98$COMMENTS
mt98$CURATOR
mt98$METHODS

st_id <- 280

mt280 <- meta[meta$STUDY_ID == st_id,]
raw280 <- BT[BT$STUDY_ID == st_id,]
mt280$SAMPLE_DESC_NAME
mt280$TITLE
sort(unique(raw280$SAMPLE_DESC))
mt280$COMMENTS
mt280$CURATOR
mt280$METHODS

raw98[raw98$SAMPLE_DESC == "69.325_-133.74_1975_7_20_Petterson-grab_2",]
raw280[raw280$SAMPLE_DESC == "69.325_-133.74_1975_2",]

raw98[raw98$SAMPLE_DESC == "69.325_-133.74_1975_7_20_Petterson-grab_2",]
raw280[raw280$SAMPLE_DESC == "69.325_-133.74_1975_2",]


raw98%>% mutate_if(is.character,as.factor) %>% summary()
1762/nrow(raw98) # 83% species level
(1762+199)/nrow(raw98) # 92% genus level

raw280 %>% mutate_if(is.character,as.factor) %>% summary()
2/nrow(raw280) # 4% species level
(2+1)/nrow(raw280) # 6% genus level

#### just do from scratch...
setwd("~/Library/CloudStorage/Dropbox/My Dropbox/BioTIME/study98-280")

occ <- read.delim("~/Library/CloudStorage/Dropbox/My Dropbox/BioTIME/study98-280/occurrence.txt")
names(occ) 
occ %>% View()
occ <- occ %>% select(occurrenceRemarks, dynamicProperties, year, month, day, 
                      maximumDepthInMeters,
                      scientificName, scientificNameID, decimalLongitude, decimalLatitude)

occ$method <- as.factor(word(word(occ$occurrenceRemarks, 2, sep = ";"), 2, sep = "="))
summary(occ$method)

occ$method[occ$method == "Petterson grab"] <- "Petterson Grab"

occ$sample_size <- NA
occ$sample_size[occ$method != "dredge"] <- as.numeric(word(word(occ$dynamicProperties[occ$method != "dredge"], 2, sep = ";"), 2, sep="="))
table(occ$sample_size)

table(occ$sample_size, occ$method)

occ$weigth <- as.numeric(str_sub(occ$dynamicProperties, 
                      start = str_locate(occ$dynamicProperties,"observedweight")[,2]+2, 
                      end = str_locate(occ$dynamicProperties,";")[,2]-1))
occ$count <- as.numeric(str_sub(occ$dynamicProperties, 
                                 start = str_locate(occ$dynamicProperties,"observedindividualcount")[,2]+2, 
                                 end = str_locate(occ$dynamicProperties,";$")[,2]-1))
summary(occ$weigth)
table(occ$weigth)

summary(occ$count)
table(occ$count)
occ[occ$count == 3.5,]

plot(occ$count~occ$year, col = as.factor(occ$method))
plot(occ$weigth~occ$year, col = as.factor(occ$method))

table(occ$method, occ$year)

#only 2 methods across year

table(occ$sample_size, occ$year)


table(occ$sample_size, occ$year, occ$method)



occ_summary <- occ %>% group_by(method) %>% mutate(n_events = n_distinct(paste(decimalLatitude, decimalLongitude,
                                                                year, month, day, maximumDepthInMeters)),
                                    n_events_monthOnly = n_distinct(paste(decimalLatitude, decimalLongitude,
                                                                          year, month, maximumDepthInMeters)),
                                    n_species = n(),
                                    n_event_loc = n_distinct(paste(decimalLatitude, decimalLongitude)),
                                    n_event_depths = n_distinct(maximumDepthInMeters),
                                    n_event_dates = n_distinct(paste(year, month,day)), 
                                    n_ss = n_distinct(sample_size), 
                                    min_ss = min(sample_size), 
                                    max_ss = max(sample_size), 
                                    year_start = min(year),
                                    year_finish = max(year)) %>% 
  select(method,n_events, n_event_loc, n_ss, min_ss, max_ss,
         n_event_depths, n_event_dates,n_events_monthOnly, year_start, year_finish) %>% unique() %>% View() # %>% write.csv("study191and200raw.csv")



# weight has been normalized to pe representative of a square meter of area, 
# so difference in sample size doesn't matter and things should be comparable 
# through years. But checking nonetheless.

occ <- occ %>% filter(method == "Petterson Grab"| method == "Wildco Petersen grab")
occ <- occ %>% mutate(event = paste(paste(decimalLatitude, decimalLongitude,
                                             year, month, day, maximumDepthInMeters, sep = "_")))
occ %>% group_by(event) %>% mutate (n_species = n()) %>% 
  select(event, year, n_species, method) %>% unique() %>%
  ggplot(aes(year, n_species, col = method)) +
  geom_point()+
  theme_bw()

occ %>% group_by(event) %>% mutate (n_species = n()) %>% 
  select(event, year, n_species, method, sample_size) %>% unique() %>%
  ggplot(aes(year, n_species, col = sample_size)) +
  facet_wrap(~method)+
  geom_boxplot(aes(group = year))+
  geom_jitter()+
  theme_bw()

occ %>% group_by(event) %>% mutate (n_species = n()) %>% 
  select(event, year, n_species, method, sample_size) %>% unique() %>%
  ggplot(aes(year, n_species, col = sample_size)) +
  geom_jitter()+
  theme_bw()

# keeping only one method

occ <- occ %>% filter(method == "Wildco Petersen grab")
unique(occ$scientificNameID)

occ <- occ %>% 
  select(count, weigth, scientificName, decimalLatitude, decimalLongitude, 
         maximumDepthInMeters, day, month, year, event, scientificNameID) %>%
  rename(Abundance = count,
         Biomass = weigth,
         GenusSpecies = scientificName,
         SampleDescription = event,
         Latitude = decimalLatitude,
         Longitude = decimalLongitude,
         DepthElevation = maximumDepthInMeters,
         Day = day,
         Month = month,
         Year = year,
         TaxonomyID = scientificNameID)

sort(unique(occ$GenusSpecies))

SpeciesX <- str_split_fixed(occ$GenusSpecies, " ",3)[,3]
SpeciesX[SpeciesX == ""]<- str_split_fixed(occ$GenusSpecies[SpeciesX == ""], " ",3)[,2]
occ$Species <- SpeciesX
occ$Genus <- str_split_fixed(occ$GenusSpecies, " ",3)[,1] 

occ %>% mutate_if(is.character,as.factor) %>% summary()

summary(occ[occ$Abundance==0,])
summary(occ[occ$Biomass==0,])
occ <- occ[occ$Abundance != 0 | occ$Biomass != 0,]
occ$Plot <- NA
occ$StudyID <- 98
occ$Family <- NA
occ$Plot <- NA

dt <- occ[,c("Abundance",
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
              "StudyID","TaxonomyID")]

summary(dt)

write.csv(dt, "Zoobenthos data from the Southern Beaufort Sea, 1971-1975 - Wildco Petersen Grabs 1974-1975 only_VB.csv") 
clipr::write_clip(dt)

dt <- dt[!is.na(dt$Latitude),]
summary(dt) 

# Convert data points into point spatial object
dt_coord <- dt %>% select(Longitude, Latitude) %>% distinct() %>%
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
  geom_point(data = dt, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  #coord_sf(xlim = c(-115, -110), ylim = c(30,35)) + # make sure these fit your points
  labs(x = NULL, y = NULL) +
  theme_minimal()

