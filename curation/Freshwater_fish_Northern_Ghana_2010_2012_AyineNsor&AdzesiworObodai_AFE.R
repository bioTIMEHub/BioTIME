################################################################################
#Curation script: Fish population from wetlands in Northern Region of Ghana
#Publication: Environmental Determinants Influencing Fish Community Structure and Diversity in Two Distinct Seasons among Wetlands of Northern Region (Ghana)
#Curator: AFE
#Date: 12/2/2021 (updates Oct 2023)
################################################################################

# Main source ==================================================================
# https://doi.org/10.1155/2016/1598701

# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(clipr)
library(readxl)
library(measurements)
library(rworldmap)

rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()

# read data ====================================================================
files_dir <-  "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Freshwater_fish_Northern_Ghana_2010_2012_AyineNsor&AdzesiworObodai_AFE"
 
Dry10 <- read_excel(paste0(files_dir, "/Dry season fish data 2010.xlsx"), sheet = "Sheet1")
Dry12 <- read_excel(paste0(files_dir, "/Dry season fish data 2012.xlsx"), sheet = "Sheet1")

WetS10 <- read_excel(paste0(files_dir, "/Wet season fish data  2010.xlsx"), sheet = "Sheet1")
WetS12 <- read_excel(paste0(files_dir, "/Wet season fish data 2012.xlsx"), sheet = "Sheet1")

# format =======================================================================

# format Dry season datasets: --------------------------------------------------
Dry10 <- Dry10[-1,]
Dry12 <- Dry12[-1,] # rm double heading column

lDry10 <- list("Wuntori"=Dry10[,c(1:2)], "Kukobila"=Dry10[,c(4:5)], "Tugu"=Dry10[,c(7:8)],
               "Nabogu"=Dry10[,c(10:11)], "Kudanali"=Dry10[,c(13:14)], "Bunlung"=Dry10[,c(16:17)])
lDry10 <- lapply(names(lDry10), function(x) {
  lDry10[[x]] %>% 
    mutate(Wetland = x)
})
nms <- c("Species", "Abundance", "Wetland")
lDry10 <- lapply(lDry10, setNames, nms)
Dry10 <- do.call(rbind, lDry10)
Dry10$Season <- "Dry"
Dry10$Year <- 2010

lDry12 <- list("Wuntori"=Dry12[,c(1:2)], "Kukobila"=Dry12[,c(4:5)], "Tugu"=Dry12[,c(7:8)],
               "Nabogu"=Dry12[,c(10:11)], "Kudanali"=Dry12[,c(13:14)], "Bunlung"=Dry12[,c(16:17)])
lDry12 <- lapply(names(lDry12), function(x) {
  lDry12[[x]] %>% 
    mutate(Wetland = x)
})
lDry12 <- lapply(lDry12, setNames, nms)
Dry12 <- do.call(rbind, lDry12)
Dry12$Season <- "Dry"
Dry12$Year <- 2012

dtDry <- rbind(Dry10, Dry12)

dtDry$Wetland[dtDry$Wetland=="Kudanali"] <- "Adayili" # two names for the same wetland
unique(dtDry$Wetland)

# format wet season datasets:---------------------------------------------------

head(WetS10, 2L)
WetS10 <- gather(WetS10, key="Wetland", value="Abundance", -1)
WetS10$Year <- 2010
WetS10$Season <- "Wet"

WetS12 <- gather(WetS12, key="Wetland", value="Abundance", -1)
WetS12$Year <- 2012
WetS12$Season <- "Wet"

dtWetS <-rbind(WetS10, WetS12)

# correct mispellings: ---------------------------------------------------------

unique(dtWetS$Wetland)
unique(dtDry$Wetland)

dtDry$Wetland <- plyr::revalue(dtDry$Wetland, c("Nabogu"="Nabogo",
                                             "Bunlung"="Bunglung"))

identical(unique(dtDry$Wetland), unique(dtWetS$Wetland)) # TRUE

dt <- rbind(dtDry, dtWetS)                               # 660 observations


# check abundances =============================================================
unique(dt$Abundance) # some NAs and 0s

dt$Abundance <- as.numeric(dt$Abundance)
sum(is.na(dt$Abundance))           # 77
sum(dt$Abundance==0, na.rm=T)      # 238

dt <- dt[!is.na(dt$Abundance),]
dt <- dt[!dt$Abundance==0,]
range(dt$Abundance)                # ok, 1 to 85, 345 observations


# check day, month & year ======================================================

unique(dt$Year) #OK

# NOTE: represent dry and wet season samplings 
# November to February: dry season
# July to October: wet season
# Given that the abundance counts are a sum of all the counts in the sampling events of each
# season, I add months 2 & 10 to represent the seasonality of the data

dt$Month <- ifelse(dt$Season=="Dry", 2, 10)


# coordinates & check location =================================================
# system is WGS 84.
# from provided Excel data sheet & associated publication source:
dt$Latitude <- rep(NA, nrow(dt))
dt$Longitude <- rep(NA, nrow(dt))

dt$Latitude <- ifelse(dt$Wetland=="Wuntori", "09 08.335", dt$Latitude)
dt$Longitude <- ifelse(dt$Wetland=="Wuntori", "-0 109.685", dt$Longitude)
dt$Latitude <- ifelse(dt$Wetland=="Kukobila", "10 08.723", dt$Latitude)
dt$Longitude<- ifelse(dt$Wetland=="Kukobila", "-0 48.179", dt$Longitude)
dt$Latitude <- ifelse(dt$Wetland=="Tugu", "09 22.550", dt$Latitude)
dt$Longitude <- ifelse(dt$Wetland=="Tugu", "-0 35.004", dt$Longitude)
dt$Latitude <- ifelse(dt$Wetland=="Nabogo", "09 49.941", dt$Latitude)
dt$Longitude <- ifelse(dt$Wetland=="Nabogo", "-0 51.942", dt$Longitude)
dt$Latitude <- ifelse(dt$Wetland=="Adayili", "09 41.391", dt$Latitude)
dt$Longitude <- ifelse(dt$Wetland=="Adayili", "-0 41.480", dt$Longitude)
dt$Latitude <- ifelse(dt$Wetland=="Bunglung", "09 35.576", dt$Latitude)
dt$Longitude <- ifelse(dt$Wetland=="Bunglung", "-0 47.443", dt$Longitude)

unique(dt$Latitude)
unique(dt$Longitude)
dt$Latitude <- measurements::conv_unit(dt$Latitude, from = 'deg_dec_min', to = 'dec_deg')   # conveting coordinate units
dt$Longitude <- measurements::conv_unit(dt$Longitude, from = 'deg_dec_min', to = 'dec_deg') # conveting coordinate units

dt$Latitude <- as.numeric(dt$Latitude)
dt$Longitude <- as.numeric(dt$Longitude)

unique(dt$Latitude)  # 6 vals, OK
unique(dt$Longitude) # 6 vals, OK


# check location ---------------------------------------------------------------
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points

#Zoom to country:
world <- getMap(resolution = "low")
map_Ghana <- world[world@data$ADMIN %in% "Ghana", ]
(plot <- ggplot() +
    geom_polygon(data = map_Ghana, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = dt,
               aes(x = Longitude, y = Latitude))) # ok


# check taxonomy ===============================================================
unique(dt$Species)

dt$Genus <- str_split_fixed(dt$Species, " ", 2)[, 1]
dt$Species <- str_split_fixed(dt$Species, " ", 2)[, 2]

unique(dt$Genus)
unique(dt$Species)

dt$Species <- gsub('\\s+', '',dt$Species) # rm spaces

# Genus:------------------------------------------------------------------------
sort(unique(dt$Genus))
# Chromodotilapia is a misspelling of Chromidotilapia
# Tilapi of Tilapia

dt$Genus <- plyr::revalue(dt$Genus, c("Tilapi"="Tilapia",
                                      "Chromodotilapia"="Chromidotilapia"))
sort(unique(dt$Genus)) # OK

# Species: ---------------------------------------------------------------------
sort(unique(dt$Species))
# bodorsalis is a misspelling (correct name is Heterobranchus bidorsalis)

# Hemisynodontis membranaceous is a synonym for Synodontis membranaceus (FishBase accessed 2023)
# Polypterus senegalus senegalus is a synonym for Polypterus senegalus  (FishBase accessed 2023)
dt[dt$Genus=="Hemisynodontis",]
dt$Genus <- plyr::revalue(dt$Genus, c("Hemisynodontis"="Synodontis"))
dt$Species <- plyr::revalue(dt$Species, c("bodorsalis"="bidorsalis",
                                          "membranaceous"="membranaceus",
                                          "senegalussenegalus"="senegalus"))
sort(unique(dt$Species))

# aggregate ====================================================================
sum(duplicated(dt[,-2])==T) 
dt[duplicated(dt[,-2])==T,] # 1, not a true dup, it stems from the change in name of Hemisynodontis to Synodontis
dt <- as.data.frame(dt)
rawdata <- dt %>% group_by(Genus, Species, Wetland, Latitude, Longitude, Month, Year) %>%
  summarise(Abundance=sum(Abundance)) # 344

rawdata$Family <- rep(NA, nrow(rawdata))
rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass, Family), .before = Genus)

rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata$SampleDescription <- paste0(rawdata$Wetland, "_", rawdata$Month, "_", rawdata$Year)
rawdata <- rawdata %>% relocate(c(Plot, SampleDescription), .before = Latitude)

rawdata$DepthElevation <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(DepthElevation), .after = Longitude)

rawdata$Day <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Day), .after = DepthElevation)

rawdata$StudyID <- rep(NA, nrow(rawdata))

names(rawdata)
rawdata <- within(rawdata, rm(Wetland))
rawdata <- as.data.frame(rawdata)
str(rawdata)

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Freshwater_fish_Northern_Ghana_2010_2012_AyineNsor&AdzesiworObodai_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area

## also useful snippet if coordinates are ever in degree minutes seconds

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim = c(-5,5), ylim = c(7,10)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()  # ok


# End of script ################################################################