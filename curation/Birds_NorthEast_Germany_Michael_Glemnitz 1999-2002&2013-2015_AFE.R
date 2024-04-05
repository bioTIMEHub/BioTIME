################################################################################
# Study: Farmland bird monitoring in North-east Germany 1999-2002 & 2013-2015
# Curator: AFE
# Date: 2020 & revised December 2023
################################################################################

# Main source: =================================================================
# https://dx.doi.org/10.4228/ZALF.DK.85
# https://www.doi.org/10.4228/ZALF.DK.86


# Libraries: ===================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)


rm(list=ls())

setwd("C:/Users/afe1/OneDrive - University of St Andrews/PHD/BioTIMEGithub")
myd <- getwd()


# Read Data: ===================================================================
raw_data_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Birds_NorthEast_Germany_Michael_Glemnitz 1999-2002&2013-2015_AFE"
dt1 <- read.csv(file = paste0(raw_data_path,"/bird_survey_1999_2002 - Copy.csv"), h=T) # load copy where "sky" is added as header of the last column
dt2 <- read.csv(file = paste0(raw_data_path,"/bird_survey_2013_2015.csv"), h=T)
coordsdt <- read_excel(paste0(raw_data_path, "/qu_vogPT200_TableToExcel1.xlsx"), sheet="qu_vogPT200_TableToExcel")


# Check structure ==============================================================

str(dt1)
str(dt2)

dim(dt1) # 20007 
dim(dt2) # 9227

dt1$point_no <- as.character(dt1$point_no)
dt2$point_no <- as.character(dt2$point_no)

length(unique(dt1$point_no))
length(unique(dt2$point_no))

dt1$point_no <- ifelse(nchar(dt1$point_no) < 4, sprintf("%s0", dt1$point_no), dt1$point_no) # add trailing 0s that were removed at reading data
unique(dt1$point_no)
dt2$point_no <- ifelse(nchar(dt2$point_no) < 4, sprintf("%s0", dt2$point_no), dt2$point_no) # add trailing 0s that were removed at reading data
unique(dt2$point_no)

setdiff(as.character(sort(unique(dt1$point_no))), as.character(sort(unique(dt2$point_no))))
#Sampled only between 1999 & 2002: "1.11" "1.12" "1.16" "1.21" "1.23" "3.01" "3.02" "3.03" "3.04" "3.05"
setdiff(as.character(sort(unique(dt2$point_no))), as.character(sort(unique(dt1$point_no))))
#Sampled only between 2013 & 2015: "1.15"  "3.07"  "3.08" 

sum(is.na(dt2$point_no))          # 4 unclassified observations
dt2 <- dt2[!is.na(dt2$point_no),] # rm

range(dt1$no_of_individuals)
range(dt2$no_of_individuals)


# Point observation ============================================================

setdiff(unique(dt1$point_no), unique(dt2$point_no))
setdiff(unique(dt2$point_no), unique(dt1$point_no))

length(unique(c(dt1$point_no, dt2$point_no))) # 124

c1 <- dt1 %>% group_by(point_no) %>% summarise(n=n_distinct(obs_year))
# 1.23 data for 2 different years, the rest for 4 years (within 1999-2002 period)
c2 <- dt2 %>% group_by(point_no) %>% summarise(n=n_distinct(obs_year))
# 3.06 data for only 1 year in 2013-2015, but also in period 1999-2002
# 1.15 data for only 1 year in 2013-2015, rm
dt2 <- dt2[!dt2$point_no=="1.15",] 


# Temporal sampling events =====================================================
str(dt1)
str(dt2)


#dt1:---------------------------------------------------------------------------
length(unique(dt1$obs_date)) # 19 levels

dt1$Month <- as.integer(str_split_fixed(as.character(dt1$obs_date), "/", 3)[, 2])
dt1$Day <- as.integer(str_split_fixed(as.character(dt1$obs_date), "/", 3)[, 1])

unique(dt1$obs_year)
unique(dt1$Month)
unique(dt1$Day)

#Sampling events/year:
unique(dt1$obs_date[dt1$obs_year==1999])#4
unique(dt1$obs_date[dt1$obs_year==2000])#5
unique(dt1$obs_date[dt1$obs_year==2001])#5
unique(dt1$obs_date[dt1$obs_year==2002])#5

c3 <- dt1 %>% group_by(point_no, obs_year, Month) %>% summarise(nDay=n_distinct(Day))
# NOTES: there can be, for the same observation point, one or two sampling events per month per year

dt1$obs_h_start <- strptime(dt1$obs_h_start, format = "%H:%M:%S")
dt1$obs_h_end <- strptime(dt1$obs_h_end, format = "%H:%M:%S")

c4 <- dt1 %>% group_by(point_no, obs_year, Month, Day) %>% summarise(HoursSampled = obs_h_end - obs_h_start)
c4$HoursSampled <- as.numeric(c4$HoursSampled, units = "hours")
range(c4$HoursSampled) 


#dt2:---------------------------------------------------------------------------
length(unique(dt2$obs_date)) # 93 levels

dt2$obs_year <- as.integer(dt2$obs_year)
dt2$Month <- as.integer(str_split_fixed(as.character(dt2$obs_date), "-", 3)[, 2])
dt2$Day <- as.integer(str_split_fixed(as.character(dt2$obs_date), "-", 3)[, 3])

unique(dt2$obs_year)
unique(dt2$Month)
unique(dt2$Day)


c5 <- dt2 %>% group_by(point_no, obs_year, Month) %>% summarise(nDay=n_distinct(Day))
# NOTE: as above, there can be, for the same observation point, 
# one or two sampling events per month per year
# the difference is, in 1999-2002, it is always the first or the second of the month
# in 2013-2015, the dates are more variable.

dt2$obs_h_start <- strptime(dt2$obs_h_start, format = "%H:%M:%S")
dt2$obs_h_end <- strptime(dt2$obs_h_end, format = "%H:%M:%S")

c6 <- dt2 %>% group_by(point_no, obs_year, Month, Day) %>% summarise(HoursSampled = obs_h_end - obs_h_start)
c6$HoursSampled <- as.numeric(c6$HoursSampled, units = "hours")
range(c6$HoursSampled) 

dt2 <- within(dt2, rm(obs_number))

# NOTE: confirmed methods were equivalent in both periods.


# Merge ========================================================================
dt1 <- dt1 %>% select(., obs_year, point_no, name_latin, no_of_individuals,
                      C_dist, distance, Month, Day)
unique(dt2$remark)
dt2 <- dt2 %>% select(., obs_year, point_no, name_latin, no_of_individuals,
                      C_dist, distance, Month, Day)
dt <- rbind(dt1, dt2)       # 29179 observations
range(dt$no_of_individuals) # 1 to 500
unique(dt$point_no)

# Taxonomy =====================================================================

sort(unique(as.character(dt$name_latin)))
"Columba livia domestica" # subspecies of Columba livia (Common pigeon)
"Corvus corone cornix"    # subspecies of Corvus corone

dt$name_latin <- plyr::revalue(dt$name_latin, c("Columba livia domestica"="Columba livia",
                                                "Corvus corone cornix"="Corvus corone"))

dt$Species <- str_split_fixed(as.character(dt$name_latin), " ", 2)[, 2]
dt$Genus <- str_split_fixed(as.character(dt$name_latin), " ", 2)[, 1]

sort(unique(dt$Species))
sort(unique(dt$Genus))


# Coordinates  =================================================================
setdiff(sort(unique(dt$point_no)), sort(unique(coordsdt$PUNKT_NR))) # 0
setdiff(sort(unique(coordsdt$PUNKT_NR)), sort(unique(dt$point_no))) # "1.15" "1.22"
str(coordsdt)

sf_data <- st_as_sf(coordsdt, coords = c("X", "Y"), crs = 31469)
sf_data_wgs84 <- st_transform(sf_data, 4326)
transformed_coords <- st_coordinates(sf_data_wgs84)
colnames(transformed_coords) <- c("Longitude", "Latitude")
coordsdt2 <- as.data.frame(cbind(coordsdt, transformed_coords))

length(unique(coordsdt2$PUNKT_NR))  # 125
length(unique(coordsdt2$Longitude)) # 125
length(unique(coordsdt2$Latitude))  # 125


dt$Longitude <- coordsdt2$Longitude[match(dt$point_no, coordsdt2$PUNKT_NR)]
sum(is.na(dt$Longitude))
dt$Latitude <- coordsdt2$Latitude[match(dt$point_no, coordsdt2$PUNKT_NR)]
sum(is.na(dt$Latitude))

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points


points_zoom <- points +
  ylim(40,60)+
  xlim(-20,20)
points_zoom    


# Pool abundances ==============================================================
unique(dt$C_dist)
unique(dt$distance)
sum(dt$C_dist==1) #10414 observations within 100 m distance
sum(dt$C_dist==2) #18765 observations beyond 100 m distance

newdt <- dt %>% group_by(Genus, Species, point_no, 
                         Latitude, Longitude, Day, 
                         Month, obs_year) %>%
  summarise(Abundance=sum(no_of_individuals))
range(newdt$Abundance)

newdt$Biomass <- rep(NA, nrow(newdt))
newdt$Family <- rep(NA, nrow(newdt))
newdt$Plot <- rep(NA, nrow(newdt))
newdt$SampleDescription <- paste0(newdt$point_no, "_", 
                                  newdt$Day, "_",
                                  newdt$Month, "_",
                                  newdt$obs_year)
newdt$DepthElevation <- rep(NA, nrow(newdt))
newdt$StudyID <- rep(NA, nrow(newdt))

newdt <- newdt %>% relocate(c(Family), .before = Genus)
newdt <- newdt %>% relocate(c(Abundance, Biomass), .before = Family)

newdt <- newdt %>% relocate(c(Plot, SampleDescription), .before = Latitude)
newdt <- newdt %>% relocate(c(DepthElevation), .after = Longitude)
newdt <- within(newdt, rm(point_no))
names(newdt)[names(newdt)=="obs_year"] <- "Year"


unique(newdt$SampleDescription)
sum(newdt$Abundance) # 66805 individuals
newdt <- as.data.frame(newdt)
str(newdt)


# Write csv ====================================================================
rawData <- newdt
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Birds_NorthEast_Germany_Michael_Glemnitz 1999-2002&2013-2015_AFE"
write.csv(rawData, file=paste0(path, "/rawData.csv"), row.names = F)   


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawData
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
centroid

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
area 

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
  coord_sf(xlim = c(13,15), ylim = c(53,54)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal() # OK, longitudinal transect


# End of script ################################################################


