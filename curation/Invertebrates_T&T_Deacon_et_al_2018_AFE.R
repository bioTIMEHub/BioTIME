################################################################################
# Study: Invertebrate survey in the Northern Range, Trinidad (BioTIME dataset)
# Curator: AFE
# Date: August 2023
################################################################################

# Main sources =================================================================
# https://www.pnas.org/doi/10.1073/pnas.1712594115

# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(data.table)


rm(list=ls())
getwd()


# Read raw data files ==========================================================
files_dir <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Invertebrates_T&T_Deacon_et_al_2018_AFE"

load(paste0(files_dir, "/BenthicInv1.RData"))                # inv dataset
dt <- as.data.frame(BenthicInv1)
load(paste0(files_dir, "/keysamplingdates.RData"))           # timestep to date key
temp <- as.data.frame(keysamplingdates)
coords <- read.csv(paste0(files_dir, "/coords_TT.csv"), h=T) # coordinates


# Abundances ===================================================================
range(dt$abundance) # 1 1616
str(dt)
str(temp)


# Temporal =====================================================================
# Two change in dates needed:
# Acono sites (two repeatabilities, one the 12th the other the 15th of August 2011, data corresponds to the 12th)
# Maracas sites (two repeatabilities, one the 1st the other the 3rd of June 2011, data corresponds to the 3rd)
# Changed below

temp$Day[temp$Session==3 & temp$Site %like% "Acono"]
temp$Day[temp$Session==2 & temp$Site %like% "Maracas"]

temp$Day[temp$Day=="15" & temp$Session==3 & temp$Site %like% "Acono"] <- "12" # Remove date of repeated sample for Acono sites (two repeatabilities, one the 12th the other the 15th, data corresponds to the 12th)
temp$Day[temp$Day=="01" & temp$Session==2 & temp$Site %like% "Maracas"] <- "03"  # Remove date of repeated sample for Maracas sites (two repeatabilities, one the 1st the other the 3rd, data corresponds to the 3rd)

identical(sort(unique(dt$Site)), sort(unique(temp$Site))) # TRUE

temp$Code <- paste0(temp$Site, "_", temp$Session)
dt$Code <- paste0(dt$Site, "_", dt$timestep)

dt$Year <- as.integer(temp$Year[match(dt$Code, temp$Code)])
dt$Month <- as.integer(temp$Month[match(dt$Code, temp$Code)])
dt$Day <- as.integer(temp$Day[match(dt$Code, temp$Code)])

sort(unique(dt$Year))
sort(unique(dt$Month))
sort(unique(dt$Day))

sum(is.na(dt$Year))
sum(is.na(dt$Month))
sum(is.na(dt$Day))


# Coordinates & locations ======================================================
dt$Site <- paste0(substr(dt$Site, 1, nchar(dt$Site) - 1), toupper(substr(dt$Site, nchar(dt$Site), nchar(dt$Site))))
dt$Site <- gsub(" ", "", dt$Site)
coords$Site <- paste0(substr(coords$Site, 1, nchar(coords$Site) - 1), toupper(substr(coords$Site, nchar(coords$Site), nchar(coords$Site))))
coords$Site <- gsub("_", "", coords$Site)

dt$Latitude <- coords$Latitude[match(dt$Site, coords$Site)]
dt$Longitude <- coords$Longitude[match(dt$Site, coords$Site)]

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points       # OK

#Zoom:
points_zoom <- points +
  ylim(5, 15)+
  xlim(-65, -55)
points_zoom # OK


# Taxonomy =====================================================================
sort(unique(dt$Classification))  # Lowest resolution is the last name

dt$Class1 <- str_split_fixed(dt$Classification, " ", 3)[,1]
dt$Class2 <- str_split_fixed(dt$Classification, " ", 3)[,2]
dt$Class3 <- str_split_fixed(dt$Classification, " ", 3)[,3]

sort(unique(dt$Class3))
unique(dt$Class2[dt$Class3==""])
unique(dt$Class1[dt$Class3==""])

# when Class3 is ""
# collembola: subclass
# crustacea: subphylum
# oligochaeta: subclass
# turbellaria: class

# when Class3 is "hydracarina"
unique(dt$Class2[dt$Class3=="hydracarina"]) # "chelicerata"
unique(dt$Class1[dt$Class3=="hydracarina"]) # "arachnoidea"

# when Class3 is "unknown"
unique(dt$Class2[dt$Class3=="unknown"]) # "lepidoptera"
unique(dt$Class1[dt$Class3=="unknown"]) # "insecta"

# NOTE: these above family classifications are kept since they represent 
# groups separate from the other family level ones (i.e., no overlap)

dt$Family <- dt$Class3

dt$Family[dt$Family=="" & dt$Class2=="collembola"] <- "collembola"
dt$Family[dt$Family=="" & dt$Class2=="spp"] <- "crustacea"
dt$Family[dt$Family=="" & dt$Class1=="turbellaria"] <- "turbellaria"
dt$Family[dt$Family=="" & dt$Class1=="oligochaeta"] <- "oligochaeta"

dt$Family[dt$Family=="unknown"] <- "lepidoptera"

sort(unique(dt$Family))
sum(is.na(dt$Family))

dt$Family <- paste0(toupper(substring(dt$Family, 1, 1)), substring(dt$Family, 2))
sort(unique(dt$Family))


# SampleDesc & data aggregation ================================================
rawdata <- dt %>% group_by(Family, Site, Latitude, Longitude, Day, Month, Year) %>%
  summarise(Abundance=sum(abundance)) # 5993 obs

rawdata$Biomass <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Abundance, Biomass), .before=Family)

rawdata$Genus <- rep(NA, nrow(rawdata))
rawdata$Species <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(Genus, Species), .after=Family)

rawdata$SampleDescription <- paste0(rawdata$Site, "_",
                                    rawdata$Day, "_", rawdata$Month, "_", rawdata$Year)
rawdata$Plot <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(c(SampleDescription, Plot), .before = Latitude)

rawdata$DepthElevation <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(DepthElevation, .after = Longitude)

rawdata$StudyID <- rep(NA, nrow(rawdata))
rawdata <- rawdata %>% relocate(StudyID, .after = Year)

rc1 <- rawdata %>% group_by(Site, Year) %>% summarise(n=n_distinct(Month)) # OK, correct.
range(rawdata$Abundance) # 1 1616
rawdata <- within(rawdata, rm(Site))

rawdata <- as.data.frame(rawdata)
str(rawdata)

path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Invertebrates_T&T_Deacon_et_al_2018_AFE"
write.csv(rawdata, file=paste0(path, "/rawdata.csv"), row.names = F)


# Convex hulls =================================================================
# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_merged <- rawdata
dt_merged$Latitude <- as.numeric(dt_merged$Latitude)
dt_merged$Longitude <- as.numeric(dt_merged$Longitude)
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
  coord_sf(xlim = c(-62,-60), ylim = c(10,12)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal()

# End of script ################################################################
################################################################################