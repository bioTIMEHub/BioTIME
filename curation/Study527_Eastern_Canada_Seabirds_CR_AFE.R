################################################################################
# Study: CWS-EC Eastern Canada Seabirds at Sea (ECSAS)
# Curator: Cerren Richards (updates AFE)
# Date: 2020 & 2024
################################################################################


# Main sources =================================================================
# https://obis.org/dataset/51391fb1-ae4d-44d8-9178-a06f95545604


# Libraries ====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)
library(stringi)
library(sf)
library(readxl)
library(sp)


# Read Data ====================================================================
rm(list=ls())
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/Study527_Eastern_Canada_Seabirds_CR_AFE"

dt <- read.delim(paste0(path, "/occurrence.txt"))


# Explore ======================================================================
range(dt$organismQuantity)       # 1 1750
unique(dt$organismQuantityType)  # "Individuals"
unique(dt$samplingProtocol)      # "Standardised surveys"


# Sample Event Date:============================================================
sort(unique(dt$day))
sort(unique(dt$month))
sort(unique(dt$year))


# Locations ====================================================================
range(dt$decimalLatitude)
sum(dt$decimalLatitude==".")   # 52
range(dt$decimalLongitude)
sum(dt$decimalLongitude==".")  # 52

dt <- dt[!dt$decimalLatitude==".",] # rm records with no coords

dt$LatitudeI <- as.numeric(str_split_fixed(dt$decimalLatitude, "\\.", 2)[,1])
range(dt$LatitudeI)
dt$LatitudeII <- str_split_fixed(dt$decimalLatitude, "\\.", 2)[,2]
sum(dt$LatitudeII=="") # 660
dt$LatitudeII[dt$LatitudeII==""] <- "0"
dt$LatitudeII <- as.numeric(dt$LatitudeII)
range(dt$LatitudeII)

dt$Latitude <- as.numeric(paste0(dt$LatitudeI, ".", dt$LatitudeII))


dt$LongitudeI <- str_split_fixed(dt$decimalLongitude, "\\.", 2)[,1]
sum(dt$LongitudeI=="")   # 11
sum(dt$LongitudeI=="-")  # 9
dt$LongitudeI[dt$LongitudeI==""] <- "0"

dt$LongitudeII <- str_split_fixed(dt$decimalLongitude, "\\.", 2)[,2]
range(dt$LongitudeII)
sum(dt$LongitudeII=="")   # 70
dt$LatitudeII[dt$LatitudeII==""] <- "0"

dt$Longitude <- as.numeric(paste0(dt$LongitudeI, ".", dt$LongitudeII))


range(dt$Latitude)
range(dt$Longitude)

world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=dt, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points


check <- dt[abs(dt$Latitude) < 40 & abs(dt$Longitude) < 10,]
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=check, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points
dt <- dt[!(abs(dt$Latitude) < 40 & abs(dt$Longitude) < 10),]   # rm


check2 <- dt[abs(dt$Latitude) < 40 & abs(dt$Longitude) > 80,]
world_map <- map_data("world") 
world <- ggplot() + coord_fixed() +xlab("") + ylab("") 
world  <- world  + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
               colour="gray60", fill="gray60") 
points <- world  + 
  geom_point(data=check2, 
             aes(x=Longitude, y=Latitude, alpha=0.01)) 
points
dt <- dt[!(abs(dt$Latitude) < 40 & abs(dt$Longitude) > 80),]   # rm



# Taxonomy =====================================================================
sort(unique(dt$scientificName))

sum(dt$scientificName=="")                # 5
#View(dt[dt$scientificName=="",])
dt$scientificName[(dt$scientificName=="" & dt$vernacularName=="Black Bear")]   <- "Ursus americanus"
dt$scientificName[dt$scientificName==""]  <- "Otariidae"                      # sea lions
sum(dt$scientificName=="Cetacea")                                             # 667
#View(dt[dt$scientificName=="Cetacea",])

require(taxize)
forclass <- unique(c(dt$scientificName))
forclass <- forclass[!is.na(forclass)]
Class <- classification(unique(forclass), db = 'itis')

Class2 <- Filter(is.data.frame, Class)
Class2 <- lapply(Class2, function(x) {x[x$rank %in% c("family", "class"),]})
Class2 <- do.call(rbind, Class2)
Class2$ScientificName <- str_split_fixed(rownames(Class2), "\\.", 2)[,1]

fams <- subset(Class2, Class2$rank=="family")
class <- subset(Class2, Class2$rank=="class")

dt$Class <- class$name[match(dt$scientificName, class$ScientificName)]
unique(dt$Class)
sum(dt$Class=="Mammalia", na.rm=T)          # 6920
unique(dt$scientificName[is.na(dt$Class)])
dt$Class <- ifelse(dt$scientificName %in% c("Pinnipedia", "Physeter catodon", "Ziphiidae",
                                            "Phocoenidae or Delphinidae"), "Mammalia", dt$Class) # other mammals not picked up by taxize
aves <- unique(dt$scientificName[is.na(dt$Class)])
dt$Class <- ifelse(dt$scientificName %in% aves, "Aves", dt$Class) # other mammals not picked up by taxize

sum(dt$Class=="Mammalia")          # 7053
sum(dt$Class=="Aves")              # 202965

dt <- dt[!dt$Class=="Mammalia",]   # only incidental observations


sort(unique(fams$name))
fams <- fams[!fams$name %in% c("Balaenidae", "Balaenopteridae", "Delphinidae",
                               "Eschrichtiidae", "Hyperoodontidae", "Monodontidae",
                               "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Ursidae"),]
dt$Family <- fams$name[match(dt$scientificName, fams$ScientificName)]
sum(is.na(dt$Family))              # 23871


dt$scientificName[dt$scientificName=="All duck genera"] <- "Anatidae"
dt$scientificName[dt$scientificName=="Branta, Anser, Chen"] <- "Anatidae"
dt$scientificName[dt$scientificName=="Larus glaucoides glaucoides"] <- "Larus glaucoides"
dt$scientificName[dt$scientificName=="Larus glaucoides kumlieni"] <- "Larus glaucoides"
dt$scientificName[dt$scientificName=="Larus hyperboreus or glaucoides"] <- "Larus sp"
dt$scientificName[dt$scientificName=="Larus, Xema, Rissa, Pagophila, Rhodostethia"] <- "Laridae"
dt$scientificName[dt$scientificName=="Puffinus or Calonectris or Ardenna"] <- "Procellariidae"
dt$scientificName[dt$scientificName=="Stercorarius Jaegers"] <- "Stercorarius"
dt$scientificName[dt$scientificName=="Sterna hirundo or paradisaea"] <- "Sterna sp"
dt$scientificName[dt$scientificName=="Uria or Alca"] <- "Alcidae"
dt$scientificName[dt$scientificName=="Mergus or Lophodytes"] <- "Anatidae"
dt$scientificName[dt$scientificName=="Stercorarius Skuas"] <- "Stercorarius skua"
dt$scientificName[dt$scientificName=="Sternidae"] <- "Laridae"


dt$scientificName[dt$scientificName=="Bucephala"] <- "Bucephala sp"
dt$scientificName[dt$scientificName=="Larus"] <- "Larus sp"
dt$scientificName[dt$scientificName=="Melanitta"] <- "Melanitta sp"
dt$scientificName[dt$scientificName=="Phalacrocorax"] <- "Phalacrocorax sp"
dt$scientificName[dt$scientificName=="Oceanodroma"] <- "Oceanodroma sp"
dt$scientificName[dt$scientificName=="Pterodroma"] <- "Pterodroma sp"
dt$scientificName[dt$scientificName=="Phalaropus"] <- "Phalaropus sp"
dt$scientificName[dt$scientificName=="Puffinus"] <- "Puffinus sp"
dt$scientificName[dt$scientificName=="Rissa"] <- "Rissa sp"
dt$scientificName[dt$scientificName=="Somateria"] <- "Somateria sp"
dt$scientificName[dt$scientificName=="Stercorarius"] <- "Stercorarius sp"
dt$scientificName[dt$scientificName=="Sterna"] <- "Sterna sp"
dt$scientificName[dt$scientificName=="Synthliboramphus"] <- "Synthliboramphus sp"
dt$scientificName[dt$scientificName=="Uria"] <- "Uria sp"
dt$scientificName[dt$scientificName=="Phalaropus"] <- "Phalaropus sp"


dt$Genus <- str_split_fixed(dt$scientificName," ", 2)[,1]
dt$Species <- str_split_fixed(dt$scientificName," ", 2)[,2]
sum(dt$Species=="")
dt$Species[dt$Species==""] <- NA

unique(dt$Genus[is.na(dt$Family)])
unique(dt$Genus)

fam <- c("Anatidae", "Procellariidae", "Laridae", "Alcidae", "Procellariiformes", "Gaviiformes", "Gaviidae", "Hydrobatidae",
         "Diomedeidae", "Phalacrocoracidae", "Stercorariidae")
# "Ardenna"/ "Calonectris" in Procellariidae
# "Stercorarius" in Stercorariidae
# "Phalaropus" in Scolopacidae
# "Larus" in Laridae
# "Sterna" in Laridae
# "Chen" in Anatidae
# "Rynchops" in Laridae

dt$Family <- ifelse(dt$Genus %in% fam, dt$Genus, dt$Family)
dt$Genus <- ifelse(dt$Genus %in% fam, NA, dt$Genus)
dt$Family <- ifelse(dt$Genus %in% c("Ardenna", "Calonectris"), "Procellariidae", dt$Family)
dt$Family <- ifelse(dt$Genus %in% c("Stercorarius"), "Stercorariidae", dt$Family)
dt$Family <- ifelse(dt$Genus %in% c("Phalaropus"), "Scolopacidae", dt$Family)
dt$Family <- ifelse(dt$Genus %in% c("Chen"), "Anatidae", dt$Family)
dt$Family <- ifelse(dt$Genus %in% c("Rynchops", "Sterna", "Larus"), "Laridae", dt$Family)
sum(is.na(dt$Family))

sort(unique(dt$Family))
sort(unique(dt$Genus))
sort(unique(dt$Species))

sum(is.na(dt$Family) & is.na(dt$Genus) & is.na(dt$Species)) # 0



# Sample Description ===========================================================
identical(dt$id, dt$occurrenceID)  # T
identical(dt$id, dt$catalogNumber) # T

c1 <- dt %>% group_by(Latitude, Longitude) %>% summarise(n=n_distinct(year))    # mostly 1. Dealt with in rarefaction
c2 <- dt %>% group_by(occurrenceID) %>% summarise(n=n_distinct(year))           # always 1 (this id's an operation)
c3 <- dt %>% group_by(Latitude, Longitude) %>% summarise(nD=n_distinct(day),
                                                         nM=n_distinct(month),
                                                         nY=n_distinct(year)) 

rawData <- dt %>% group_by(Family, Genus, Species, Latitude, Longitude, day, month, year) %>%
  summarise(Abundance=sum(organismQuantity)) # 132261

rawData$Biomass <-  rep(NA, nrow(rawData))
rawData$Plot <-  rep(NA, nrow(rawData))
rawData$SampleDescription <- paste0(rawData$Latitude, "_", rawData$Longitude,
                                    "_", rawData$day, "_", rawData$month, "_", rawData$year)
rawData$DepthElevation <-  rep(NA, nrow(rawData))
rawData$StudyID <-  rep(527, nrow(rawData))

rawData <- rawData %>% relocate(c(Abundance, Biomass), .before=Family)
rawData <- rawData %>% relocate(c(SampleDescription, Plot), .after=Species)
rawData <- rawData %>% relocate(c(DepthElevation), .after=Longitude)

names(rawData)[names(rawData)=="day"] <- "Day"
names(rawData)[names(rawData)=="month"] <- "Month"
names(rawData)[names(rawData)=="year"] <- "Year"

rawData <- as.data.frame(rawData)
str(rawData)


# Save =========================================================================
path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIMEGithubMetadatas/Study527_Eastern_Canada_Seabirds_CR_AFE"
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
  #coord_sf(xlim = c(45,56), ylim = c(-25,-10)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal() # OK, longitudinal transect


# End of script ################################################################
