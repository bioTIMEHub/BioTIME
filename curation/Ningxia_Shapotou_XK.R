## ---------------------------
##
## Script name: Ningxia_Shapotou_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Ningxia Shapotou desert fauna community, 2003-2008
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-29
##
## Copyright (c) Xuejia Ke, 2023
## Email: xk5@st-andrews.ac.uk
##
## ---------------------------
##
## Notes:
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/Ningxia_Shapotou-20230629T182852Z-001/Ningxia_Shapotou_XK")   # Xuejia's working directory (PC)

## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)
require(sf)
require(clipr)

## load functions
## Function if coordinates are ever in degree minutes seconds

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}


## ---------------------------read files-----------------------------
## copy file ./实体数据/SPD_SWSJ_动物群落组成与结构.xls and rename it as Ningxia_Shapotou_XK_RawData.xls
## read file for mapping
name_map <- read_excel('./Ningxia_Shapotou_namelist_XK.xlsx', sheet=1, col_names=T, na='', skip=1)
## Rename columns
colnames(name_map) <- c("Species_Cn","Taxon","Category")
## Split Genus and Species
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")

## move insects Genus names to Family
name_map$Family <- rep("",nrow(name_map))
name_map[48:65,"Family"] <- name_map[48:65,"Genus"]
name_map$Genus[name_map$Category == "昆虫"] <- NA

## read data files
SPDZH02 <- read_excel('./Ningxia_Shapotou_XK_RawData.xls', sheet=1, range = cell_rows(3:45), col_names=T, na='')
SPDFZ02 <- read_excel('./Ningxia_Shapotou_XK_RawData.xls', sheet=1, range = cell_rows(47:105), col_names=T, na='')

SPDZH02$Site <- rep("SPDZH02",nrow(SPDZH02))
SPDFZ02$Site <- rep("SPDFZ02",nrow(SPDFZ02))

all <- bind_rows(SPDZH02,SPDFZ02)

## rename columns
colnames(all) <- c("Year","Month","Method","Species_Cn","Species_En","Abundance","Notes","Site")

## map their names
all_mapped <- left_join(all, name_map, by="Species_Cn", multiple="all")

## Add variables to data
all_mapped$Latitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Longitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Plot <- rep('', nrow(all_mapped))
all_mapped$DepthElevation <- rep('', nrow(all_mapped))
all_mapped$Day <- rep('', nrow(all_mapped))
all_mapped$StudyID <- rep('', nrow(all_mapped))
all_mapped$Biomass <- as.numeric(rep('', nrow(all_mapped)))

## ---------------------------coordinates---------------------------
## assign coordinates
SPDZH02 <- data.frame(Longitude = c(angle2dec("105 0 28"),angle2dec("105 0 1"),
                                    angle2dec("105 0 23"),angle2dec("104 59 56")),
                      Latitude = c(angle2dec("37 28 8"),angle2dec("37 28 4"),
                                   angle2dec("37 28 18"),angle2dec("37 28 14")))

SPDZH02_coord <- SPDZH02 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- SPDZH02_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long


all_mapped$Longitude[all_mapped$Site == "SPDZH02"] <- 105.00333331225
all_mapped$Latitude[all_mapped$Site == "SPDZH02"] <- 37.4697222733529


SPDFZ02 <- data.frame(Longitude = c(angle2dec("105 0 21"),angle2dec("105 0 25")),
                      Latitude = c(angle2dec("37 27 15"),angle2dec("37 27 33")))

SPDFZ02_coord <- SPDFZ02 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- SPDFZ02_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long


all_mapped$Longitude[all_mapped$Site == "SPDFZ02"] <- 105.006388870317
all_mapped$Latitude[all_mapped$Site == "SPDFZ02"] <- 37.456666667967


world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,50))


## assign Depth Elevation
all_mapped$DepthElevation[all_mapped$Site == "SPDZH02"] <- 1350
all_mapped$DepthElevation[all_mapped$Site == "SPDFZ02"] <- 1230

## ---------------------------Taxonomic fields---------------------------
## assign values
index <- with(all_mapped, which(is.na(Family) & !is.na(Species_En)))
all_mapped[index,"Family"] <- all_mapped[index,"Species_En"]

## check that genera are genera, not family names (-idae/eae)
str_which(all_mapped$Species, 'idae$|eae$') # none
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(all_mapped$Species)) # check species
sort(unique(all_mapped$Genus)) # check genus
sort(unique(all_mapped$Family)) # check family

## check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))
unique(check$Species_Cn) ## none

unique(all_mapped$Species_Cn) ## check

# --------------------------- aggregate biomass & abundance records that are same species, plot, and survey day.
## aggregate abundance
all_aggre <- all_mapped %>% group_by(Biomass,Family, Genus, Species, Plot, Latitude, Longitude, 
                                    DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all_mapped)[1] # check changes

##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
all_aggre$Biomass[all_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
all_aggre$Abundance[all_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
all_aggre <- all_aggre[!with(all_aggre,is.na(Abundance)& is.na(Biomass)),]

## check
all_aggre %>% filter(is.na(Family)) # none

##  ---------------------------add SampleDescription ---------------------------
all_aggre$SampleDescription <- as.factor(with(all_aggre, paste(all_aggre$Site, all_aggre$Month, all_aggre$Year, sep='_')))
unique(all_aggre$SampleDescription)


##  ---------------------------add Plot ---------------------------
## none

##  ---------------------------final check ---------------------------
str(all_aggre)

##  ---------------------------final format ---------------------------
all_aggre <- all_aggre[c('Abundance',
                         'Biomass',
                         'Family','Genus',
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

str(all_aggre) # final check!

## Export
write.csv(all_aggre,file="./Ningxia_Shapotou_XK.csv")


dt_coord <- all_aggre %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


write_clip(all_aggre) # copy
