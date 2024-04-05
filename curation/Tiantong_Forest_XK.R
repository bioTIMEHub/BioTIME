## ---------------------------
##
## Script name: Tiantong_Forest_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## A dataset of species composition and biomass in successional stages of Tiantong typical evergreen broad-leaved forest (2008–2017)
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

setwd("~/myphd/BioTIME/Tiantong_Forest_XK")   # Xuejia's working directory (PC)

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
## read data file
## copy file ./2008-2017天童演替系列样地数据集.xlsx and rename it as Tiantong_Forest_XK_RawData.xlsx
all <- read_excel('./Tiantong_Forest_XK_RawData.xlsx', sheet=1, col_names=T, na='')

## rename columns
colnames(all) <- c("Index","Year","Site","Layer","Species_Cn","Taxon","Diameter","Biomass","Abundance")

## check years
unique(all$Year)

## Split Genus and Species
all <- all %>% separate(Taxon, c("Genus", "Species"), " ")

## Add variables to data
all$Latitude <- as.numeric(rep('', nrow(all)))
all$Longitude <- as.numeric(rep('', nrow(all)))
all$Plot <- rep('', nrow(all))
all$DepthElevation <- rep('', nrow(all))
all$Day <- rep('', nrow(all))
all$StudyID <- rep('', nrow(all))
all$Family <- rep('', nrow(all))
all$Month <- rep('', nrow(all))

## ---------------------------coordinates---------------------------
## assign coordinates
all$Latitude[all$Site == "常绿灌丛"] <- angle2dec("29 48 1")
all$Longitude[all$Site == "常绿灌丛"] <- angle2dec("121 47 23")

all$Latitude[all$Site == "木荷林"] <- angle2dec("29 48 0")
all$Longitude[all$Site == "木荷林"] <- angle2dec("121 47 32")

all$Latitude[all$Site == "栲树林"] <- angle2dec("29 48 7")
all$Longitude[all$Site == "栲树林"] <- angle2dec("121 47 29")

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,50))


## ---------------------------Depth Elevation-----------------------
## assign Depth Elevation
all$DepthElevation[all$Site == "常绿灌丛"] <- 230
all$DepthElevation[all$Site == "木荷林"] <- 145
all$DepthElevation[all$Site == "栲树林"] <- 185

## ---------------------------Taxonomic fields---------------------------
## check that genera are genera, not family names (-idae/eae)
str_which(all$Species, 'idae$|eae$') # none
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(all$Species)) # check species
sort(unique(all$Genus)) # check genus
sort(unique(all$Family)) # check family

## check names that are not mapped successfully
check <- all %>% filter(is.na(Species))
unique(check$Species_Cn)

## correct values
all$Genus[all$Species_Cn == "野茉莉"] <- "Styrax"
all$Species[all$Species_Cn == "野茉莉"] <- "japonicus"

## correct unknown records
all$Family[all$Species_Cn == "未知"] <- "Plantae"

## again check names that are not mapped successfully
check <- all %>% filter(is.na(Species))
unique(check$Species_Cn)

check <- all %>% filter(is.na(Genus))
unique(check$Species_Cn)

check <- all %>% filter(is.na(Family))
unique(check$Species_Cn) ## none

##  ---------------------------add Plot ---------------------------
all$Plot <- all$Site

## rename Plot
all$Plot[all$Plot == "常绿灌丛"] <- "evergreen_shrubland"
all$Plot[all$Plot == "木荷林"] <- "magnolia_woodland"
all$Plot[all$Plot == "栲树林"] <- "oak_woodland"


# --------------------------- aggregate biomass & abundance records that are same species, plot, and survey day.
## aggregate abundance
all_aggre <- all %>% group_by(Biomass,Family, Genus, Species, Plot, Latitude, Longitude, 
                                     DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all)[1] # check changes

## aggregate Biomass
all_aggre <- all_aggre %>% group_by(Abundance,Family, Genus, Species, Plot, Latitude, Longitude, 
                              DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all)[1] # check changes


##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
all_aggre$Biomass[all_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
all_aggre$Abundance[all_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
all_aggre <- all_aggre[!with(all_aggre,is.na(Abundance)& is.na(Biomass)),]

##  ---------------------------add SampleDescription ---------------------------
all_aggre$SampleDescription <- as.factor(with(all_aggre, paste(all_aggre$Year, all_aggre$DepthElevation, sep='_')))
unique(all_aggre$SampleDescription)


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
write.csv(all_aggre,file="./Tiantong_Forest_XK.csv")

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


