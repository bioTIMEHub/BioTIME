## ---------------------------
##
## Script name: InnerMongolia_Xilingol_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Inner Mongolia Xilingol plant assemblage monitoring, 2005-2008
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-28
##
## Copyright (c) Xuejia Ke, 2023
## Email: xk5@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: mean depth elevation of site 综合观测场 (NMGZH01) is taken.
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/InnerMongolia_Xilingol-20230629T145440Z-001/InnerMongolia_Xilingol_XK")   # Xuejia's working directory (PC)

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
## copy file ./实体数据/DP2011_NMG_SWSJ.xls and rename it as InnerMongolia_Xilingol_XK_RawData.xls
## read file for mapping
name_map <- read_excel('./InnerMongolia_Xilingol_XK_RawData.xls', sheet=1, col_names=T, na='', skip=2)
name_map <- name_map[-1]

## Rename columns
colnames(name_map) <- c("Species_Cn","Family_Cn","Taxon")
## Split Genus and Species
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")

## correct the namelist
name_map[17:19,"Genus"] <- "Artemisia"
name_map[22:23,"Genus"] <- "Chenopodium"
name_map[28:32,"Genus"] <- "Allium"
name_map[37,"Genus"] <- "Thalictrum"
name_map[40:41,"Genus"] <- "Potentilla"
name_map[40:41,"Genus"] <- "Potentilla"
name_map$Genus[name_map$Species_Cn == "乳浆大戟"] <- "Euphorbia"
name_map$Species[name_map$Species_Cn == "乳浆大戟"] <- "esula"


## read data files
NMGZH01 <- read_excel('./InnerMongolia_Xilingol_XK_RawData.xls', sheet=2, range = cell_rows(2:119),col_names=T, na='')
NMGZH01$Site <- rep("NMGZH01",nrow(NMGZH01))

NMGFZ01 <- read_excel('./InnerMongolia_Xilingol_XK_RawData.xls', sheet=2, range = cell_rows(127:221),col_names=T, na='')
NMGFZ01$Site <- rep("NMGFZ01",nrow(NMGFZ01))

all <- bind_rows(NMGZH01,NMGFZ01)

## rename columns
colnames(all) <- c("Year","Species_Cn","Abundance","Biomass","Above_weight","Under_weight","Notes","Site")

## map their names
all_mapped <- left_join(all, name_map, by="Species_Cn", multiple="all")


## Add variables to data
all_mapped$Latitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Longitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Plot <- rep('', nrow(all_mapped))
all_mapped$DepthElevation <- rep('', nrow(all_mapped))
all_mapped$Day <- rep('', nrow(all_mapped))
all_mapped$StudyID <- rep('', nrow(all_mapped))
all_mapped$Family <- rep('', nrow(all_mapped))
all_mapped$Month <- rep('', nrow(all_mapped))

## assign values to month
all_mapped$Month <- 8


## ---------------------------coordinates---------------------------
## assign coordinates
all_mapped$Longitude[all_mapped$Site == "NMGZH01"] <- angle2dec("116 40 40")
all_mapped$Latitude[all_mapped$Site == "NMGZH01"] <- angle2dec("43 32 29")

all_mapped$Longitude[all_mapped$Site == "NMGFZ01"] <- angle2dec("116 33 17")
all_mapped$Latitude[all_mapped$Site == "NMGFZ01"] <- angle2dec("43 33 22")


world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,50))


## ---------------------------Taxonomic fields---------------------------
## check that genera are genera, not family names (-idae/eae)
str_which(all_mapped$Species, 'idae$|eae$') # none
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(all_mapped$Species)) # check species
sort(unique(all_mapped$Genus)) # check genus
sort(unique(all_mapped$Family)) # check family

## check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Species))
unique(check$Species_Cn)

## assign values
all_mapped$Genus[all_mapped$Species_Cn == "直立黄芪"] <- "Astragalus"
all_mapped$Species[all_mapped$Species_Cn == "直立黄芪"] <- "adsurgens"

## check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Species))
unique(check$Species_Cn) ##None

## ------------------- Depth Elevation------------
all_mapped$DepthElevation[all_mapped$Site == "NMGZH01"] <- 1225 ## take the mean
all_mapped$DepthElevation[all_mapped$Site == "NMGFZ01"] <- 1130


# --------------------------- aggregate biomass & abundance records that are same species, plot, and survey day.
## convert variables into factors
#cols <- colnames(all_mapped)
#all_mapped[cols] <- lapply(all_mapped[cols] , factor)

## aggregate biomass
all_mapped$Biomass <- as.numeric(all_mapped$Biomass)
all_mapped$Abundance <- as.numeric(all_mapped$Abundance)
all_aggre <- all_mapped %>% group_by(Abundance,Family, Genus, Species, Plot, Latitude, Longitude, 
                                      DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all_mapped)[1] # check changes

## aggregate abundance
all_aggre <- all_aggre %>% group_by(Biomass,Family, Genus, Species, Plot, Latitude, Longitude, 
                                    DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all_aggre)[1] # check changes


##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
all_aggre$Biomass[all_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
all_aggre$Abundance[all_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
all_aggre <- all_aggre[!with(all_aggre,is.na(Abundance)& is.na(Biomass)),]

## check
all_aggre %>% filter(is.na(Species)) # none

##  ---------------------------add SampleDescription ---------------------------
all_aggre$SampleDescription <- as.factor(with(all_aggre, paste(all_aggre$Site, all_aggre$Year, sep='_')))
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
write.csv(all_aggre,file="./InnerMongolia_Xilingol_XK.csv")


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
