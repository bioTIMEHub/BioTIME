## ---------------------------
##
## Script name: Guangdong_Heshan_B_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Guangdong Heshan soil macrofauna community, 1994-2005 
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-27
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

setwd("~/myphd/BioTIME/Guangdong_Heshan_B-20230629T002654Z-001/Guangdong_Heshan_B_XK")   # Xuejia's working directory (PC)

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
## read files for mapping
## copy file Guangdong_Dinghu_B_namelist_XK.xlsx and rename it as Guangdong_Heshan_B_namelist_XK.xlsx
name_map <- read_excel('./Guangdong_Heshan_B_namelist_XK.xlsx', sheet=1, col_names=T, na='', skip=1)
## Rename columns
colnames(name_map) <- c("Species_Cn","Taxon","Other_name","Family_Cn")
## Split Genus and Species
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")

## read data files
## copy file ./实体数据/鹤山生物数据_土壤动物种类与数量.xlsx and rename it as Guangdong_Heshan_B_XK_RawData.xlsx
site1 <- read_excel('./Guangdong_Heshan_B_XK_RawData.xlsx', sheet=1, col_names=T, na='', skip=1)
site2 <- read_excel('./Guangdong_Heshan_B_XK_RawData.xlsx', sheet=2, col_names=T, na='', skip=1)
site3 <- read_excel('./Guangdong_Heshan_B_XK_RawData.xlsx', sheet=3, col_names=T, na='', skip=1)

## assign location
site1$Site <- rep("HSFZH01",nrow(site1))
site2$Site <- rep("HSFFZ01",nrow(site2))
site3$Site <- rep("HSFZQ01",nrow(site3))

## bind them
all <- bind_rows(site1,site2,site3)

## rename columns
colnames(all) <- c("Year","Month","Day","Sample_n","Sample_area","Species_Cn","Species_La","Abundance","Site")

## map their names
all_mapped <- left_join(all, name_map, by="Species_Cn", multiple="all")

## move names in soil to Family
all_mapped$Family <- all_mapped$Genus
all_mapped$Genus <- NA

## Check
check <- all_mapped %>% filter(is.na(Family))
unique(check$Species_Cn)

## assign their values
all_mapped$Family[all_mapped$Species_Cn == "线蚓"] <- "Enchytraeidae"
all_mapped$Family[all_mapped$Species_Cn == "蚯蚓"] <- "Lumbricina"
all_mapped$Family[all_mapped$Species_Cn == "熊虫"] <- "Tardigrada"
all_mapped$Family[all_mapped$Species_Cn == "拟蝎目"] <- "Pseudoscorpiones"
all_mapped$Family[all_mapped$Species_Cn == "盲蛛目"] <- "Opiliones"
all_mapped$Family[all_mapped$Species_Cn == "蜱螨目"] <- "Acari"
all_mapped$Family[all_mapped$Species_Cn == "倍足纳"] <- "Diplopoda"
all_mapped$Family[all_mapped$Species_Cn == "唇足纲"] <- "Chilopoda"
all_mapped$Family[all_mapped$Species_Cn == "啮虫目"] <- "Psocodea"
all_mapped$Family[all_mapped$Species_Cn == "等翅目"] <- "Isoptera"
all_mapped$Family[all_mapped$Species_Cn == "土壤线虫"] <- "Nematoda"
all_mapped$Family[all_mapped$Species_Cn == "土壤螨虫"] <- "Acari"
all_mapped$Family[all_mapped$Species_Cn == "腹足纲"] <- "Gastropods"
all_mapped$Family[all_mapped$Species_Cn == "倍足纲"] <- "Millipede"

## Add variables to data
all_mapped$Latitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Longitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Plot <- rep('', nrow(all_mapped))
all_mapped$DepthElevation <- rep('', nrow(all_mapped))
all_mapped$StudyID <- rep('', nrow(all_mapped))
all_mapped$Biomass <- as.numeric(rep('', nrow(all_mapped)))


## ---------------------------coordinates---------------------------
## assign coordinates
HSFZH01 <- data.frame(Longitude = c(angle2dec("112 53 51"),angle2dec("112 53 58")),
                      Latitude = c(angle2dec("22 40 35"),angle2dec("22 40 46")))

HSFZH01_coord <- HSFZH01 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- HSFZH01_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long


all_mapped$Longitude[all_mapped$Site == "HSFZH01"] <- 112.89847221139
all_mapped$Latitude[all_mapped$Site == "HSFZH01"] <- 22.677916669601


HSFFZ01 <- data.frame(Longitude = c(angle2dec("112 54 0"),angle2dec("112 54 5")),
                      Latitude = c(angle2dec("22 40 42"),angle2dec("22 40 48")))

HSFFZ01_coord <- HSFFZ01 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- HSFFZ01_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long


all_mapped$Longitude[all_mapped$Site == "HSFFZ01"] <- 112.900694440224
all_mapped$Latitude[all_mapped$Site == "HSFFZ01"] <- 22.6791666681639


HSFZQ01 <- data.frame(Longitude = c(angle2dec("112 53 58"),angle2dec("112 54 5")),
                      Latitude = c(angle2dec("22 40 48"),angle2dec("22 40 57")))

HSFZQ01_coord <- HSFZQ01 %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

## Calculate convex hull, area and centroid
centroid <- HSFZQ01_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long


all_mapped$Longitude[all_mapped$Site == "HSFZQ01"] <- 112.900416657802
all_mapped$Latitude[all_mapped$Site == "HSFZQ01"] <- 22.6812500029347


world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,40))


## -------------------------------------- assign Depth Elevation
all_mapped$DepthElevation[all_mapped$Site == "HSFZH01"] <- 55
all_mapped$DepthElevation[all_mapped$Site == "HSFFZ01"] <- 60
all_mapped$DepthElevation[all_mapped$Site == "HSFZQ01"] <- 55


## ---------------------------Taxonomic fields---------------------------
## check that genera are genera, not family names (-idae/eae)
str_which(all_mapped$Species, 'idae$|eae$') # none
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(all_mapped$Species)) # check species
sort(unique(all_mapped$Genus)) # check genus
sort(unique(all_mapped$Family)) # check family

## check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))
unique(check$Species_Cn) # none

# --------------------------- aggregate Abundance records that are same species, plot, and survey day.
all_aggre <- all_mapped %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, 
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
all_aggre$SampleDescription <- as.factor(with(all_aggre, paste(all_aggre$Site, all_aggre$Year, sep='_')))
unique(all_aggre$SampleDescription)

##  ---------------------------add Plot ---------------------------
table(all_aggre$Site,all_aggre$Year)
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
write.csv(all_aggre,file="./Guangdong_Heshan_B_XK.csv")


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
  

