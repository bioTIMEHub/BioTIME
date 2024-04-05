## ---------------------------
##
## Script name: Guangdong_Dinghu_P_shrub_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Guangdong Dinghu Mountain plant community composition from 2003 to 2008 - shrub
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
## Notes: Site DHFFZ01 马尾松林 doesn't have data in year 1999. Mean depth elevation is taken for site DHFZH01 季风林
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/Guangdong_Dinghu_P-20230627T173335Z-001/Guangdong_Dinghu_P_XK/Guangdong_Dinghu_P_shrub_XK")   # Xuejia's working directory (PC)

## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)
require(tidyr)

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
## copy file ./RawData/鼎湖山生物数据-灌木层植物种组成.xlsx and rename it as Guangdong_Dinghu_P_namelist_XK.xlsx
name_map <- read_excel('./Guangdong_Dinghu_P_namelist_XK.xlsx', sheet=1, col_names=T, na='', skip=1)
## Rename columns
colnames(name_map) <- c("Species_Cn","Taxon","Other_name","Family_Cn")
## Split Genus and Species
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")

## read data files
## copy file ./RawData/鼎湖山生物数据-灌木层植物种组成.xlsx and rename it as Guangdong_Dinghu_P_shrub_XK_RawData.xlsx

## 1999年季风林
DHFZH01_1999 <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=1, col_names=T, na='', 
                           range = cell_rows(2:56))
colnames(DHFZH01_1999) <- c("Species_Cn","Abundance","Biomass")
DHFZH01_1999$Year <- rep("1999", nrow(DHFZH01_1999))
DHFZH01_1999$Site <- rep("DHFZH01", nrow(DHFZH01_1999))

## 2004-2005年季风林
DHFZH01_more <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=2, col_names=T, na='', skip=1)
## rename columns
colnames(DHFZH01_more) <- c("Year","Species_Cn","Abundance","Height","Diameter","Biomass","Weight1","Weight2","Total_weight")
DHFZH01_more$Site <- rep("DHFZH01", nrow(DHFZH01_more))
## put together, 季风林
DHFZH01 <- bind_rows(DHFZH01_1999,DHFZH01_more)

## check
table(DHFZH01$Year, DHFZH01$Site) ## np



## 1999年针阔II号
DHFFZ02_1999 <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=4, col_names=T, na='', range = cell_rows(2:32))
colnames(DHFFZ02_1999) <- c("Species_Cn","Abundance","Biomass")
DHFFZ02_1999$Year <- rep("1999", nrow(DHFFZ02_1999))
DHFFZ02_1999$Site <- rep("DHFFZ02", nrow(DHFFZ02_1999))

## 2004年针阔II号
DHFFZ02_2004 <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=5, col_names=T, na='', range = cell_rows(53:72))
DHFFZ02_2004 <- DHFFZ02_2004[,2:5]
colnames(DHFFZ02_2004) <- c("Species_Cn","Abundance","Height","Biomass")
DHFFZ02_2004$Year <- rep("2004", nrow(DHFFZ02_2004))
DHFFZ02_2004$Site <- rep("DHFFZ02", nrow(DHFFZ02_2004))

## 2005年针阔II号
DHFFZ02_2005 <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=3, col_names=T, na='', range = cell_rows(2:26))
DHFFZ02_2005 <- DHFFZ02_2005[,2:6]
colnames(DHFFZ02_2005) <- c("Species_Cn","Height","Diameter","Abundance","Biomass")
DHFFZ02_2005$Year <- rep("2005", nrow(DHFFZ02_2005))
DHFFZ02_2005$Site <- rep("DHFFZ02", nrow(DHFFZ02_2005))

## put together, 针阔II号
DHFFZ02 <- bind_rows(DHFFZ02_1999,DHFFZ02_2004,DHFFZ02_2005)

## check
table(DHFFZ02$Year, DHFFZ02$Site) ## np


## 2004年马尾松林
DHFFZ01_2004 <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=5, col_names=T, na='', range = cell_rows(53:72))
DHFFZ01_2004 <- DHFFZ01_2004[,2:5]
colnames(DHFFZ01_2004) <- c("Species_Cn","Abundance","Height","Biomass")
DHFFZ01_2004$Year <- rep("2004", nrow(DHFFZ01_2004))
DHFFZ01_2004$Site <- rep("DHFFZ01", nrow(DHFFZ01_2004))


## 2005年马尾松林
DHFFZ01_2005 <- read_excel('./Guangdong_Dinghu_P_shrub_XK_RawData.xlsx', sheet=3, col_names=T, na='', range = cell_rows(26:47))
DHFFZ01_2005 <- DHFFZ01_2005[,2:6]
colnames(DHFFZ01_2005) <- c("Species_Cn","Height","Diameter","Abundance","Biomass")
DHFFZ01_2005$Year <- rep("2005", nrow(DHFFZ01_2005))
DHFFZ01_2005$Site <- rep("DHFFZ01", nrow(DHFFZ01_2005))

## put together, 马尾松林
DHFFZ01 <- bind_rows(DHFFZ01_2004,DHFFZ01_2005)

## check
table(DHFFZ01$Year, DHFFZ01$Site) ## np


## ------------------ put all together ---------------------
DHFZH01$Diameter <- as.numeric(DHFZH01$Diameter)
shrub <- bind_rows(DHFZH01,DHFFZ02,DHFFZ01)
## check
table(shrub$Year,shrub$Site) ## np


## ------------------------- map their names --------------------
shrub_mapped <- left_join(shrub, name_map, by="Species_Cn", multiple="all")

## Add variables to data
shrub_mapped$Latitude <- as.numeric(rep('', nrow(shrub_mapped)))
shrub_mapped$Longitude <- as.numeric(rep('', nrow(shrub_mapped)))
shrub_mapped$Plot <- rep('', nrow(shrub_mapped))
shrub_mapped$DepthElevation <- rep('', nrow(shrub_mapped))
shrub_mapped$Day <- rep('', nrow(shrub_mapped))
shrub_mapped$StudyID <- rep('', nrow(shrub_mapped))
shrub_mapped$Family <- rep('', nrow(shrub_mapped))
shrub_mapped$Month <- rep('', nrow(shrub_mapped))


## ---------------------------coordinates---------------------------
shrub_mapped$Longitude[shrub_mapped$Site == "DHFFZ01"] <- 112.556521752549 
shrub_mapped$Latitude[shrub_mapped$Site == "DHFFZ01"] <- 23.1662300870867
shrub_mapped$Longitude[shrub_mapped$Site == "DHFZH01"] <- 112.539435975954 
shrub_mapped$Latitude[shrub_mapped$Site == "DHFZH01"] <- 23.1698111592563
shrub_mapped$Longitude[shrub_mapped$Site == "DHFFZ02"] <- 112.548245374589 
shrub_mapped$Latitude[shrub_mapped$Site == "DHFFZ02"] <- 23.1724564824651


## --------------------------- Depth Elevation -----------------
shrub_mapped$DepthElevation[shrub_mapped$Site == "DHFFZ02"] <- 150
shrub_mapped$DepthElevation[shrub_mapped$Site == "DHFFZ01"] <- 100 
## mean is taken
shrub_mapped$DepthElevation[shrub_mapped$Site == "DHFZH01"] <- 290  



world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=shrub_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,40))

## ---------------------------Taxonomic fields---------------------------
## check that genera are genera, not family names (-idae/eae)
str_which(shrub_mapped$Species, 'idae$|eae$') # none
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(shrub_mapped$Species)) # check species
shrub_mapped$Species[shrub_mapped$Species == "chinensis(Champ.ex"] <- "chinensis"

sort(unique(shrub_mapped$Genus)) # check genus, np
sort(unique(shrub_mapped$Family_Cn)) # check family, np

## Assign Plantae to family
shrub_mapped$Family <- "Plantae"

## check names that are not mapped successfully
check <- shrub_mapped %>% filter(is.na(Species))
unique(check$Species_Cn)

shrub_mapped$Genus[shrub_mapped$Species_Cn == "大青藤"] <- "Sinomenium"
shrub_mapped$Species[shrub_mapped$Species_Cn == "大青藤"] <- "acutum"

## check again
check <- shrub_mapped %>% filter(is.na(Species))
unique(check$Species_Cn) ## none
unique(check$Other_name) ## visual check, no AB
## done

# --------------------------- aggregate biomass records that are same species, plot, and survey day.
shrub_aggre <- shrub_mapped %>% group_by(Abundance, Family, Genus, Species, Plot, Latitude, Longitude, 
                                         DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(shrub_aggre)[1]-dim(shrub_mapped)[1] # check changes

# --------------------------- aggregate Abundance records that are same species, plot, and survey day.
shrub_aggre <- shrub_aggre %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, 
                                         DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(shrub_aggre)[1]-dim(shrub_mapped)[1] # check changes

##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
shrub_aggre$Biomass[shrub_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
shrub_aggre$Abundance[shrub_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
shrub_aggre <- shrub_aggre[!with(shrub_aggre,is.na(Abundance)& is.na(Biomass)),]

## check
shrub_aggre %>% filter(is.na(Species)) # none

##  ---------------------------add SampleDescription ---------------------------
shrub_aggre$SampleDescription <- as.factor(with(shrub_aggre, paste(shrub_aggre$Site, shrub_aggre$Year, sep='_')))
unique(shrub_aggre$SampleDescription)

##  ---------------------------add Plot ---------------------------
## none

##  ---------------------------final check ---------------------------
str(shrub_aggre)

##  ---------------------------final format ---------------------------
shrub_aggre <- shrub_aggre[c('Abundance',
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

str(shrub_aggre) # final check!

## Export
write.csv(shrub_aggre,file="./Guangdong_Dinghu_P_shrub_XK.csv")


dt_coord <- shrub_aggre %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)


write_clip(shrub_aggre) # copy





