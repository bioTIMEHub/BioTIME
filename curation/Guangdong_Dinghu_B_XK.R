## ---------------------------
##
## Script name: Guangdong_Dinghu_B_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## CERN Guangdong Dinghu Mountain species composition and soil microbial biomass community, 2004 to 2005
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
## Notes: mean depth elevation is used for "季风林" ("DHFZH01"), central coordiates are used for calculating the area
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/Guangdong_Dinghu_B-20230628T195026Z-001/Guangdong_Dinghu_B_XK")   # Xuejia's working directory (PC)

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
## copy file ./实体数据/鼎湖山生物数据-动物名录.xlsx and rename it as Guangdong_Dinghu_B_namelist_XK.xlsx
name_map <- read_excel('./Guangdong_Dinghu_B_namelist_XK.xlsx', sheet=1, col_names=T, na='', skip=1)
## Rename columns
colnames(name_map) <- c("Species_Cn","Taxon","Category")
## Split Genus and Species
name_map <- name_map %>% separate(Taxon, c("Genus", "Species"), " ")
## correct
name_map$Species[name_map$Genus == "Rattusedwardsi"] <- "edwardsi"
name_map$Species[name_map$Genus == "Rattus"] <- "Rattus"

## Move some Genus names to Family
name_map$Family <- rep('', nrow(name_map))
name_map[48:65,"Family"] <- name_map[48:65,"Genus"]
name_map[48:65,"Genus"] <- NA

## read data files
## copy file ./实体数据/鼎湖山生物数据-大型土壤动物种类与数量.xlsx and rename it as Guangdong_Dinghu_B_soil_XK_RawData.xlsx
## copy file ./实体数据/鼎湖山生物数据-大型野生动物种类与数量.xlsx and rename it as Guangdong_Dinghu_B_animal_XK_RawData.xlsx
soil <- read_excel('./Guangdong_Dinghu_B_soil_XK_RawData.xlsx', sheet=1, col_names=T, na='', skip=1)
animal <- read_excel('./Guangdong_Dinghu_B_animal_XK_RawData.xlsx', sheet=1, col_names=T, na='', skip=1)

## we only need insects in animal data sheet
animal <- animal[12:41,]

colnames(animal) <- c("Site","Area","Category","Species_Cn","Abundance")
colnames(soil) <- c("Site","Species_Cn","Species_Latin","Abundance")

## map their names
animal_mapped <- left_join(animal, name_map, by="Species_Cn", multiple="all")
soil_mapped <- left_join(soil, name_map, by="Species_Cn", multiple="all")

## move names in soil to Family
soil_mapped$Family <- soil_mapped$Species_Latin

## assign year
soil_mapped$Year <- as.numeric(rep('2004', nrow(soil_mapped)))
animal_mapped$Year <- as.numeric(rep('2005', nrow(animal_mapped)))


## ------------------- assign Depth Elevation
animal_mapped$DepthElevation[animal_mapped$Site == "针阔II号"] <- 190
animal_mapped$DepthElevation[animal_mapped$Site == "针阔Ⅱ号"] <- 190
soil_mapped$DepthElevation[soil_mapped$Site == "针阔II号"] <- 160
soil_mapped$DepthElevation[soil_mapped$Site == "针阔Ⅱ号"] <- 160

animal_mapped$DepthElevation[animal_mapped$Site == "马尾松林"] <- 80
soil_mapped$DepthElevation[soil_mapped$Site == "马尾松林"] <- 180

## mean is taken
animal_mapped$DepthElevation[animal_mapped$Site == "季风林"] <- 290
soil_mapped$DepthElevation[soil_mapped$Site == "季风林"] <- 290


## bind them together
animal_mapped$Abundance <- as.numeric(animal_mapped$Abundance)
all <- bind_rows(animal_mapped,soil_mapped)

## Add variables to data
all$Latitude <- as.numeric(rep('', nrow(all)))
all$Longitude <- as.numeric(rep('', nrow(all)))
all$Plot <- rep('', nrow(all))
all$Day <- rep('', nrow(all))
all$StudyID <- rep('', nrow(all))
all$Month <- rep('', nrow(all))
all$Biomass <- as.numeric(rep('', nrow(all)))

## Rename Sites
all$Site[all$Site == "季风林"] <- "DHFZH01"
all$Site[all$Site == "针阔II号"] <- "DHFFZ02"
all$Site[all$Site == "马尾松林"] <- "DHFFZ01"
all$Site[all$Site == "针阔Ⅱ号"] <- "DHFFZ02"

unique(all$Site) # check

## ---------------------------coordinates---------------------------
all$Longitude[all$Site == "DHFZH01"] <- 112.539435975954
all$Latitude[all$Site == "DHFZH01"] <- 23.1698111592563
all$Longitude[all$Site == "DHFFZ02"] <- 112.548245374589
all$Latitude[all$Site == "DHFFZ02"] <- 23.1724564824651
all$Longitude[all$Site == "DHFFZ01"] <- 112.556521752549
all$Latitude[all$Site == "DHFFZ01"] <- 23.1662300870867

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,40))



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

# --------------------------- aggregate Abundance records that are same species, plot, and survey day.
all_aggre <- all %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, 
                              DepthElevation, Day, Month, Year, StudyID, Site) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all)[1] # check changes

##  ---------------------------Remove records with no AB ---------------------------
## covert 0 Biomass to NA
all_aggre$Biomass[all_aggre$Biomass == 0] <- NA
## covert 0 Abundance to NA
all_aggre$Abundance[all_aggre$Abundance == 0] <- NA
## remove records with no Abundance and Biomass
all_aggre <- all_aggre[!with(all_aggre,is.na(Abundance)& is.na(Biomass)),]

## check
check <- all_aggre %>% filter(is.na(Species)) # Has Family

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
write.csv(all_aggre,file="./Guangdong_Dinghu_B_XK.csv")

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
