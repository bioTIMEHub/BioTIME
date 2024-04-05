## ---------------------------
##
## Script name: BSC_macroinverts_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## RivFish ID 38 Vincent Resh Big Sulfur Creek - benthic macroinvertebrates
##
## Author: Xuejia Ke
##
## Date Created: 2023-07-01
##
## Copyright (c) Xuejia Ke, 2023
## Email: xk5@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: Methods info for site 1 is missing, info of site 5 is used.
## 
##   
## ---------------------------

## set working directory

setwd("~/myphd/BioTIME/BSC_macroinverts_XK")   # Xuejia's working directory (PC)

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
## read files
name_map <- read_excel('./BSC_macroinverts_XK_RawData.xls', sheet=2, col_names=T, na='')
abundance_raw <- read_excel('./BSC_macroinverts_XK_RawData.xls', sheet=1, col_names=T, na='')

## drop entry & reformat the data frame
abundance_raw <- abundance_raw[-1]
abundance_raw <- abundance_raw %>% pivot_longer(Acarin:Zaitze,names_to = "Taxon", values_to = "Abundance")

## Rename the column name
colnames(name_map)[colnames(name_map) == "Code"] ="Taxon"
## map their names
all_mapped <- left_join(abundance_raw, name_map, by="Taxon", multiple="all")

## extract date
unique(all_mapped$Year)
typeof(all_mapped$Year)

all_mapped$Year <- as.character(all_mapped$Year)
all_mapped$Year <- paste0("19",all_mapped$Year,sep="")
all_mapped$Year <- as.numeric(all_mapped$Year)

unique(all_mapped$Year) ## check fine
unique(all_mapped$Month) ## check

## Add variables to data
all_mapped$Latitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Longitude <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Plot <- rep('', nrow(all_mapped))
all_mapped$DepthElevation <- rep('', nrow(all_mapped))
all_mapped$StudyID <- rep('', nrow(all_mapped))
all_mapped$Biomass <- as.numeric(rep('', nrow(all_mapped)))
all_mapped$Day <- rep('', nrow(all_mapped))


## ---------------------------coordinates---------------------------
## assign coordinates
all_mapped$Latitude <- angle2dec("38 48 3")
all_mapped$Longitude <- angle2dec("122 48 36")


world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=all_mapped %>% distinct(Latitude, Longitude, Year), 
                             aes(x=Longitude, y=Latitude, fill = Year), shape=21)

points + coord_fixed(xlim=c(100,150), ylim=c(0,50))


## ---------------------------Depth Elevation---------------------------
## assign Depth Elevation
all_mapped$DepthElevation <- 415


## ---------------------------Taxonomic fields---------------------------
## check
unique(all_mapped$Genus)
unique(all_mapped$Family)


## --------------------- check Family ---------------------
## check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))

unique(check$Order)

## assign order to family when family is na
index <- which(is.na(all_mapped$Family))
unique(all_mapped[index,"Order"]) ## check

all_mapped[index,"Family"] <- all_mapped[index,"Order"]

## again check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))

unique(check$Order) ## none


## Final check
check <- all_mapped %>% filter(is.na(Family))
unique(check$Order) ## none

sum(is.na(all_mapped$Family)) ## 0
## Done

## get species from Genus
all_mapped$Species <- word(all_mapped$Genus, start=2)
all_mapped$Genus <- word(all_mapped$Genus, start=1)


## check that genera are genera, not family names (-idae/eae)
str_which(all_mapped$Species, 'idae$|eae$') # none

## check
unique(all_mapped$Species)
unique(all_mapped$Genus)
unique(all_mapped$Family)

## check
check <- all_mapped %>% filter(Genus == "Amphinemura/Malenka")
unique(check$Species) ## none
unique(check$Family) ## "Nemouridae"
unique(check$Order) ## "Plecoptera"

## Malenka is the sister genus of Amphinemura
## replace it with Amphinemura
all_mapped$Genus[all_mapped$Genus == "Amphinemura/Malenka"] <- "Amphinemura"
unique(all_mapped$Genus) ## visual check, np

## check species
unique(all_mapped$Species)

## convert OTU to sp in species
all_mapped$Species <- gsub("OTU-", "sp", all_mapped$Species)

## check
unique(all_mapped$Species) ## visual check, np
unique(all_mapped$Genus) ## visual check, np
unique(all_mapped$Family) ## visual check, np


# --------------------------- aggregate biomass & abundance records that are same species, plot, and survey day.
## aggregate abundance
all_aggre <- all_mapped %>% group_by(Biomass,Family, Genus, Species, Plot, Latitude, Longitude, 
                                     DepthElevation, Day, Month, Year, StudyID, SITE) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(all_aggre)[1]-dim(all_mapped)[1] # check changes

## check
all_aggre %>% filter(is.na(Family)) # none



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
unique(all_mapped$SITE)
unique(all_mapped$Month)

## check
table(all_mapped$Month, all_mapped$SITE)

all_aggre$SampleDescription <- as.factor(with(all_aggre, paste(all_aggre$SITE, all_aggre$Month, all_aggre$Year, sep='_')))
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

str(all_aggre) # final check, np

## Export
write.csv(all_aggre,file="./BSC_macroinverts_XK.csv")

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

