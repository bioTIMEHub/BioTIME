## ---------------------------
##
## Script name: HCKC_macroinverts_XK.R
##
## Purpose of script: Data Curation for BioTIME Database
## RivFish ID 38 Vincent Resh Hunting Creek and Knoxville Creek macroinvertes
##
## Author: Xuejia Ke
##
## Date Created: 2023-06-30
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

setwd("~/myphd/BioTIME/HCKC_macroinverts_XK")   # Xuejia's working directory (PC)

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
site_date_entry <- read_excel('./HCKC_macroinverts_XK_RawData.xls', sheet=2, col_names=T, na='')
name_map <- read_excel('./HCKC_macroinverts_XK_RawData.xls', sheet=4, col_names=T, na='')
abundance_raw <- read_excel('./HCKC_macroinverts_XK_RawData.xls', sheet=5, col_names=T, na='')

## check if entry is redundant
length(abundance_raw$Entry) ## good
length(site_date_entry$Entry) ## good

## combine by entry
all <- merge(abundance_raw,site_date_entry,by="Entry")

## drop entry & reformat the data frame
all <- all[-1]
all <- all %>% pivot_longer(Acarin:Zoniag,names_to = "Taxon", values_to = "Abundance")

## check
length(unique(all$Year)) ## 20

## check if taxon match
a <- unique(all$Taxon)
b <- unique(name_map$Taxacode)

setdiff(a,b) ## none
identical(a,b) ## True

## map their names
colnames(name_map)[1] <- "Taxon"
all_mapped <- left_join(all, name_map, by="Taxon", multiple="all")

## Rename the column name
colnames(all_mapped)[colnames(all_mapped) == "Month"] ="Month_En"
colnames(all_mapped)[colnames(all_mapped) == "Month_name"] ="Month"

colnames(all_mapped)[colnames(all_mapped) == "Site"] ="Site_n"
colnames(all_mapped)[colnames(all_mapped) == "Site_name2"] ="Site"

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
all_mapped$Longitude[all_mapped$Site == "1D"] <- angle2dec("122 18 53")
all_mapped$Latitude[all_mapped$Site == "1D"] <- angle2dec("38 47 56")

all_mapped$Longitude[all_mapped$Site == "1P"] <- angle2dec("122 24 54")
all_mapped$Latitude[all_mapped$Site == "1P"] <- angle2dec("38 51 56")

all_mapped$Longitude[all_mapped$Site == "2D"] <- angle2dec("122 22 36")
all_mapped$Latitude[all_mapped$Site == "2D"] <- angle2dec("38 48 30")

all_mapped$Longitude[all_mapped$Site == "2P"] <- angle2dec("122 22 45")
all_mapped$Latitude[all_mapped$Site == "2P"] <- angle2dec("38 49 45")


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
all_mapped$DepthElevation[all_mapped$Site == "1D"] <- 390
all_mapped$DepthElevation[all_mapped$Site == "1P"] <- 634
all_mapped$DepthElevation[all_mapped$Site == "2D"] <- 348
all_mapped$DepthElevation[all_mapped$Site == "2P"] <- 402


## ---------------------------Taxonomic fields---------------------------
## convert unknow into NA in Family and Genus
all_mapped$Family[all_mapped$Family == "unknown"] <- NA
all_mapped$Genus[all_mapped$Genus == "unknown"] <- NA

## assign values
## convert OTU to sp
## find index with only integers in "Species/OTU"
list <- as.character(1:20)
ind <- which(all_mapped$`Species/OTU` %in% list)
ind_update <- all_mapped[ind,is.na("Species/OTU")]
for (i in ind){
  all_mapped[i,"Species/OTU"] <- paste0("sp",all_mapped[i,"Species/OTU"])
}

## correct
index <- str_which(all_mapped$Description, 'sp. 1')
all_mapped[index,"Species/OTU"] <- "sp1"

index <- str_which(all_mapped$Description, 'OTU-1')
all_mapped[index,"Species/OTU"] <- "sp1"


## rename
colnames(all_mapped)[colnames(all_mapped) == "Species/OTU"] = "Species"

## check that genera are genera, not family names (-idae/eae)
str_which(all_mapped$Species, 'idae$|eae$') # none


# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(all_mapped$Species)) # check species
sort(unique(all_mapped$Genus)) # check genus
sort(unique(all_mapped$Family)) # check family

## check names that are not mapped successfully
check <- check %>% filter(is.na(Family))

unique(check$Order)

## put Order into family
index <- which(is.na(all_mapped$Family))
unique(all_mapped[index,"Order"]) ## check

all_mapped[index,"Family"] <- all_mapped[index,"Order"]

## again check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))

unique(check$Order) ## none
unique(check$Class)

## put Class into family
index <- which(is.na(all_mapped$Family))
all_mapped[index,"Family"] <- all_mapped[index,"Class"]

## again check names that are not mapped successfully
check <- all_mapped %>% filter(is.na(Family))

unique(check$Order) ## none
unique(check$Class) ## none
unique(check$Phylum)

## put Phylum into family
index <- which(is.na(all_mapped$Family))
all_mapped[index,"Family"] <- all_mapped[index,"Phylum"]


## Final check
check <- all_mapped %>% filter(is.na(Family))
unique(check$Order) ## none
unique(check$Class) ## none
unique(check$Phylum) ## none

sum(is.na(all_mapped$Family)) ## 0
## Done

## ------------- check species-------------------
check <- all_mapped %>% filter(is.na(Species))
unique(check$Description)

## correct values
index <- str_which(all_mapped$Description, '1')
all_mapped[index,"Species"] <- "sp1"

index <- str_which(all_mapped$Description, '2')
all_mapped[index,"Species"] <- "sp2"

index <- str_which(all_mapped$Description, '3')
all_mapped[index,"Species"] <- "sp3"

check <- all_mapped %>% filter(is.na(Species))
identified <- unique(check$Description)

## assign species with sp1 for those identified species in Description
for (i in identified){
  all_mapped$Species[all_mapped$Description == i] <- "sp1"
}

check <- all_mapped %>% filter(is.na(Species)) ## none

sum(is.na(all_mapped$Species)) ## 0
## done

## ------------- check redundance-------------------
## make sure description is unique for each combination of family, genus, species
combinations <- unique(all_mapped[,c('Family','Genus','Species')])

my_list <- list()  

for (i in (1:nrow(combinations))){
  condition <- as.data.frame(combinations[i,])
  test <- inner_join(all_mapped,condition)
  check <- length(unique(test$Description))
  if (check >= 2){
    my_list[[i]] <- condition
  }
}
my_list[[31]]
my_list[[32]]
my_list[[33]]

## check each abnormal1
abnormal1 <- inner_join(all_mapped,my_list[[31]])
unique(abnormal1$Description)

## correct them
all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp1" &
                     all_mapped$Description == "OTU-10"] <- "sp10"

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp1" &
                     all_mapped$Description == "OTU-11"] <- "sp11"

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp1" &
                     all_mapped$Description == "OTU-14"] <- "sp14"

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp1" &
                     all_mapped$Description == "OTU-15"] <- "sp15"

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp1" &
                     all_mapped$Description == "Taxon AF Chiro OTU-16"] <- "sp16"

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp1" &
                     all_mapped$Description == "Taxon AL Chiro OTU-10A"] <- "sp10"

abnormal1 <- inner_join(all_mapped,my_list[[31]])
unique(abnormal1$Description) ## done


## check each abnormal2
abnormal2 <- inner_join(all_mapped,my_list[[32]])
unique(abnormal2$Description)

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp2" &
                     all_mapped$Description == "OTU-12"] <- "sp12"

abnormal2 <- inner_join(all_mapped,my_list[[32]])
unique(abnormal2$Description) ## done


## check each abnormal3
abnormal3 <- inner_join(all_mapped,my_list[[33]])
unique(abnormal3$Description)

all_mapped$Species[all_mapped$Family == "Chironomidae" &
                     is.na(all_mapped$Genus) &
                     all_mapped$Species == "sp3" &
                     all_mapped$Description == "OTU-13"] <- "sp13"

abnormal3 <- inner_join(all_mapped,my_list[[33]])
unique(abnormal3$Description) ## done


## again check
combinations <- unique(all_mapped[,c('Family','Genus','Species')])

my_list <- list()  

for (i in (1:nrow(combinations))){
  condition <- as.data.frame(combinations[i,])
  test <- inner_join(all_mapped,condition)
  check <- length(unique(test$Description))
  if (check >= 2){
    my_list[[i]] <- condition
  }
}
my_list

## check
abnormal <- inner_join(all_mapped,my_list[[40]])
unique(abnormal$Description)
## fine
## done

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
write.csv(all_aggre,file="./HCKC_macroinverts_XK.csv")

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




