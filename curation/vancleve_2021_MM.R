
# Curation Script ---------------------------------------------------------

# Dataset: Bonanza Creek LTER: Tree Inventory Data from 1989 to present at Core research sites in Interior Alaska, Bonanza Creek LTER
# Location:  Bonanza Creek, Alaska
# Curator: Mike McWilliam
# Date: 14 / 7 / 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

dt<- readRDS("vancleve_2021_Original/rdata.rds")
dt <- subset(dt, select=-NOTE)
head(dt)
nrow(dt)

dt$SITE <-  toupper(dt$SITE) # all should be caps
dt$SPECIES <-  toupper(dt$SPECIES) 
dt$SPECIES <- gsub(" ", "", dt$SPECIES) 

# Primary field check -----------------------------------------------------

# biomass = diameter at breast height
dt$Biomass <- dt$DBH

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Biomass) # check the minimum
dt <- subset(dt, Biomass > 0) # no zeros. get rid of -9999 (dead)
min(dt$Biomass)
sum(dt$Biomass=="") # no blanks 

# count = as it's individual data
dt$Abundance <- 1

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct

dt$Year <- format(as.Date(dt$DATE, "%Y-%m-%d"), "%Y")
dt$Month <- format(as.Date(dt$DATE, "%Y-%m-%d"), "%m")
dt$Day <- format(as.Date(dt$DATE, "%Y-%m-%d"), "%d")
head(dt)

# some sites have 1 year only
siteyears <- table(dt[,c("SITE", "Year")])
siteyears[siteyears > 0] <- 1
Nyears <- rowSums(siteyears)
names(Nyears[Nyears==1])
dt <- dt[!dt$SITE %in% names(Nyears[Nyears==1]), ]
unique(dt$DATE)

# --------------------- try aggregate
# aggregate from individual to species-level data
# sum of abundance and mean of DBH
#agg.cols <- c("DATE", "SITE", "PLOT", "SPECIES")
#agg1 <- aggregate(Abundance~., dt[,c(agg.cols, "Abundance")], sum) 
#agg2 <- aggregate(Biomass~., dt[,c(agg.cols, "Biomass")], mean)
#dtX <- data.frame(merge(agg1, agg2))
#head(dtX, 20)

##dtX$gp <- paste(dtX$PLOT, dtX$SPECIES)
#ggplot(data=dtX[dtX$SITE%in%unique(dtX$SITE)[2:10],], aes(x=DATE, y=Biomass))+
#geom_line(aes(group=gp, col=SPECIES))+facet_wrap(~SITE)
#ggplot(data=dtX[dtX$SITE%in%unique(dtX$SITE)[2:10],], aes(x=DATE, y=Abundance))+
#geom_line(aes(group=gp, col=SPECIES))+facet_wrap(~SITE)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

# lat/long from metadata
coords <- read.table("vancleve_2021_Original/coords.txt")
coords$type <- rep(c("name", "long", "long2", "lat", "lat2"), nrow(coords)/5)
head(coords)
latlong <- data.frame(site=coords$V1[coords$type=="name"], lat=coords$V1[coords$type=="lat"], long=coords$V1[coords$type=="long"])
dt$Latitude <- as.numeric(latlong$lat[match(dt$SITE, latlong$site)])
dt$Longitude <- as.numeric(latlong$long[match(dt$SITE, latlong$site)])

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 20
lw_ratio <- 1

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_point(data=points, aes(x=Longitude, y=Latitude), col="red", shape=21)+
  #coord_fixed()+ 
coord_cartesian(c(mid[2]-a, mid[2]+a), c(mid[1]-a*lw_ratio, mid[1]+a*lw_ratio))+
  labs(x='Longitude', y='Latitude')+
  theme_bw() + theme(panel.grid=element_blank(), aspect.ratio=lw_ratio)
world

# Taxonomic fields ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

unique(dt$SPECIES)
dt$spp <- ifelse(dt$SPECIES=="PICMAR", "Picea mariana", 
ifelse(dt$SPECIES=="LARLAR", "Larix laricina",
ifelse(dt$SPECIES=="BETNEO", "Betula neoalaskana", 
ifelse(dt$SPECIES=="POPTRE", "Populus tremuloides", 
ifelse(dt$SPECIES=="PICGLA", "Picea glauca",
ifelse(dt$SPECIES=="POPBAL", "Populus balsamifera",
ifelse(dt$SPECIES=="PICEA", "Picea spp.", 
ifelse(dt$SPECIES=="?", "Unknown sp", 
ifelse(dt$SPECIES=="ND", "Unknown sp", 
ifelse(dt$SPECIES=="", "Unknown sp", 
ifelse(dt$SPECIES=="NOTREES",NA, NA)))))))))))
unique(dt$spp)
head(dt)

dt$Genus <- word(dt$spp, 1)
dt$Species <- word(dt$spp, 2)
dt$Family <- ""

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$spp))
sort(unique(dt$Species))
sort(unique(dt$Genus))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

dt$DepthElevation <- "365m"

head(dt)
dt$Plot <- paste(dt$SITE, dt$PLOT, sep="_")
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

merge_cols <- c("Family", "Genus", "Species", "spp","DATE","Year","Month", "Day", "Plot","Latitude", "Longitude", "SITE", "DepthElevation", "StudyID")

head(dt[,c("Abundance", merge_cols)])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Abundance", merge_cols)], sum)
dt_biomass <- aggregate(Biomass ~ ., data = dt[,c("Biomass", merge_cols)], sum)
dt_merged$x <- apply( dt_merged[ , merge_cols ] , 1 , paste , collapse = "-" )
dt_biomass$x <- apply( dt_biomass[ , merge_cols ] , 1 , paste , collapse = "-" )
dt_merged$Biomass <- dt_biomass$Biomass[match( dt_merged$x, dt_biomass$x )]
dim(dt)[1]-dim(dt_merged)[1] # any change after aggregating 

table(unique(dt[,c("SITE", "Year")])$SITE) # many sites with only 2 years
plt <- c("FP1C",  "FP2A" , "FP2B" , "FP2C" , "FP3A" , "FP3B"  ,"FP3C" , "FP4A"  ,"FP4B" , "FP4C",  "FP5A")

dt_merged$gp <- paste(dt_merged$Plot, dt_merged$spp)
ggplot(data=dt_merged[dt_merged$SITE %in% plt,], aes(x=DATE, y=Biomass))+
geom_line(aes(group=gp, col=spp))+facet_wrap(~SITE)

ggplot(data=dt_merged[dt_merged$SITE %in% plt,], aes(x=DATE, y=Abundance))+
geom_line(aes(group=gp, col=spp))+facet_wrap(~SITE, scale="free_y")

ggplot(dt_merged[dt_merged$spp %in% unique(dt_merged$spp),], aes(x=Abundance, y=Biomass))+geom_point(aes(col=spp))+scale_y_log10()+scale_x_log10()

dataset.name <- 'vancleve_2021'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Month, Day, Plot, sep='_')))
length(levels(dt_merged$SampleDescription)) # 54 samples

# reorder columns by BioTIME format
dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] 
                 
                         
head(dt_merged) # final check :)
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset.name, '_rawdata_MM.csv'), row.names=F)

# Convex Hull for centroid ------------------------------------------------

# load libraries
library(sf)
library("clipr")
library("dplyr")

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_merged %>% select(Longitude, Latitude) %>% distinct() %>%
   st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

# transform to mercator to get area in sq km
area <- st_transform(dt_coord, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
write_clip(area)

centroid 
area
