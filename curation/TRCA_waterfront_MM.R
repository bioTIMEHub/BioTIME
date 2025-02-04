
# Curation Script ---------------------------------------------------------

# Dataset: TRCA Waterfront Fish Data 2000 - 2018
# Location:  Canada
# Curator: Mike McWilliam
# Date: 17 / 7 / 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

dt <- read.csv("TRCA_waterfront_Original/trca-electrofishing-data-2000-2018-for-open-data.csv")
head(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- as.numeric(dt$No_Individuals)
dt$Biomass <- as.numeric(dt$Weight_g)

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum
dt <- subset(dt, Abundance > 0) # no zeros
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 

min(dt$Biomass) # check the minimum
dt <- subset(dt, Biomass > 0) # no zeros
min(dt$Biomass)
sum(dt$Biomass=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct

dt$Year <- dt$Year
dt$Month <- format(as.Date(dt$Date, "%d/%m/%Y"), "%m")
dt$Day <- format(as.Date(dt$Date, "%d/%m/%Y"), "%d")
head(dt)

table(unique(dt[,c("Date", "Hour")])$Date) # multiple surveys per day.

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 10
lw_ratio <- 1

world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_point(data=points, aes(x=Longitude, y=Latitude), col="red", shape=21)+
coord_cartesian(c(mid[2]-a, mid[2]+a), c(mid[1]-a*lw_ratio, mid[1]+a*lw_ratio))+
  labs(x='Longitude', y='Latitude')+
  theme_bw() + theme(panel.grid=element_blank(), aspect.ratio=lw_ratio)
world

# Taxonomic fields ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)

head(dt)
dt$Taxon <- dt$Species

# ----------- skip this section / use file below -----------#
library("taxize")
sci_names <- comm2sci(unique(dt$Taxon))
ref <- as.data.frame(do.call("rbind", sci_names))
name_ref <- data.frame(spp = unique(dt$Taxon))
name_ref$new <- ref$V1[match(name_ref$spp, rownames(ref))]
name_ref
# add the rest manually. 
name_ref$fill <- name_ref$new
name_ref$fill[name_ref$spp=="Brown Trout"] <- "Salmo trutta"
name_ref$fill[name_ref$spp=="Threespine Stickleback"] <- "Gasterosteus aculeatus"
name_ref$fill[name_ref$spp=="Rock Bass"] <- "Ambloplites rupestris"
name_ref$fill[name_ref$spp=="Trout-perch"] <- "Percopsis omiscomaycus"
name_ref$fill[name_ref$spp=="Northern Pearl Dace"] <- "Margariscus nachtriebi"
name_ref$fill[name_ref$spp=="Johnny/Tesselated Darter"] <- "Etheostoma olmstedi"
name_ref$fill[name_ref$spp=="Quillback"] <- "Carpiodes cyprinus"
name_ref$fill[name_ref$spp=="Brook Stickleback"] <- "Culaea inconstans"
name_ref$spp[name_ref$spp=="Wiper"] <- "Striped Bass x White Bass hybrid"
name_ref$fill[name_ref$spp=="Unknown"] <- "remove"

name_ref[grepl(" sp.", name_ref$spp),"fill"] <- name_ref[grepl(" sp.", name_ref$spp),"spp"]
name_ref[grepl(" x ", name_ref$spp),"fill"] <- "hybrid"
name_ref[grepl("idae", name_ref$spp),"fill"] <- "remove"
name_ref
#write.csv(name_ref, "TRCA_waterfront_Original/spp_names_mm.csv")
# ------------------------------------------------------### 

spp_names <- read.csv("TRCA_waterfront_Original/spp_names_mm.csv")
dt$TaxSci <- spp_names$fill[match(dt$Taxon, spp_names$spp)]
dt <- dt[!dt$TaxSci =="remove",]
dt <- dt[!dt$TaxSci =="hybrid",]
head(dt)

dt$Genus <- word(dt$TaxSci, 1)
dt$Species <- word(dt$TaxSci, 2)
dt$Family <- ""

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))

ggplot(dt[dt$Taxon=="Rock Bass",])+geom_point(aes(x=as.Date(Date, "%d/%m/%Y"), y=Abundance))


# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

head(dt)
unique(dt$Efishing_Sec)
table(dt$Efishing_Sec) # most are 1000 minutes. Subset? 
dt <- subset(dt, Efishing_Sec > 850 & Efishing_Sec < 1150)

dt$DepthElevation <- ""

head(dt) # Run_UID is unique site & time. 
unique(dt[,c("Site_ID", "Transect_Name")]) # Site/Transect the same
sites <- data.frame(site =unique(dt$Transect_Name), N= paste("Site", 1:length(unique(dt$Transect_Name)))) # Number for simplicity. 
dt$SiteN <- sites$N[match(dt$Transect_Name, sites$site)]

# hour is another replicate. 
dt$Plot <- dt$SiteN 
head(dt)

ggplot(data=unique(dt[,c("SiteN", "Hour")]))+geom_point(aes(x=Hour,y=SiteN))

dt$Habitat<-dt$Habitat_Type # important in the paper
habs <- unique(dt[,c("Plot","Habitat")])
table(habs$Plot) # 1 habitat per site.


# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

# sum biomass/count for each day/hour
# then find means for each day (multiple hours)

match_cols <- c("Family", "Genus", "Species","Year","Month", "Day", "Plot", "Date", "Taxon", "Latitude", "Longitude", "DepthElevation","StudyID")

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Abundance", "Hour",match_cols)], sum)
dt_merged <- aggregate(Abundance ~ ., data = dt_merged[,c("Abundance", match_cols)], mean)

dt_biomass <- aggregate(Biomass ~ ., data = dt[,c("Biomass", "Hour", match_cols)], sum)
dt_biomass <- aggregate(Biomass ~ ., data = dt_biomass[,c("Biomass", match_cols)], mean)

dt_merged$x <- apply( dt_merged[ , match_cols ] , 1 , paste , collapse = "-" )
dt_biomass$x <- apply( dt_biomass[ , match_cols ] , 1 , paste , collapse = "-" )
dt_merged$Biomass <- dt_biomass$Biomass[match( dt_merged$x, dt_biomass$x )]

head(dt_merged)

ggplot(dt_merged[dt_merged$Taxon%in% unique(dt_merged$Taxon)[1:15],], aes(x=Abundance, y=Biomass, colour=Taxon))+
geom_point()+scale_y_log10()+scale_x_log10()#+facet_wrap(~Taxon)

taxa_plot <- unique(dt$Taxon)[5]
plot_plot <-unique(dt_merged$Plot)[21:50]
ggplot(dt_merged[dt_merged$Taxon %in% taxa_plot & dt_merged$Plot %in% plot_plot,], aes(x=as.Date(Date, "%d/%m/%Y"), y=Abundance, group=Plot, col=Taxon))+
geom_point()+geom_line()+
facet_wrap(~Plot)

dim(dt)[1]-dim(dt_merged)[1] # any change after aggregating 

# re-add habitat
dt_merged$Habitat <- habs$Habitat[match(dt_merged$Plot, habs$Plot)]

dataset.name <- 'TRCA_waterfront'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Month, Day,  Plot, Habitat, sep='_')))
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