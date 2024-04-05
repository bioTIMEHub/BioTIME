
# Curation Script ---------------------------------------------------------

# Dataset: Sperandii et al. (2019). Back into the past: Resurveying random plots to track community changes in Italian coastal dunes.
# Location: Lazio Province Sand Dunes, Central Italy
# Curator: Mike McWilliam
# Date: 03 - 07 - 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

raw <- readRDS("sperandii_2020_Original/rdata.rds")
head(raw)

# Put in long format 
dt<-melt(raw, id.vars=c("Plot", "Time", "Habitat_code"), value.name="Biomass", variable.name="species_code", na.rm=TRUE, factorsAsStrings=TRUE)
head(dt)
nrow(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- ""
dt$Biomass <- as.numeric(dt$Biomass)

# No negative values, zeroes, or NAs in Biomass/biomass fields.
min(dt$Biomass) # check the minimum (no zeroes) 
dt <- subset(dt, Biomass > 0)
min(dt$Biomass)
sum(dt$Biomass=="") # no blanks 


############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct 

dt$Year <- ifelse(dt$Time==0, 2004, ifelse(dt$Time==1, 2017, NA)) # midpoints used
dt$Month <- ""
dt$Day <- ""
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

# Manually input centroid coordinates from metadata
dt$Latitude <- 41.6336
dt$Longitude <- 12.3858

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(41.6336, 12.3858 )
a <- 10
lw_ratio <- 0.75

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

head(dt)

dt$Family <- ""
dt$Genus <- word(dt$species_code, 1)
dt$Species <- word(dt$species_code, 2)
head(dt)

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Family))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc are factors/integers? 
# elevation or depth will normally be numeric unless in a treatment format. 

dt$DepthElevation <- ""
dt$Plot <- gsub("rev", "", dt$Plot) # remove "rev" 
head(dt)

# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

# aggregate Biomass records that are same species, plot, and survey day.
dt_merged <- aggregate(Biomass ~ ., data = dt[,c("Family", "Genus", "Species","Year","Month", "Day", "Plot", "Biomass","Habitat_code", "Abundance","Latitude", "species_code", "Longitude", "DepthElevation", "StudyID")], sum)

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? yep. 4000 rows lost
# Check DAY ! 

taxa_plot <- unique(dt$species_code)[1:20]
plot_plot <-unique(dt_merged$Plot)[1:10]
ggplot(dt_merged[dt_merged$species_code %in% taxa_plot & dt_merged$Plot %in% plot_plot,], aes(x=Year, y=Biomass, group=species_code, col=species_code))+
geom_point()+geom_line()+
facet_wrap(~Plot)

dataset.name <- 'sperandii_2020'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year,  Plot, Habitat_code, sep='_')))
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
                         
head(dt_merged) # final check
str(dt_merged)

# Export final ------------------------------------------------------------

write.csv(dt_merged, paste0(dataset.name, '_rawdata_MM.csv'), row.names=F)


