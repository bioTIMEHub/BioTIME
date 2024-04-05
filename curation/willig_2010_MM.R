

# Curation Script ---------------------------------------------------------

# Dataset:  Willig & Presley (2022). Long-term trends in gastropod abundance and biodiversity: Disentangling effects of press versus pulse disturbances
# Location:  Luquillo Forest Dynamics Plot (LFDP), Puerto Rico
# Curator: Mike McWilliam
# Date: 03 - 07 - 2023

# Set up ------------------------------------------------------------------
rm(list=ls())
library("maps")
library("ggplot2")
library("reshape2")

raw <- read.csv("willig_2010_Original/LFDPSnailCaptures.csv")
head(raw)

# Put in long format 
dt<-melt(raw, id.vars=c("YEAR", "SEASON", "RUN", "POINT", "DATE", "COMMENTS"), value.name="Abundance", variable.name="species_code", na.rm=TRUE, factorsAsStrings=TRUE)
head(dt)
nrow(dt)

# Primary field check -----------------------------------------------------

dt$Abundance <- as.numeric(dt$Abundance)
dt$Biomass <- ""

# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum (no zeroes) 
dt <- subset(dt, Abundance > 0)
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct 

dt$DATE <- as.Date(dt$DATE, "%m/%d/%Y")
sort(unique(dt$DATE)) # one date is 2107 (typo?) switch to 2007
dt$DATE[dt$DATE=="2107-07-11"] <- "2007-07-11"

head(dt)
colnames(dt)[colnames(dt)=="YEAR"] <- 'Year' # just a rename
dt$Year <- as.factor(dt$Year)
colnames(dt)[colnames(dt)=="month"] <- 'Month' # just a rename
dt$Month <- format(as.Date(dt$DATE, format="%m/%d/%Y"),"%m")
dt$Day <- format(as.Date(dt$DATE, format="%m/%d/%Y"),"%d")
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

# one point from paper
dt$Latitude <- 18.316667
dt$Longitude <- -65.816667

# check whether the GPS coordinates match expectations
points <- unique(dt[,c("Latitude", "Longitude")])
mid <- c(points[1,1], points[1,2])
a <- 7
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

# match species with species list found at the data url
spp <- read.csv("willig_2010_Original/willig_spp.csv")
sort(unique(dt$species_code))
sort(unique(spp$code))

dt <- dt[!dt$species_code %in% c("TOTABU"),]
#dt <- dt[!dt$species_code %in% c("UNKNOWN"),] # keep? 
unique(dt$species_code)

spp$dat <- unique(dt$species_code)[match(spp$code, unique(dt$species_code))]

dt[,c("Family", "Genus", "Species")] <- spp[match(dt$species_code, spp$code), c("family", "genus", "species")]
head(dt)

dt[is.na(dt$Family),]

unique(dt$Family)

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Family))

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

dt$DepthElevation <- 400 # midpoint from paper

# 2 SEASONS 4 RUNS / 160 POINTS
dt$Plot <- dt$POINT
head(dt)
unique(dt$SEASON)
unique(dt$RUN)
length(unique(dt$POINT))


# Prepare raw data --------------------------------------------------------

dt$StudyID <- ""

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Family", "Genus", "Species","species_code", "Year","Month", "Day", "DATE","Plot", "Abundance", "Biomass","Latitude", "Longitude", "DepthElevation", "StudyID", "SEASON", "RUN")], sum)

dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? 

taxa_plot <- unique(dt$species_code)[c(4)]
plot_plot <-unique(dt_merged$Plot)[1]
dt_merged$DATE <- as.Date(dt_merged$DATE, "%m/%d/%Y")
unique(dt_merged$DATE)
max(dt$DATE)

to_plot <- dt_merged[dt_merged$species_code %in% taxa_plot & dt_merged$Plot %in% plot_plot,]
table(unique(to_plot[,c("DATE", "Year")])$Year)
sort(to_plot$DATE)

ggplot(to_plot, aes(x=as.Date(DATE), y=Abundance, group=species_code, col=species_code))+
geom_point()+geom_line()+
facet_wrap(~Plot)

dataset.name <- 'willig_2010'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, Month, Day, "Plot", Plot,  SEASON, RUN, sep='_')))
length(levels(dt_merged$SampleDescription)) 

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

