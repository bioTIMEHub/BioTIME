
# Curation Script ---------------------------------------------------------

# Dataset: Werner et al. (2014) Cross-Scale Interactions and the Distribution-Abundance Relationship 
# Location:  ES George Reserve, MI USA
# Curator: Mike McWilliam
# Date: 14 / 7 / 2023

# Set up ------------------------------------------------------------------
rm(list=ls()) 
library("maps")
library("ggplot2")
library("reshape2")
library("stringr")

#raw1<- readRDS("ddata.rds")
#head(raw1, 50)
raw <- read.csv("werner_2014_Original/dryad_data.csv")
head(raw, 10)
nrow(raw)

# Put in long format 
dt<-melt(raw, id.vars=c("year"), value.name="Abundance", variable.name="sample", na.rm=TRUE, factorsAsStrings=TRUE)
head(dt)
nrow(dt)

occ <- dt[grepl("occupancy", dt$sample),] # remove occupancy data
dt <- dt[grepl("density", dt$sample),] 

# Primary field check -----------------------------------------------------

dt$Biomass <- ""
dt$Abundance <- as.numeric(dt$Abundance)


# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance) # check the minimum
dt <- subset(dt, Abundance>0)
min(dt$Abundance)
sum(dt$Abundance=="") # no blanks 

############## YEAR MONTH DAY (int/factors)
# no negative values, 0s or NAs, all are logical
# Year < 2023, month < 12, day < 31
# Date should be POSIXct

dt$Year <- dt$year
dt$Month <- ""
dt$Day <- ""
head(dt)

############## LAT LONG (numeric)
# Latitude -90 to 90 / Longitude -180 to 180,  no blanks, no NAs

dt$Latitude <- 42.454274
dt$Longitude <- -84.011964
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

# Rana catesbeiana (Rca),  

dt$taxon <- gsub(".mean.density", "", dt$sample)
head(dt)



unique(dt$taxon)
dt$spp <- ifelse(dt$taxon=="BAM", "Bufo americanus", 
ifelse(dt$taxon=="HVE", "Hyla versicolor",
ifelse(dt$taxon=="PCR", "Pseudacris crucifer", 
ifelse(dt$taxon=="PTR", "Pseudacris triseriata", 
ifelse(dt$taxon=="RCL", "Rana clamitans",
ifelse(dt$taxon=="RPA", "Rana palustris",
ifelse(dt$taxon=="RPI", "Rana pipiens", 
ifelse(dt$taxon=="RSY", "Rana sylvatica", 
ifelse(dt$taxon=="ALA", "Ambystoma laterale", 
ifelse(dt$taxon=="ATI", "Ambystoma tigrinum", 
ifelse(dt$taxon=="AMA", "Ambystoma maculatum", 
ifelse(dt$taxon=="NVI", "Notophthalmus viridescens ", 
ifelse(dt$taxon=="HSC", "Hemidactylium scutatum",  dt$taxon)))))))))))))
unique(dt$spp)
head(dt)


dt$Genus <- word(dt$spp, 1)
dt$Species <- word(dt$spp, 2)
dt$Family <- ""

# check the species list for misspellings or non-BioTIME conventions
sort(unique(dt$Species))
sort(unique(dt$Genus))
head(dt, 100)

# Secondary fields ---------------------------------------------------

# trawl, plot, transect etc / # elevation or depth 

dt$DepthElevation <- ""

dt$Plot <- "" # pond-level data not public

# Prepare raw data --------------------------------------------------------

dt$StudyID <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- aggregate(Abundance ~ ., data = dt[,c("Family", "Genus", "Species","taxon","Year","Month", "Day", "Plot",  "Abundance", "Biomass","Latitude", "Longitude", "DepthElevation", "StudyID")], sum)

ggplot(dt_merged, aes(Year, Abundance))+geom_line(aes(colour=taxon))+facet_wrap(~Plot)

dim(dt)[1]-dim(dt_merged)[1] # any change after aggregating 

dataset.name <- 'werner_2014'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(Latitude, Longitude, Year, sep='_')))
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
