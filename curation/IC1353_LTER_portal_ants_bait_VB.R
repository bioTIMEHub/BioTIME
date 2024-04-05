### Dataset 4 - 1353 LTER portal ants bait
# comment: 1445 is not included in biotime as methods in the source suggest a lot of non constant sampling effort
# (link: https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md)
### Insect change integration 


#Clear workspace
rm(list=ls())

##read data

dt_name <- "LTER portal ants bait"
dt_ID <- "1353"
Roel_insect_local_folder <- "~/Desktop/Biotime push Roel/"
setwd(paste0(Roel_insect_local_folder,"1353 and 1445 LTER portal ants/"))
dt_name <- "LTER_portal_ants_bait"


Plots <- read.csv(paste0(dt_ID, " Plots.csv")); dim(Plots)
names(Plots)
RawData<- read.csv(paste0(dt_ID, " Data.csv")); dim(RawData)
names(RawData)
Studies<- read.csv(paste0(dt_ID, " Study details.csv")); dim(Studies)
names(Studies)
Samples<- read.csv(paste0(dt_ID, " Samples.csv")); dim(Studies)
names(Samples)
taxa<-read.csv(paste0(dt_ID, " Taxa.csv")); dim(taxa)
head(taxa)

# merge data and taxa
merge1<-merge(RawData, taxa, by = "Taxon")
dim(merge1)

# merge sample
merge2<-(merge(merge1, Samples, by = "Sample_ID"))
dim(merge2) # all there.
names(merge2)

# merge with plot
merge3<- merge(merge2, Plots , by = c("Plot_ID", "DataSource_ID") )#
dim(merge3) # all there

# merge with studies
merge4<- merge(merge3, Studies, by = "DataSource_ID")
names(merge4)[order(names(merge4))]
dim(merge4)

dt <- merge4


## curation and checks for BioTIME
require(tidyverse)
require(readxl)
require(maps)

# are there 2 or more unique years in the dataset?
n_distinct(dt$Year) >= 2
summary(dt)

#get info fo metadata
names(dt)
dt$ExperimentalManipulation
dt$Biomass
dt$Date
dt$Family
dt$Year
dt$SampleArea
sum(!is.na(dt$SampleArea))
dt$SamplingMethod
dt$SamplingMethodDetailed
dt$AggregationOfReplicates
dt$Elevation
dt$SourceGeogrData
dt$Realm
dt$NationState
dt$ClimateZone

table(dt$PlotName)
table(dt$Plot_ID) #Plot_ID is fine

#keep only relevant info
dt<- dt[,c("Number","Biomass","Family", "validTaxon.x","Latitude", "Longitude", "Plot_ID", "Elevation","Year")]

colnames(dt) <- c("Abundance", "Biomass","Family","GenusSpecies","Latitude", "Longitude", "Plot","DepthElevation","Year")

is.numeric(dt$Abundance)
summary(dt)
is.factor(dt$Family) | is.character(dt$Family)
is.factor(dt$GenusSpecies) | is.character(dt$GenusSpecies)


### Fixes
# The year column here is numeric so convert it to factor
dt$Year <- as.factor(dt$Year)
# and characters for the taxonomic fields
dt$Family <- as.character(dt$Family)
#check again
summary(dt)

### Plot
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(-120,-100), ylim=c(20,45))
# seems right
detach('package:maps')

#secondary fields

### Cleaning and checking objectives #Thank you Roel for cleaning this!!

# - Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes.
# - Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.
# - Fit proper taxa level with the correct fields (`Family, Genus, Species`)
# - Remove non-organismal records (e.g. rock, sand) and incidental taxa records.
# - Check that genus column doesn't have family names (end in *-eae* or *-idae*) or species column has genus names.
# - Re-allocate taxonomic classifications into the appropriate column.
# - Check for duplication and misspellings in the species names  (e.g. *Puffnius sp, Puffinus sp*; unidentified/unknown; *Buenia jeffreysi / Buenia jeffreysii*; double spacing; several spaces, authors name & date, etc.). We're not validating or updating species, but we do quick searches in Google Scholar to figure out which spelling is correct.
#                                           - Subspecies and annotated species names should be disregarded (e.g. *Amphiura (acrocnida) brachiate* should be considered as *Acrocnida brachiate*)
#                                           - If a record only has data until the genus level, it must have "sp" in species column.
#                                           
### Nomenclature

#**Uncertain or unidentified IDs**: "Genus sp" (without the full stop/period). If the dataset distinguishes between different uncertain IDs, we reformat them while making sure they're recognised as separate species: like "Genus sp1" and "Genus sp2"  
#**No subspecies or varieties**: Lowest level = "Genus species"  
#**Species groups**: Some taxa are less resolved and are placed into species complexes, e.g. *Dascyllus trimaculatus agg.* or *Rubus fruticosus agg.*. We denote this with "agg." to make sure it's not confused with other specific epithets. Uncertain IDs with several potential options do not count.  

#   *Reminder*: For `Species`, we only leave in species epithets! Don't put it into the usual *Genus species* convention.

unique(dt$Family) # check family
unique(dt$GenusSpecies)

dt$Genus <- word(dt$GenusSpecies, 1) # separate genus to its own column
dt$Species <- word(dt$GenusSpecies, start=2) # species to its own column

sum(is.na(dt$Genus))
sum(is.na(dt$Species))
sort(unique(dt$Genus))
sort(unique(dt$Species))

dt$Species[dt$Species == ""] <- "sp"

## Prepare for export
dt$Biomass <- rep('', nrow(dt))
dt$DepthElevation <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$Month <- rep(7,nrow(dt)) #always in July
dt$Day <- rep('', nrow(dt))

##aggegation check

# aggregate abundance records that are same species, plot, site, year.
dt_merged <- dt %>% group_by(Genus, Species, Plot, Year) %>%
  summarise(Abundance = sum(Abundance)) %>% ungroup() %>% arrange(Year)
dt <- dt %>% group_by(Biomass,Family,Genus, Species,
                      Plot,Latitude, Longitude,DepthElevation,
                      Day, Month,Year,StudyID) %>% arrange(Year, Family, Genus, Species)

dim(dt)[1] - dim(dt_merged)[1] #ok

##some checks
table(table(dt_merged$Abundance))
table(table(dt$Abundance))

# save the dataset name as an object so we save some typing
dataset_name <- paste0("IC",dt_ID,"_",dt_name)

# put in as many non-blank fields unique to the sampling event
# If plot, depthelevation, month, and day are available, I'd add those in too
dt$SampleDescription <-as.factor(with(dt,paste(Plot, Year, sep = '_')))
length(levels(dt$SampleDescription)) # 38 sampling events
# Now we'll reorder the columns to the proper format:

dt <- dt[c('Abundance',
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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
head(dt) # final check!
summary(dt)


## Export and spreadsheet prep

#The raw data is ready to be exported!

write.csv(dt, paste0(dataset_name, '_rawdata_VB.csv'), row.names=F, na='')

#Excel to fill out the curation spreadsheet. 

# Central coordinates (`CentralLatitude, CentralLongitude`) and sampling area 
# can't be retrieved here as only one pair of coordinates is present
