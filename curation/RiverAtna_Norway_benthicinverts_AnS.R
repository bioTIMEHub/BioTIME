# Norway (Benthic invertebrates) Final Script # 

# 1. Load the required packages

require(tidyverse)
require(readxl)
require(maps)

# 2. Read in data

# Since the different sites are on different sheets, read them in separately and then combine later

dt.1<- read_excel('Documents/bio/DataCuration-workshop/S097-S100.xlsx',sheet=1, skip=3, col_names=T, na='')
# View(dt.1)
str(dt.1)
dim(dt.1)
summary(dt.1)

dt.2<- read_excel('Documents/bio/DataCuration-workshop/S097-S100.xlsx',sheet=2, skip=3, col_names=T, na='')
# View(dt.2)
str(dt.2)
dim(dt.2)
summary(dt.2)

dt.3<- read_excel('Documents/bio/DataCuration-workshop/S097-S100.xlsx',sheet=3, skip=3, col_names=T, na='')
# View(dt.3)
str(dt.3)
dim(dt.3)
summary(dt.3)

dt.4<- read_excel('Documents/bio/DataCuration-workshop/S097-S100.xlsx',sheet=4, skip=3, col_names=T, na='')
# View(dt.4)
str(dt.4)
dim(dt.4)
summary(dt.4)

# We can now combine the sheets together 

rbind(dt.1, dt.2, dt.3, dt.4)
dt <- rbind(dt.1, dt.2, dt.3, dt.4)

# 3. Check data 

# Visual check of data shows:
#   Need to break up 'date' into day, year, month? or leave in POSIXct form? DONE
#   Taxon should be separated into genus and species 
#   Read paper and find out what a kick sample is (abundance/ biomass) -> I think it is abundance 
#   Taxon is in the character form, this is good 
#   Kick sample is numeric, this is good 

# Are there more than two distinct years? (yes)
separate(dt, "Sampling date", c("Year", "Month", "Day"), sep = "-")
dt$Year <- word(dt$`Sampling date`, sep = "-", start=1)
dt$Month <- word(dt$`Sampling date`, sep = "-", start=2)
dt$Day <- word(dt$`Sampling date`, sep = "-", start=3)
dt$`Sampling date` <- NULL

n_distinct(dt$Year) >= 2

# Check Year, month and day -> all say FALSE
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change them to factors 
dt$Year <- as.factor(dt$Year) 
dt$Month <- as.factor(dt$Month)
dt$Day <- as.factor(dt$Day)

# Check them again -> now says TRUE
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change Kick Sample to abundance and check is numeric TRUE 
colnames(dt)[3] = "Abundance"
is.numeric(dt$Abundance)

# We can also re-name 'site' to 'Plot' so it is the same as BioTime 
colnames(dt)[1] = "Plot"

# Now test Taxon
is.factor(dt$Taxon) | is.integer(dt$Taxon)

# Change it to a factor
dt$Taxon <- as.factor(dt$Taxon)

# Check it again --> now says TRUE 
is.factor(dt$Taxon) | is.integer(dt$Taxon)

# Check data again -> looks good 
str(dt)

#     4. Add in the extra information

# Add in a new column for the altitude (but we can name it DepthElevation to match Biotime)

dt.1$DepthElevation = rep(c('1120'), each = 1)
dt.2$DepthElevation = rep(c('1020'), each = 1)
dt.3$DepthElevation = rep(c('710'), each = 1)
dt.4$DepthElevation = rep(c('375'), each = 1)

# Check co-ordinates and add latitude and longitude 

dt.1$Latitude <- as.numeric(rep('61.982861', nrow(dt.1)))
dt.1$Longitude <- as.numeric(rep('9.803406', nrow(dt.1)))

dt.2$Latitude <- as.numeric(rep('61.996916', nrow(dt.2)))
dt.2$Longitude <- as.numeric(rep('9.809092', nrow(dt.2)))

dt.3$Latitude <- as.numeric(rep('61.983785', nrow(dt.3)))
dt.3$Longitude <- as.numeric(rep('10.028318', nrow(dt.3)))

dt.4$Latitude <- as.numeric(rep('61.74542', nrow(dt.4)))
dt.4$Longitude <- as.numeric(rep('10.746716', nrow(dt.4)))

# Merge the data back together 
rbind(dt.1, dt.2, dt.3, dt.4)
dt <- rbind(dt.1, dt.2, dt.3, dt.4)

# Notes, when merging the data back together, you need to go back and fix the sampling data again 
# Just re-reun lines 52-92

# Check whether the GPS coordinates match expectations --> They do! 
world_map <- map_data('world') 
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt, aes(x=Longitude, y=Latitude), shape=21)
points + coord_fixed(xlim=c(5,15), ylim=c(57,67))
detach('package:maps')

# We need to re-run the first half of the code since we have re-combined the data tables (see weekly log for explanation)

# Are there more than two distinct years? (yes)
separate(dt, "Sampling date", c("Year", "Month", "Day"), sep = "-")
dt$Year <- word(dt$`Sampling date`, sep = "-", start=1)
dt$Month <- word(dt$`Sampling date`, sep = "-", start=2)
dt$Day <- word(dt$`Sampling date`, sep = "-", start=3)
dt$`Sampling date` <- NULL

n_distinct(dt$Year) >= 2

# Check Year, month and day -> all say FALSE
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change them to factors 
dt$Year <- as.factor(dt$Year) 
dt$Month <- as.factor(dt$Month)
dt$Day <- as.factor(dt$Day)

# Check them again -> now says TRUE
is.factor(dt$Year) | is.integer(dt$Year)
is.factor(dt$Month) | is.integer(dt$Month)
is.factor(dt$Day) | is.integer(dt$Day)

# Change Kick Sample to abundance and check is numeric TRUE 
colnames(dt)[3] = "Abundance"
is.numeric(dt$Abundance)

# We can also re-name 'site' to 'Plot' so it is the same as BioTime 
colnames(dt)[1] = "Plot"

# Now test Taxon
is.factor(dt$Taxon) | is.integer(dt$Taxon)

# Change it to a factor
dt$Taxon <- as.factor(dt$Taxon)

# Check it again --> now says TRUE 
is.factor(dt$Taxon) | is.integer(dt$Taxon)

# Check data again -> looks good 
str(dt)

# 5. Check for errors in the data 

# Make a new column for the species 
dt$Species <- dt$Taxon

# Check than none end in the family suffix 
str_which(dt$Species, 'idae$|eae$')

# We need to remove these because we need them sampled down to the species level 
delete <- c(str_which(dt$Species, 'idae$|eae$'))
delete
dt_new<- dt[-delete, ]
dt <- dt_new

# Now to separate the genus and the species 
dt$Genus <- word(dt$Species, 1)
dt$Species <- word(dt$Species, start=2)
dt$Taxon <- NULL

# check the species list for misspellings or non-BioTIME taxonomic convention names
# Check species
sort(unique(word(dt$Species, start=2, end=-1)))
# Check genera
sort(unique(word(dt$Species, 1)))
# Check combined
sort(unique(dt$Species))

# Looking at the data, here are some problems:
#   A. muliebris/hispida 
#   Acari
#   Agapetus spp.
#   Apatania spp.
#   Athripsodes sp.
#   Capnia sp.
#   Ceratopogonidae
#   Chironomidae
#   Coleoptera
#   Diptera
#   Diptera sp.
#   Dytiscidae
#   Glossosoma spp.
#   Hydraenidae
#   Hydropsyche spp.
#   Hydroptila spp.
#   Isoperla spp.
#   Leptoceridae
#   Leuctra fusca/digitata
#   Limnephilidae
#   Limnephilus sp.
#   Micrasema spp.
#   Nematomorpha (Gordius)
#   Oligochaeta
#   Pericoma spp.
#   Platyhelminthes
#   Potamophylax spp.
#   Psychodidae
#   Sialis sp.
#   Simuliidae
#   Tipulidae

# I have created a replacement vector to fix the problems

initial_replace <- c(
  "Diptera" = "Diptera sp.",
  "Diptera sp" = "Diptera sp.",
  "A. muliebris/hispida" = "Apatania muliebris",
  "Acari" = "NA",
  "Agapetus spp." = "Agapetus sp.",
  "Apatania spp." = "Apatania sp.",
  "Ceratopogonida" = "NA",
  "Chironomidae" = "NA",
  "Coleoptera" = "NA",
  "Dytiscidae" = "NA",
  "Glossosoma spp." = " Glossosoma sp.",
  "Hydraenidae" = "NA",
  "Hydropsyche spp." = "Hydropsyche sp.",
  "Hydroptila spp." = "Hydroptila sp.",
  "Isoperla spp." = "Isoperla sp.",
  "Leptoceridae" = "NA",
  "Leuctra fusca/digitata" = "Leuctra fusca",
  "Limnephilidae" = "NA",
  "Micrasema spp" = "Micrasema sp.",
  "Nematomorpha (Gordius)" = "Gordius sp.",
  "Oligochaeta" = "NA",
  "Pericoma spp." = "Pericoma sp.",
  "Platyhelminthes" = "NA",
  "Potamophylax spp." = "Potamophylax sp.",
  "Psychodidae" = "NA",
  "Simuliidae" = "NA",
  "Tipulidae" = "NA")

dt$Species <- str_replace_all(dt$Species, initial_replace)
dt <- dt[!is.na(dt$Species),]

# STEP 5: Prepare curated data for export

# Add in BioTime variables that are currently missing from the corrected datafile
dt$Biomass <- rep('', nrow(dt))
dt$StudyID <- rep('', nrow(dt))
dt$Family <- rep('', nrow(dt))

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

# no change in aggregating
dim(dt)[1]-dim(dt_merged)[1]

# save the dataset name as an object so we save some typing
dataset_name <- 'Norway_BenthicInvertebrates_AS'

# Generate a sample description. For this put in as many non-blank fields unique to the sampling event from which the abundance estimate was obtained.
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name, Latitude, Longitude, Day, Month, Year, DepthElevation, sep='_')))

# how many sampling events? (218)
length(levels(dt_merged$SampleDescription))

# Now reorder the columns to the proper BioTime format:
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
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)

# Final check
head(dt_merged) 

# STEP 6: Export curated data and prepare spreadsheet

setwd(dirname("Documents"))
write.csv(dt_merged, paste0(getwd(), '/', dataset_name, '_rawdata_AS.csv'), row.names=F, na='')


# code for calculating geographical features of the data:

# load libraries
library(sp)
library(rgeos)

# Import the raw data file - has latitude & longitude

##1. Convert data points into point spatial object
points <- SpatialPoints(cbind(dt$Longitude,dt$Latitude))

##2. Calculate convex hull, area and centroid
convhull<-gConvexHull(points)
gArea(convhull)  ##get area

# 0.02871542 = Area 

###get centroid
centroid<-gCentroid(convhull)    

##to get the coordinates
centroid@coords

# lat = 61.90863
# lon = 10.17564

############to plot
plot(points,cex=0.5,axes=F,pch=19, xlab="Longitude", ylab="Latitude")
axis(side=2,las=1,cex.axis=0.8, tck=-.01,line=-0.2)
axis(side=1,tck=-.01,labels=NA) 
axis(side=1,lwd = 0, line = -0.6,cex.axis=.8, labels=T) 

##add the convex hull
plot(convhull,add=T)

##add the centroid
points(centroid,col="Red",pch=19,cex=1.2)

## looks right!

################################# end of code ##################################





