#Curate the Muschet 2017 dataset:
#Cottonwood Lake Study Area - Invertebrate Counts 1992 - 2015

#NB there is a version 2 of this dataset which continues the data up until 2021.
#So, I have curated the longer, more recent version of the dataset - but this 
#folder is still called 'muschet_2017' so I can keep track of which BioTIME 
#data curation task this corresponds to.

setwd("C:/Users/apenny3/OneDrive - University of Edinburgh/PapersInPrep/BioTIME_2.0/BioTIME hack/muschet_2017")
rm(list=ls())

library(tidyverse)
library(readxl)
library(maps)

#This data came from https://www.sciencebase.gov/catalog/item/624c779ad34e21f82764df2e

#The occurrence records/data
ds <- read.table("muschet_2017_CLSAInvertebrateCounts1992_2021.csv", header = TRUE, sep = ",")
#View(ds)

## CHECKING DATASET CRITERIA
#Initial checks that the dataset meets data criteria for BioTIME.

#CRITERION 1: Dataset contains at least 2 years of data
length(unique(ds$YEAR)) >= 2 #TRUE

#CRITERION 2: Dataset consists of entire assemblages, not just populations; i.e.
#does not exclude some taxa intentionally.
#The purpose of the study is listed as 'to identify seasonal and interannual
#changes in macroinvertebrate community composition'- so it's definitely assemblage
#data

#CRITERION 3: Data should record abundance, biomass or both.
#This study presents count data, so it fulfils this condition.

#CRITERION 4: Sampling methods and effort are consistent through time
#Aquatic macroinvertebrates were collected each month (April-September) from all 
#wetlands at Cottonwood Lake Study Area containing water using vertically oriented 
#funnel traps (Swanson 1978). Sampling was stratified to provide separate estimates 
#of invertebrate biomass and abundance in all major vegetative zones of each wetland.  
#Samples were collected at random locations along the 3 established transects in 
#each wetland that were established earlier and used to collect other biotic and 
#abiotic data (LaBaugh et al. 1987).  The length of each vegetation zone as bisected 
#by transects was measured and a computer-generated set of random numbers used to 
#identify sample points for the collection of invertebrate samples in each vegetative 
#zone.  One sample was collected from each major vegetative zone from each transect. 
#Data consist of counts by taxa.

#NB we need to check that sampling was even here. Not all wetlands had all three
#transects sampled each time, so we need to filter out those which  has less than
#3 transects sampled.

#CRITERION 5: Individuals are identified to species level where possible
#Metadata says that 'Taxa were identified to the finest resolution possible' - 
#so this dataset conforms.

#####
###TRANSLATING TAXONOMIC CODES TO TAXONOMIC NAMES
#The taxonomic keys to the taxon codes. These are available in the metadata.

#The XML with metadata in it - not sure how to open this with formatting but
# clicking the 'View' button in the 'Attached files' section of this webpage:
#https://www.sciencebase.gov/catalog/item/624c779ad34e21f82764df2e allows us to
#view the file here: 
#https://www.sciencebase.gov/catalog/file/get/624c779ad34e21f82764df2e?f=__disk__55%2F7a%2F07%2F557a07ac2ff7eef1b8e495e14fa3679a7ea8f63f&transform=1&allowOpen=true

#Make a key to these codes using Alban's script

# We are missing
#the metadata which links the codes to the taxonomic names (i.e. don't know what
#the codes mean)

#However, we can read in Alban's taxonomy key from version 1 of the dataset and
#assume the names haven't changed. 

tax_v1 <- read_csv(file = "./muschet_2017_CLSAInvertebrateCounts_metadata_taxonomy.csv")

all(tax_v1$codes %in% colnames(ds)) #FALSE

#Which taxon codes are missing from this version?
tax_v1$codes[which(!tax_v1$codes %in% colnames(ds))] #"BRAN" "CLAD" "ELMI" "MOLA"

#I cross-checked with version 1 of this dataset, and each of these four taxa
#has only D, 0 and NA in their respective columns - perhaps this is why they have
#been removed from version 2.0.

#Are all the version 2.0 taxon codes present in the taxon list for version 1?
all(colnames(ds)[-c(1:5)] %in% tax_v1$codes) #FALSE

colnames(ds)[-c(1:5)][which(!colnames(ds)[-c(1:5)] %in% tax_v1$codes)] #CORIN

#The metadata translates CORIN as 'Corixidae nymph sp.' - so we could manually 
#add this to the meta_taxonomy.

tax_v2_codes <- c(tax_v1$codes, "CORIN")
tax_v2_longnames <- c(tax_v1$long_names, "Corixidae nymph sp.")

tax_v2 <- data.frame(codes = tax_v2_codes, long_names = tax_v2_longnames)
tax_v2 <- tax_v2[order(tax_v2$codes),]

#Save the updated taxonomy.
#write.table(tax_v2, file = "CLSAInvertebrateCounts_metadata_taxonomy_updated.csv")

tax_v2 <- read.table("CLSAInvertebrateCounts_metadata_taxonomy_updated.csv")

####
#OVERALL CHECKS
dim(ds)
summary(ds)

##ALBAN'S CODE (adapted for new object names and types)
# melting species ----
ds <- data.table::melt(ds,
                          id.vars = 1:5,
                          variable.name = "species",
                          na.rm = TRUE
)

###
#NB Get a warning above for using deprecated reshape2.

#Alban's script mentions filtering out any MONTH where 
#fewer than three transect were sampled - but how many months is this?

ds_checkmonths <- ds %>%
  group_by(YEAR, WETLAND, MONTH) %>%
  dplyr::summarise(n_transect = length(unique(TRANSECT)))

unique(ds_checkmonths$n_transect) #3, 1, 2

#Returns 3, 1, 2 - so if we are standardising sampling, we should do this at the
#level of transects.

#How much data do we lose if we filter out wetlands/months where fewer than 3 
#transects were measured?

hist(ds_checkmonths$n_transect)

#Most samples include 3 or more transects - so filter.

test <- ds %>%
  group_by(YEAR, WETLAND, MONTH) %>%
  mutate(n_transects = n_distinct(TRANSECT)) %>%
  ungroup() %>%
  filter(n_transects == 3)

unique(test$n_transects) #3, good

ds <- test

#We now have standardised effort

# when value is D, the pond was not sampled because it was dry
ds <- ds[which(!ds$value %in% c("0", "D", "")),]

####
#Check data formats
str(ds)

#Abundance or biomass needs to be numeric
is.numeric(ds$value) #FALSE
ds$value <- as.numeric(ds$value)

sum(ds$value)

#Coordinates need to be numeric
#These are in the separate document site_locations

#Need to adjust the plot names - Alban's code
coords <- read.csv("muschet_2017_site locations.csv", skip = 1)
coords$Plot_name <- ifelse(nchar(coords$Plot_name) == 2, paste0(substr(coords$Plot_name, 1, 1), "0", substr(coords$Plot_name, 2, 2)), coords$Plot_name)

#End

#Add the plot locations to the dataset.
ds <- left_join(ds, coords, by = join_by(WETLAND == Plot_name))

#####
#OVERALL CHECKS
#Check that all the columns have the right format
str(ds)

#Abundance or biomass needs to be numeric
is.numeric(ds$value) # TRUE
ds$value <- as.numeric(ds$value)

#Coordinates need to be numeric
is.numeric(ds$Latitude) #TRUE
is.numeric(ds$Longitude) #TRUE

#Dates POSIXct or YEAR, MONTH, DAY cols: Integer or factors
is.integer(ds$YEAR) #TRUE
is.integer(ds$MONTH) #TRUE

#Plot: factors or integers
ds$WETLAND <- as.factor(ds$WETLAND)

#DepthElevation - we don't have this

#Taxonomic: Characters or factors
is.factor(ds$species) #TRUE

#####
#PRIMARY FIELD CHECKS
#Primary fields are abundance, coordinates, and dates.

#Fields don’t contain blanks, zeroes, negative values, NAs, or NaN

#Abundance
min(ds$value) #0.1, no zeroes or negative value
length(which(is.na(ds$value))) #0, no NAs or NaN
length(which(ds$value == "")) #0
length(which(ds$value == " ")) #0

#Coordinates - No NAs or NaN
length(which(is.na(ds$Latitude))) #0
length(which(is.na(ds$Longitude))) #0

#Dates
length(which(is.na(ds$YEAR))) #0
length(which(is.na(ds$MONTH))) #0

#Also need to make sense
range(ds$MONTH) #4, 9
range(ds$YEAR) #1992, 2021

#I don't think the abundances need to be pooled again, for now. They aren't 
#separated by e.g. life stage.

#Plot the data to ensure the coordinates are as expected.
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=ds %>% distinct(Latitude, Longitude, YEAR), 
                             aes(x=Longitude, y=Latitude, fill = YEAR), shape=21)

points + coord_fixed(xlim=c(-140, -80), ylim=c(35,60))
detach('package:maps')

#This map looks correct (all points are in North Dakota)

#Try plotting a more zoomed-in map
points + coord_fixed(xlim=c(-99.15, -99.08), ylim=c(47.08,47.12)) 

#There is quite a range of colours here so I guess we don't have complete
#coverage over time. 

#####
#SECONDARY FIELDS

#Check plot - here WETLAND
sort(unique(ds$WETLAND)) #All makes sense.

#DepthElevation: We don't have this here.

####
#TAXONOMIC FIELDS
#First, convert the abbreviated taxon names to full names
ds_tax <- left_join(ds, tax_v2, by = join_by(species == codes))

#Rename fields, drop the codes
ds_tax <- ds_tax %>%
  select(-species) %>%
  dplyr::rename("taxon" = long_names)

#Eliminate blanks, NAs and zeroes
length(which(is.na(ds_tax$taxon))) #0
length(which(ds_tax$taxon == 0))
unique(ds_tax$taxon) 

#Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.
#Abbreviations eliminated.

#Check names comply with BioTIME syntax. 

#Copy the taxon column
ds_tax$copy_taxon <- ds_tax$taxon

#Do we have "sp."? If so, convert to "sp"
unique(grep(pattern = "sp.", x = ds_tax$copy_taxon, value = TRUE, fixed = TRUE)) #"Corixidae nymph sp." "Dasycorixa sp."   

#Replace "Corixidae nymph sp." with "Corixidae nymph sp"
ds_tax$copy_taxon[which(ds_tax$copy_taxon == "Corixidae nymph sp.")] <- "Corixidae nymph sp"

#Replace "Dasycorixa sp." with "Dasycorixa sp"
ds_tax$copy_taxon[which(ds_tax$copy_taxon == "Dasycorixa sp.")] <- "Dasycorixa sp"

#Does the dataset distinguish between different uncertain IDs?
#No- though it does include info like 'nymph' or 'immature' for some species.

#"immature Saldidae" is a problem; change to Saldidae
ds_tax$copy_taxon[which(ds_tax$copy_taxon == "immature Saldidae")] <- "Saldidae"

#No subspecies or varieties are allowed - only Genus and Species
#"Valvata lewisi morph ontariensis" is a problem taxon.
#"Corixidae nymph sp" is a problem too.

#Change "Valvata lewisi morph ontariensis" to "Valvata lewisi"
ds_tax$copy_taxon[which(ds_tax$copy_taxon == "Valvata lewisi morph ontariensis")] <- "Valvata lewisi"

#Change "Corixidae nymph sp" to "Corixidae"
ds_tax$copy_taxon[which(ds_tax$copy_taxon == "Corixidae nymph sp")] <- "Corixidae"

#Fit proper taxa level with the correct fields (Family, Genus, Species)

#We have Class and Subclass and Order-level IDs here too - the protocol is to put
#these in the Family column. BUT - we first need to identify which taxonomic rank
#these names belong to.
#library(taxize)

#Can taxize::classification detect e.g. Order, Class for us?
#ds_class_object <- classification(sci_id = unique(ds_tax$copy_taxon), db = "itis")

#This took so long (it got stuck on genus "Anax" that I decided to just manually
#make a list of all the taxonomic names at Family level or higher)

highlevel_taxa <- c("Aeshnidae", "Anostraca", "Belostomatidae", "Calanoida", "Ceratopogonidae",         
                    "Chironominae", "Coleoptera", "Laevicaudata", "Corixidae", "Curculionidae",            
                    "Cyclopoida", "Dixidae", "Dolichopodidae", "Dytiscidae","Entomobryidae",             
                    "Ephydridae", "Scirtidae", "Auchenorrhyncha", "Hydrachnidae",                  
                    "Isotomidae", "Lepidoptera", "Leptoceridae", "Libellulidae", "Limnephilidae",
                    "Mesoveliidae", "Nepidae", "Clitellata", "Orthocladiinae", "Ostracoda",
                    "Phryganeidae", "Poduridae", "Polycentropodidae", "Psychodidae", "Saldidae",                
                    "Sminthuridae", "Sphaeridiinae", "Staphylinidae", "Stratiomyidae", "Syrphidae",
                    "Tabanidae","Tanypodinae", "Tipulidae", "Zygoptera")  

#Creating Family column
ds_tax$Family <- rep('', nrow(ds_tax))

#Put the family-level IDs in the Family column
ds_tax$Family[which(ds_tax$copy_taxon %in% highlevel_taxa)] <- ds_tax$copy_taxon[which(ds_tax$copy_taxon %in% highlevel_taxa)]

#Visual check:
check <- ds_tax %>%
  select(copy_taxon, Family) %>%
  distinct()

rm(check)

#Visual check of the other names
#Split names in the copy_taxa column at spaces (" ") to make Genus and Species 
#columns, and then delete those genus and names which are already present in the 
#Family column.

ds_tax <- ds_tax %>%
  separate(col = copy_taxon, into = c("Genus", "Species"), sep = " ", remove = FALSE)

ds_tax$Species[which(is.na(ds_tax$Species))] <- ''

#We get a warning here because there are often not two fragments to split names into.

#Remove the GENUS names which are actually family names
ds_tax$Genus[which(ds_tax$Genus == ds_tax$Family)] <- ""

#Remove non-organismal or incidental records - we have none of these

#Check that Genus column doesn't have Family names and Species column doesn't have
#genus names

length(which(ds_tax$Genus == ds_tax$Family)) #0
str_which(ds_tax$Genus, 'idae$|eae$') #0
str_which(ds_tax$Species, 'idae$|eae$') #0

unique(ds_tax$Species)

#Check for duplication and misspellings in the species names (e.g. Puffnius sp, 
#Puffinus sp; unidentified/unknown; Buenia jeffreysi / Buenia jeffreysii; double 
#spacing; several spaces, authors name & date, etc.). We’re not validating or 
#updating species, but we do quick searches in Google Scholar to figure out which 
#spelling is correct.

#Quick searches done.
sort(unique(ds_tax$copy_taxon)) #No suspect spellings here

#Checking for "  " double spaces.
grep(pattern = "  ", x = unique(ds_tax$copy_taxon)) #0

#Subspecies and annotated species names should be disregarded (e.g. Amphiura 
#(acrocnida) brachiate should be considered as Acrocnida brachiate)
#None of these.

#If a record only has data until the genus level, it must have “sp” in species column.
#Add 'sp' to all the genus-level IDs in the dataset.
ds_tax$Species[which(!is.na(ds_tax$Genus) & is.na(ds_tax$Species))] <- "sp"

#Remove copy_taxon column as we no longer need it
ds_tax$copy_taxon <- NULL
ds_tax$taxon <- NULL

#####
#Prepare for export

#Rename some columns
colnames(ds_tax)[which(colnames(ds_tax) == "YEAR")] <- "Year"
colnames(ds_tax)[which(colnames(ds_tax)== "MONTH")] <- "Month"
colnames(ds_tax)[which(colnames(ds_tax)== "value")] <- "Abundance"

#We don't have PLOTs here. The samples are centred on the same location, but 
#taken at random sites along transects - i.e. not re-sampling the same 
#individuals year after year.

#Add some new columns
ds_tax$Biomass <- rep('', nrow(ds_tax))
ds_tax$Plot <- rep('', nrow(ds_tax))
ds_tax$DepthElevation <- rep('', nrow(ds_tax))
ds_tax$Day <- rep('', nrow(ds_tax))
ds_tax$StudyID <- rep('', nrow(ds_tax))

#For sample description, use Year, Month, Wetland
#There's only one lat-long per wetland, so we don't need to use that (it adds no info)

ds_tax <- ds_tax %>%
  unite(col = SampleDescription, Year, Month, WETLAND, sep = "_", remove = FALSE)

#ds_tax$SampleDescription <- rep('', nrow(ds_tax))

dt_merged <- ds_tax[,c("Abundance", "Biomass", "Family", "Genus", "Species", 
                  "SampleDescription", "Plot", "Latitude", "Longitude",
                  "DepthElevation", "Day", "Month", "Year", "StudyID")] %>% 
  arrange(Year, Family, Genus, Species)

head(dt_merged) 

abund_orig <- dt_merged$Abundance

#Pool abundances of taxa within samples
# aggregate abundance records that are same taxon and sample
dt_merged <- dt_merged %>% 
  group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, 
           Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  dplyr::summarise(Abundance = sum(Abundance)) %>% 
  ungroup() %>% 
  arrange(Year, Family, Genus, Species)

dt_merged$SampleDescription <- as.factor(dt_merged$SampleDescription)

length(levels(dt_merged$SampleDescription)) #2139 sampling events 

#Arrange the columns again
dt_merged <- dt_merged[,c("Abundance", "Biomass", "Family", "Genus", "Species", 
                       "SampleDescription", "Plot", "Latitude", "Longitude",
                       "DepthElevation", "Day", "Month", "Year", "StudyID")] %>% 
  arrange(Year, Family, Genus, Species)

dataset_name <- "muschet_2017_CLSAInvertebrateCounts1992_2021"
#write.csv(dt_merged, paste0(dataset_name, '_rawdata_AMP.csv'), row.names=F, na='') # replace your initials here

#####
#Spreadsheet preparation

# load libraries
library(sf)
library(clipr)

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

# Plot the geometries -----------------------------------------------------
require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
ggplot(world_map) +
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_point(data = dt_merged, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'blue') +
  geom_point(aes(x = centroid[1], y = centroid[2]), size = 3, colour = 'green') +
  geom_sf(data = dt_coord %>% st_convex_hull(), colour = 'blue', fill = 'transparent') +
  coord_sf(xlim=c(-100, -99), ylim=c(46,49)) + # make sure these fit your coordinates!
  labs(x = NULL, y = NULL) +
  theme_minimal()


  



















