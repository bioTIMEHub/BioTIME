#Curating Myers-Smith et al. 2019 dataset

setwd("C:/Users/apenny3/OneDrive - University of Edinburgh/PapersInPrep/BioTIME_2.0/BioTIME hack/myers-smith_2019")
rm(list=ls())

# load the required packages!
require(dplyr)
require(ggplot2)
require(stringr)
require(readxl)
require(maps)


#read in dataset
ds <- readRDS("ddata.rds")

View(ds)

#Dataset requirements:
#Dataset consists of at least 2 years of sampling (they do not have to be consecutive).

n_distinct(ds$year) #8
# -> yes

#Dataset consists of entire assemblages, not just populations (sensu lato, i.e. does not exclude some taxa intentionally).

# -> yes - tundra shrubs

#Data should record abundance, biomass or both.

# - > yes, column 'cover' gives cover of each taxon in column 'name' 
#(this is a proportion of the points in a quadrat at which a particular
#plant species is present)

#' Species counts and bare
#ground were converted to abundance by calculating the
#proportion of points at which each species was present.'


#Sampling methods and effort are consistent through time.

# - > yes

#Individuals are identified to species level where possible.

# - > yes

#OK so let's go ahead and curate this dataset

###OVERALL CHECKS
dim(ds)

str(ds)

#1 - abundance or biomass needs to be numeric
is.numeric(ds$cover) #TRUE

#2 - lat-long need to be numbers, not character

unique(ds$sub_lat)
#[1] "69,58"

#Herschel Island is at 69.6 degrees north, so this is probably decimal degrees

unique(ds$sub_long)
#[1] "138,86" "138,87" 

#Plotted these as decimal degrees in Google Maps, and the locations matched the
#ones shown on maps in the paper - so replace commas with decimal points.

ds$sub_lat <- str_replace_all(string = ds$sub_lat, pattern = ",", replacement = ".")
ds$sub_long <- str_replace_all(string = ds$sub_long, pattern = ",", replacement = ".")

#convert latitude and longitude to numeric
ds$sub_lat <- as.numeric(ds$sub_lat)
ds$sub_long <- as.numeric(ds$sub_long)

#there are two values for sub_name: #"QHI:HE" "QHI:KO"
#I think these are two different vegetation types

#how many plots per vegetation type?

check <- ds %>%
  group_by(sub_name) %>%
  summarise(n_plot = n_distinct(plot)) %>%
  ungroup()

check

#6 plots per vegetation type. good.

#3 - Year needs to be integer or factor
is.integer(ds$year) #TRUE

#4 - plot needs to be factor or integer
is.integer(ds$plot) #TRUE

#5 - DepthElevation (we don't have this)

#6 - Taxonomic needs to be character or factor

is.character(ds$name)

summary(ds)

###PRIMARY FIELDS
#Abundance, coordinates and dates

#Abundance
min(ds$cover) > 0 #TRUE
length(which(is.na(ds$cover))) #0
range(ds$cover) #1, 100 is logical
sum(ds$cover == "") #0

#coordinates
range(ds$sub_lat)
length(which(is.na(ds$sub_lat))) #0
sum(ds$sub_lat == "") #0

range(ds$sub_long)
length(which(is.na(ds$sub_long))) #0
sum(ds$sub_long == "") #0

#dates 
range(ds$year)
length(which(is.na(ds$year)))
sum(ds$year == "")

#Do we need to pool abundances?
unique(ds$name) #No, though there are non-taxonomic values which we should remove

#Plotting coordinates - did this in Google maps and the coordinates are correct

###SECONDARY FIELDS
#check plot
sort(unique(ds$plot)) # 1:6
sort(unique(ds$sub_name)) #"QHI:HE" "QHI:KO"

#there's no treatment or any other secondary field which we haven't already checked


#check - are all six plots sampled every year?
check_sampling <- ds %>%
  group_by(sub_name, plot) %>%
  summarise(n_year_sampled = n_distinct(year)) %>%
  ungroup()

unique(check_sampling$n_year_sampled) #8 - all six plots are sampled every year,
#so sampling is even.

###TAXONOMIC FIELDS

#check out the taxonomic names
sort(unique(ds$name)) #no NAs, zeroes or blanks, but there are non-taxonomic 
#names which begin with "xxx". But not all names starting with "xxx" are non-biological 
#records which we might want to remove, so we will need to clean things up manually.

#No mis-spellings causing synonymies.

#for biological names which start with "xxx", we will want to remove "xxx" so that
#we can use these taxonomic names later.

#there are some genus-level IDs ("Oxytropis ", "Pedicularis ") which we'll want to deal with


#Eliminate blanks, NAs (but uncertain IDs up to family level are allowed), and zeroes. -> DONE
#Fit uncertain IDs to our syntax or eliminate abbreviations that dataset contributors used.

ds$taxon <- ds$name

#check for family names
str_which(ds$taxon, 'idae$|eae$') #0, no family names

#Remove non-organismal records and incidental species

ds_copy <- ds
ds_copy <- ds_copy[ds_copy$name != "XXXbareground ",]
ds_copy <- ds_copy[ds_copy$name != "XXXfeces ",]
ds_copy <- ds_copy[ds_copy$name != "XXXlitter ",]
ds_copy <- ds_copy[ds_copy$name != "XXXrock ",]
ds_copy <- ds_copy[ds_copy$name != "XXXspider ",] #incidental species
ds_copy <- ds_copy[ds_copy$name != "Xxxspider",] 
ds_copy <- ds_copy[ds_copy$name != "XXXspider",]
ds_copy <- ds_copy[ds_copy$name != "XXXstandingwater ",] 
ds_copy <- ds_copy[ds_copy$name != "XXXunkwoody ",] 

sort(unique(ds_copy$name))

#change e.g. ""XXXcarex:QHI " to "Carex sp", as there are no other Carex species here
ds <- ds_copy

#Tidy up genus names
ds$taxon[ds$taxon == "XXXcarex:QHI "] <- "Carex sp"
ds$taxon[ds$taxon == "XXXcetraria:QHI "] <- "Cetraria sp"
ds$taxon[ds$taxon == "XXXcladonia:QHI "] <- "Cladonia sp"
ds$taxon[ds$taxon == "XXXkobresia:QHI "] <- "Kobresia sp"
ds$taxon[ds$taxon == "XXXpeltigera:QHI "] <- "Peltigera sp"

ds$taxon[ds$taxon == "Oxytropis "] <- "Oxytropis sp"
ds$taxon[ds$taxon == "Pedicularis "] <- "Pedicularis sp"

#Tidy up other taxonomic names
ds$taxon[ds$taxon == "XXXfungus "] <- "Fungi sp1"
ds$taxon[ds$taxon == "XXXotherfungus "] <- "Fungi sp2"

ds$taxon[ds$taxon == "XXXliverwort:QHI "] <- "Liverwort sp"

#Consulted with Isla Myers-Smith over taxonomic names for other taxa in this dataset:
ds$taxon[ds$taxon == "XXXLUZSP:QHI "] <- "Luzula sp"
ds$taxon[ds$taxon == "XXXothermoss "] <- "Bryophyta sp" #Likely multiple moss species represented, but no way to separate them
ds$taxon[ds$taxon == "XXXotherlichen:QHI "] <- "Lichen sp2" #denotes a crustose lichen different from the crustose lichen below
ds$taxon[ds$taxon == "XXXothergram "] <- "Gramineae sp" # NB this is a family, not a genus
ds$taxon[ds$taxon == "XXXotheraster:QHI "] <- "Aster sp"
ds$taxon[ds$taxon == "XXXotherherb:QHI "] <- "Tracheophyta sp" # unidentifiable herbaceous plant
ds$taxon[ds$taxon == "XXXcrustous:QHI "] <- "Lichen sp1"

#Change "Cladina (brown)" to "Cladina sp" as it is a genus-level ID
ds$taxon[ds$taxon == "Cladina (brown)"] <- "Cladina sp"

#Remove the old names
ds$name <- NULL

#Fit taxa level with the correct fields (Family, Genus, Species)
#Separate the taxon column at the space.

#Working from a copy
ds_copy <- ds %>%
  separate(col = taxon, into = c("Genus", "Species"), remove = FALSE) 

#Manually move Genus names to Family if they don't represent a Genus
ds_copy$Family <- NA

#We do have some higher-level taxonomy to fix:
#Gramineae sp
#Tracheophyta sp
#Liverwort sp1 and sp2
#Fungi sp1 and Fungi sp2
#Lichen sp1 and sp2

#Put 'Gramineae' in the Family column with sp in the Species column
ds_copy$Family[which(ds_copy$Genus == "Gramineae")] <- "Gramineae"
ds_copy$Genus[which(ds_copy$Genus == "Gramineae")] <- ""

#Put 'Tracheophyta' in the Family column even though it's not a Family
ds_copy$Family[which(ds_copy$Genus == "Tracheophyta")] <- "Tracheophyta"
ds_copy$Genus[which(ds_copy$Genus == "Tracheophyta")] <- ""

#Put 'Liverwort' in the Family column, with sp in the Species column
ds_copy$Family[which(ds_copy$Genus == "Liverwort")] <- "Liverwort"
ds_copy$Genus[which(ds_copy$Genus == "Liverwort")] <- ""

#Put Fungi in the Family column, and sp1 or sp2 in the Species column
ds_copy$Family[which(ds_copy$Genus == "Fungi")] <- "Fungi"
ds_copy$Genus[which(ds_copy$Genus == "Fungi")] <- ""

#Put Lichen in the Family column, and sp1 or sp2 in the Species column
ds_copy$Family[which(ds_copy$Genus == "Lichen")] <- "Lichen"
ds_copy$Genus[which(ds_copy$Genus == "Lichen")] <- ""

#check the Species and Family names we now have
sort(unique(ds_copy$Species))
sort(unique(ds_copy$Genus))
sort(unique(ds_copy$Family))

#Remove the old taxon column
ds_copy$taxon <- NULL

#overwrite ds
ds <- ds_copy

rm(ds_copy)

#Prepare for export

#Add columns for Biomass, Plot, DepthElevation, Day, Month, StudyID
#We have % cover data, so we have a Biomass column, but no Abundance column
ds$Abundance <- rep('', nrow(ds))

#ds$Plot <- rep('', nrow(ds)) # we do have plot data, have merged the sub_name and plto columns to create this

ds <- ds %>%
  unite(col = "Plot", sub_name, plot, sep = "_")

ds$DepthElevation <- rep('', nrow(ds))
ds$Day <- rep('', nrow(ds))
ds$Month <- rep('', nrow(ds))
ds$StudyID <- rep('', nrow(ds))

#Rename the columns we want to keep
colnames(ds)[which(colnames(ds) == "sub_lat")] <- "Latitude"
colnames(ds)[which(colnames(ds) == "sub_long")] <- "Longitude"
colnames(ds)[which(colnames(ds) == "year")] <- "Year"
colnames(ds)[which(colnames(ds) == "sub_long")] <- "Longitude"
colnames(ds)[which(colnames(ds) == "cover")] <- "Biomass"

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- ds %>% group_by(Abundance, Family, Genus, Species, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Biomass=sum(Biomass)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)

dim(ds)[1]-dim(dt_merged)[1] # 4; a change in nrows with aggregating
sum(ds$Biomass) == sum(dt_merged$Biomass) # TRUE, good

# save the dataset name as an object so we save some typing
dataset_name <- 'myers_smith_2019'

# put in as many non-blank fields unique to the sampling event

# If plot, depthelevation, month, and day are available, I'd add those in too
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset_name, Latitude, Longitude, Year, Plot, sep='_'))) # Added plot
length(levels(dt_merged$SampleDescription)) # how many sampling events? # 96

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
head(dt_merged) # final check!

#setwd(file.choose() %>% dirname()) # choose a file in the directory you want this saved in
#write.csv(dt_merged, paste0(getwd(),"/", dataset_name, '_rawdata_AMP.csv'), row.names=F, na='') # replace your initials here

