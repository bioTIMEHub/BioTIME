library(dplyr)
library(tidyr)
library(rgbif)

# Teich_waterbirds

# Import data
raw_data <- read.delim("raw_data/Teich_birds_raw.csv", sep = ",")
str(raw_data)

# Select useful variables
raw_data <- raw_data[, c("Nom_latin", "Jour", "Mois", "Annee", "Nombre", "Protocol")]

# Filter complete counts (Protocol == 1, fully standardized reserve-scale monthly snapshot)
data <- raw_data %>% filter(Protocol == 1)

# Remove Protocol variable
data <- data %>% select(-Protocol)

# Correct variable names
names(data) <- c("Binomial", "Day", "Month", "Year", "Abundance")

# Correct names, remove subspecies
data$Binomial[data$Binomial == "Casmerodius albus"] <- "Ardea alba"
data$Binomial[data$Binomial == "Sterna sandvicensis"] <- "Thalasseus sandvicensis"
data$Binomial[data$Binomial == "Casmerodius albus"] <- "Ardea alba"
data$Binomial[data$Binomial == "Aquila pennata"] <- "Hieraaetus pennatus"
data$Binomial[data$Binomial == "Larus melanocephalus"] <- "Ichthyaetus melanocephalus"
data$Binomial[data$Binomial == "Dendrocopos minor"] <- "Dryobates minor"
data$Binomial[data$Binomial == "Carduelis spinus"] <- "Spinus spinus"
data$Binomial[data$Binomial == "Carduelis cannabina"] <- "Linaria cannabina"
data$Binomial[data$Binomial == "Carduelis chloris"] <- "Chloris chloris"
data$Binomial[data$Binomial == "Motacilla flava flava"] <- "Motacilla flava"
data$Binomial[data$Binomial == "Motacilla flava flavissima"] <- "Motacilla flava"
data$Binomial[data$Binomial == "Motacilla flava iberiae"] <- "Motacilla flava"
data$Binomial[data$Binomial == "Larus adouinii"] <- "Ichthyaetus audouinii"
data$Binomial[data$Binomial == "Motacilla alba yarrellii"] <- "Motacilla alba"
data$Binomial[data$Binomial == "Larus glaucoides kumlieni"] <- "Larus glaucoides"
data$Binomial[data$Binomial == "Motacilla flava cinereocapilla"] <- "Motacilla flava"
data$Binomial[data$Binomial == "Phylloscopus sibilatrix"] <- "Phylloscopus sibillatrix"
data$Binomial[data$Binomial == "Motacilla flava thunbergi"] <- "Motacilla flava"
data$Binomial[data$Binomial == "Motacilla flava thunbergi"] <- "Motacilla flava"
data$Binomial[data$Binomial == "Columba livia f. domestica"] <- "Columba livia"
data$Binomial[data$Binomial == "Luscinia svecica namnetum"] <- "Luscinia svecica"
data$Binomial[data$Binomial == "Luscinia svecica cyanecula"] <- "Luscinia svecica"
data$Binomial[data$Binomial == "Carduelis flammea"] <- "Acanthis flammea"
data$Binomial[data$Binomial == "Luscinia svecica cyanecula / namnetum"] <- "Luscinia svecica"
data$Binomial[data$Binomial == "Oenanthe oenanthe leucorhoa"] <- "Oenanthe oenanthe"

# Remove observations not identified to species level 
# CC commenting out to allow for this
# data <- data %>% filter(!Binomial %in% c("Larus sp.",
#                                          "Larus argentatus / cachinanns / michahellis",
#                                          "Phylloscopus sp.",
#                                          "Pluvialis sp."))

data$Binomial <- stringr::str_replace_all(data$Binomial, 'sp.$|argentatus \\/ cachinanns \\/ michahellis', 'sp')


# Sum observations of same species but different subspecies
data <- data  %>% group_by(Binomial, Day, Month, Year) %>%
                  summarise(Abundance = sum(Abundance))

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species)

names(taxonomy)[3] <- "Binomial"

# Check names
unique(data$Binomial) == taxonomy$Binomial
sum(is.na(data$Binomial) | data$Binomial == FALSE)

# Merge
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".* ", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(5, 6)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- 44.64
data$Longitude <- -1.02

# Add sample description
data$SampleDescription <- as.factor(with(data, paste(Day, Month, Year, sep = "_")))
length(levels(data$SampleDescription))

# Add empty columns
data$Biomass <- ''
data$Plot <- ''
data$DepthElevation <- ''
data$StudyID <- ''

# Reorder variables
data <- data[c("Abundance",
               "Biomass",
               "Family",
               "Genus",
               "Species",
               "SampleDescription",
               "Plot",
               "Latitude",
               "Longitude",
               "DepthElevation",
               "Day",
               "Month",
               "Year",
               "StudyID")] %>% arrange(Year, Month, Day, Family, Genus, Species)

# Remove zeros and NAs
data <- data[data$Abundance != 0 & !is.na(data$Abundance), ]

# Check the result
head(data)

# Checks
dim(data)
summary(data)
is.numeric(data$Abundance)
is.integer(data$Year)
min(data$Abundance)
sum(data$Abundance == "") > 0
summary(data[, "Year"])
sum(is.na(data))

# Save data
write.table(data, "output/Teich_birds.csv", sep = ";", dec = ".", row.names = FALSE)
