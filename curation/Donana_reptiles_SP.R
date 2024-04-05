library(tidyr)
library(dplyr)
library(rgbif)
library(stringr)

# Donana_reptiles

# Load raw data
raw_data <- read.delim("raw_data/Donana_reptiles_raw.txt", sep = "\t")
str(raw_data)

# Select useful variables
data <- raw_data %>% select(occurrenceID, individualCount, scientificName)

# Create day, month and year variable from date of occurrences
data$Year <- as.integer(str_extract(data$occurrenceID, "20[0-9]{2}"))
data$Month <- as.integer(gsub("-", "", str_extract(data$occurrenceID, "-[0-9]{2}")))
data$Day <- as.integer(gsub(":", "", gsub("-", "", str_extract(data$occurrenceID, "-[0-9]{2}:"))))

# Create transect variable. Length of different transects is not the same
data$Plot <- gsub(":", "", substr(data[, "occurrenceID"], 13, 15))

# Summarise data by date and transect
data <- data %>% group_by(Year, Month, Day, Plot, scientificName) %>%
                 summarise(Abundance = sum(individualCount)) %>%
                 mutate(Binomial = scientificName) %>%
                 select(-scientificName)

# Correct names
data$Binomial[data$Binomial == "Podarcis carbonelli (Perez Mellado, 1981)"] <- "Podarcis_carbonelli"
data$Binomial[data$Binomial == "Psammodromus algirus  (Linnaeus, 1758)"] <- "Psammodromus_algirus"
data$Binomial[data$Binomial == "Tarentola mauritanica  (Linnaeus, 1758)"] <- "Tarentola_mauritanica"
data$Binomial[data$Binomial == "Acanthodactylus erythrurus (Schinz, 1833)"] <- "Acanthodactylus_erythrurus"
data$Binomial[data$Binomial == "Testudo graeca (Linnaeus, 1758)"] <- "Testudo_graeca"
data$Binomial[data$Binomial == "Psammodromus occidentalis  (Fitze, Gonzalez-Jimena, San Jose, San Mauro & Zardoya, 2012)"] <- "Psammodromus_occidentalis"
data$Binomial[data$Binomial == "Timon lepidus (Daudin, 1802)"] <- "Timon_lepidus"
data$Binomial[data$Binomial == "Hemorrhois hippocrepis (Linnaeus, 1758)"] <- "Hemorrhois_hippocrepis"
data$Binomial[data$Binomial == "Chalcides striatus (Cuvier, 1829)"] <- "Chalcides_striatus"
data$Binomial[data$Binomial == "Malpolon monspessulanus (Hermann, 1804)"] <- "Malpolon_monspessulanus"

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial

# Merge (leaving Reptilia observations behind)
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(6, 7)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(37.0, length = nrow(data)))
data$Longitude <- as.numeric(rep(-6.4, length = nrow(data)))

# Add sample description
data$SampleDescription <- as.factor(with(data, Year))
length(levels(data$SampleDescription))

# Add empty columns
data$Biomass <- rep(NA, nrow(data))
data$DepthElevation <- rep(NA, nrow(data))
data$StudyID <- rep(NA, nrow(data))

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
               "StudyID")] %>% arrange(Year, Family, Genus, Species)

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
write.table(data, "output/Donana_reptiles.csv", sep = ";", dec = ".", row.names = FALSE)
