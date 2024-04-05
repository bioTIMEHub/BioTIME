library(tidyr)
library(dplyr)
library(rgbif)

# Zackenberg_birds

# Load data
raw_data <- read.delim("raw_data/Zackenberg_birds_raw.txt", sep = "\t")
str(raw_data)

# Change first column name
names(raw_data)[1] <- "Year"

# Name for Stercorarius longicaudus is wrong in the last two years
raw_data$Species[raw_data$Species == "Stercorarius longicaudis"] <- "Stercorarius longicaudus"

# Calculate number of territories per species and year
data <- raw_data %>% group_by(Year, Species) %>%
                     summarise(Abundance = n()) %>%
                     mutate(Binomial = sub(" ", "_", Species)) %>%
                     select(-Species)

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial

data$Binomial[data$Binomial == "Sommateria_mollissima"] <- "Somateria_mollissima"

unique(data$Binomial) == taxonomy$Binomial

# Merge
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(3:4)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(74.47, length = nrow(data)))
data$Longitude <- as.numeric(rep(-20.57, length = nrow(data)))

# Add sample description
data$SampleDescription <- as.factor(with(data, Year))
length(levels(data$SampleDescription))

# Add empty columns
data$Biomass <- rep(NA, nrow(data))
data$Plot <- rep(NA, nrow(data))
data$DepthElevation <- rep(NA, nrow(data))
data$Day <- rep(NA, nrow(data))
data$Month <- rep(NA, nrow(data))
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
sum(data$Abundance=="") > 0
summary(data[, "Year"])

# Save data
write.table(data, "output/Zackenberg_birds.csv", sep = ";", dec = ".", row.names = FALSE)
