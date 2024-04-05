library(dplyr)
library(tidyr)
library(rgif)

# CaboPalos_fish

# Load data
raw_data <- read.delim("raw_data/CaboPalos_fish_raw.csv", sep = ";", dec = ".")
str(raw_data)

# Pivot longer
data <- raw_data %>% pivot_longer(cols = c(2:62),
                                  names_to = "Binomial",
                                  values_to = "Abundance")

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial[59]

unique(data$Binomial)[59]
data$Binomial[data$Binomial == "Symphodus_melanocercus"] <- "Centrolabrus_melanocercus"
data$Binomial[data$Binomial == "Scorpaena_nonata"] <- "Scorpaena_notata"

# Genus-level taxa
taxonomy$Binomial[18] <- unique(data$Binomial)[18]
taxonomy$Binomial[55] <- unique(data$Binomial)[55]
taxonomy$Binomial[60] <- unique(data$Binomial)[60]

unique(data$Binomial) == taxonomy$Binomial

# Merge
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(1, 3:4)] <- c("Year", "Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(37.65, length = nrow(data)))
data$Longitude <- as.numeric(rep(-0.67, length = nrow(data)))

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
sum(data$Abundance == "") > 0
summary(data[, "Year"])
sum(is.na(data))

# Save data
write.table(data, "output/CaboPalos_fish.csv", sep = ";", dec = ".", row.names = FALSE)
