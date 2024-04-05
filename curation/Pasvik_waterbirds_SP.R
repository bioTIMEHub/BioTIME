library(dplyr)
library(tidyr)
library(rgbif)

# Pasvik_waterbirds

# Import data
raw_data <- read.delim("raw_data/Pasvik_waterbirds_raw.csv", sep = ";", dec = ".")
str(raw_data)

# Correct years format
names(raw_data)[2:27] <- c("Binomial", 1996:2020)

# Remove norwegian names, pivot longer
data <- raw_data %>% select(-Norwegian) %>%
                     pivot_longer(cols = c(2:26),
                                  names_to = "Year",
                                  values_to = "Abundance")

# Years as integer
data$Year <- as.integer(data$Year)

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial
data$Binomial[data$Binomial == "Mareca_penelope"] <- "Anas_penelope"
data$Binomial[data$Binomial == "Mareca_strepera"] <- "Anas_strepera"
data$Binomial[data$Binomial == "Mareca_strepera"] <- "Anas_strepera"
data$Binomial[data$Binomial == "Spatula_querquedula"] <- "Anas_querquedula"
data$Binomial[data$Binomial == "Spatula_clypeata"] <- "Anas_clypeata"
data$Binomial[data$Binomial == "Calidris_pugnax"] <- "Philomachus_pugnax"
unique(data$Binomial) == taxonomy$Binomial

# Merge
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(3:4)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(69.15, length = nrow(data)))
data$Longitude <- as.numeric(rep(29.25, length = nrow(data)))

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
write.table(data, "output/Pasvik_waterbirds.csv", sep = ";", dec = ".", row.names = FALSE)
