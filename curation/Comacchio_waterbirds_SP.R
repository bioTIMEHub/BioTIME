library(dplyr)
library(tidyr)
library(rgbif)

# Comacchio_waterbirds

# Import data
raw_data <- read.delim("raw_data/Comacchio_waterbirds_raw.csv", sep = ";", dec = ".")
str(raw_data)

# Pivot longer
data <- raw_data %>% pivot_longer(cols = c(2:9),
                                  names_to = "Binomial",
                                  values_to = "Abundance")

# Add 1 observation mentioned in the text but not included in the data table
data[nrow(data) + 1, "year"] <- 1978
data[nrow(data), "Binomial"] <- "Hydroprogne_caspia"
data[nrow(data), "Abundance"] <- 1

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial

# Merge
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(1, 3:4)] <- c("Year", "Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(44.61, length = nrow(data)))
data$Longitude <- as.numeric(rep(12.18, length = nrow(data)))

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
write.table(data, "output/Comacchio_waterbirds.csv", sep = ";", dec = ".", row.names = FALSE)
