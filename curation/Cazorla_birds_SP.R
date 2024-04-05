library(dplyr)
library(tidyr)
library(janitor)
library(rgbif)

# Cazorla_birds

# Import data
raw_data <- read.delim("raw_data/Cazorla_birds_raw.csv", sep = ";", dec = ".")
str(raw_data)

# Standardize data per 100 net hours, round to meaningful digits
raw_data[, 3:34] <- janitor::round_half_up(raw_data[, 3:34]/raw_data$net.hours * 100, digits = 2)

# Remove variable "net.hours"
raw_data <- raw_data %>% select(-net.hours)

# Pivot longer
data <- raw_data %>% pivot_longer(cols = c(2:33),
                                  names_to = "Binomial",
                                  values_to = "Abundance")

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
data$Latitude <- as.numeric(rep(37.94, length = nrow(data)))
data$Longitude <- as.numeric(rep(-2.87, length = nrow(data)))

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
write.table(data, "output/Cazorla_birds.csv", sep = ";", dec = ".", row.names = FALSE)
