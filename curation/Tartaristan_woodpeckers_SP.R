library(dplyr)
library(tidyr)
library(janitor)
library(rgbif)

# Tartaristan_woodpeckers

# Import data
raw_data <- read.delim("raw_data/Tartaristan_woodpeckers_raw.csv", sep = ",")
str(raw_data)

# Create names vector
spp_names <- c("Dendrocopos_major", "Picus_viridis", "Picoides_tridactylus",
               "Dendrocopos_leucotos", "Picus_canus", "Dryocopus_martius",
               "Dryobates_minor")

# Create "years" vector
year <- 1991:2015

# Select densities
data <- raw_data %>% select(starts_with("X"))

# Remove useless first row
data <- data[-1, ]

# Add years, correct names
names(data) <- spp_names
data <- cbind(year, data)

# Make data numeric
data[, -1] <- apply(data[, -1], 2, as.numeric)

# Round to meaningful digits
data[, -1] <- janitor::round_half_up(data[, -1], digits = 2)

# Convert all values =< 0.01 to 0, because when digitizing some
# zeros were recorded as very low densities
data[data <= 0.01] <- 0

# Pivot longer
data <- data %>% pivot_longer(cols = c(2:8),
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
write.table(data, "output/Tartaristan_woodpeckers.csv", sep = ";", dec = ".", row.names = FALSE)
