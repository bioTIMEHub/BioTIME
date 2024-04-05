library(dplyr)
library(tidyr)
library(rgbif)
library(data.table)

# Load data
raw_data <- read.delim("raw_data/Gaisatjakke_birds_raw.csv", sep = ";", dec = ".")

# Transpose the data frame
raw_data <- raw_data %>% data.table::transpose(keep.names = "Year")

# Correct names, remove useless first row
names(raw_data)[-1] <- raw_data[1, -1]
raw_data <- raw_data[-1, ]

# Remove Investigated area and Total community
raw_data <- raw_data[, -c(2, 43)]

# Correct Year values
raw_data$Year <- 1963:1982

# Pivot longer
data <- raw_data %>% pivot_longer(cols = c(2:41),
                                  names_to = "Binomial",
                                  values_to = "Abundance")

# Make abundance and year integers
data$Abundance <- as.integer(data$Abundance)
data$Year <- as.integer(data$Year)

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
names(data)[c(3:4)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(65.975, length = nrow(data)))
data$Longitude <- as.numeric(rep(16.05, length = nrow(data)))

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
sum(is.na(data$Abundance))
summary(data[, "Year"])

# Save data
write.table(data, "output/Gaisatjakke_birds.csv", sep = ";", dec = ".", row.names = FALSE)
