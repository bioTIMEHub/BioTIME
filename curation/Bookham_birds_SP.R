library(dplyr)
library(tidyr)
library(data.table)
library(rgbif)

# Bookham_birds

# Load data
raw_data <- read.csv("raw_data/Bookham_birds_raw.csv", sep = ";", dec = ".")
str(raw_data)

# Reformat data
data <- raw_data %>% pivot_longer(cols = c(3:29),
                                  names_to = "Year",
                                  values_to = "Abundance") %>%
                     mutate(Binomial = Scientific_name) %>%
                     select(-Common_name, -Scientific_name)


# Remove X from Year values
data$Year <- as.integer(gsub("X", "", data$Year))
 
# Correct names
data$Binomial[data$Binomial == "Curruca communis"] <- "Sylvia communis"
data$Binomial[data$Binomial == "Curruca curruca"] <- "Sylvia curruca"

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = species) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial

# Merge
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".* ", "", Binomial)) %>%
                 select(-Binomial)

# Correct column names
names(data)[3:4] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(51.295, length = nrow(data)))
data$Longitude <- as.numeric(rep(-0.383, length = nrow(data)))

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
write.table(data, "output/Bookham_birds.csv", sep = ";", dec = ".", row.names = FALSE)
