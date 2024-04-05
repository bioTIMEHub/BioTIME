library(dplyr)
library(tidyr)
library(rgbif)

# AlbuferaMallorca_waterbirds

# Import data
raw_data <- read.delim("raw_data/AlbuferaMallorca_waterbirds_raw.csv", sep = ";", dec = ".")
str(raw_data)

# Pivot longer
data <- raw_data %>% pivot_longer(cols = c(2:114),
                                  names_to = "Binomial",
                                  values_to = "Abundance")

# Correct names
data$Binomial[data$Binomial == "Mareca_penelope"] <- "Anas_penelope"
data$Binomial[data$Binomial == "Mareca_strepera"] <- "Anas_strepera"
data$Binomial[data$Binomial == "Spatula_clypeata"] <- "Anas_clypeata"
data$Binomial[data$Binomial == "Spatula_querquedula"] <- "Anas_querquedula"
data$Binomial[data$Binomial == "Calidris_pugnax"] <- "Philomachus_pugnax"
data$Binomial[data$Binomial == "Alopochen_aegyptiacus"] <- "Alopochen_aegyptiaca"
data$Binomial[data$Binomial == "Porphyrula_alleni"] <- "Porphyrio_alleni"
data$Binomial[data$Binomial == "Threskyornis_aethiopicus"] <- "Threskiornis_aethiopicus"
data$Binomial[data$Binomial == "Anser_rosii"] <- "Chen_rossii"
data$Binomial[data$Binomial == "Sterna_sandvicensis"] <- "Thalasseus_sandvicensis"
data$Binomial[data$Binomial == "Larus_genei"] <- "Chroicocephalus_genei"
data$Binomial[data$Binomial == "Cygnus_colombianus"] <- "Cygnus_columbianus"
data$Binomial[data$Binomial == "Circus_macrorus"] <- "Circus_macrourus"
data$Binomial[data$Binomial == "Hydropogne_caspia"] <- "Hydroprogne_caspia"

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial

# No-species taxa
unique(data$Binomial)[57]
unique(data$Binomial)[61]
unique(data$Binomial)[71]
unique(data$Binomial)[75]
unique(data$Binomial)[110]

# Merge, leaving non-species behind
data <- merge(data, taxonomy, by = "Binomial")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                select(-Binomial)

# Correct column names
names(data)[c(3:4)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(39.8, length = nrow(data)))
data$Longitude <- as.numeric(rep(3.1, length = nrow(data)))

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
write.table(data, "output/AlbuferaMallorca_waterbirds.csv", sep = ";", dec = ".", row.names = FALSE)
