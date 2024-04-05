library(dplyr)
library(tidyr)

# Halle_SteppicVegetation

# Import data (3 plots)
p1 <- read.delim("raw_data/Halle_SteppicVegetation_p1.csv", sep = ";", dec = ".")
p1$Plot <- rep(1, times = nrow(p1))
p1 <- p1[-c(35:37), ]

p2 <- read.delim("raw_data/Halle_SteppicVegetation_p2.csv", sep = ";", dec = ".")
p2$Plot <- rep(2, times = nrow(p2))
p2 <- p2[-c(35:37), ]

p3 <- read.delim("raw_data/Halle_SteppicVegetation_p3.csv", sep = ";", dec = ".")
p3$Plot <- rep(3, times = nrow(p3))
p3 <- p3[-c(35:37), ]

# Merge data frames
data <- rbind(p1, p2, p3)

# Remove unnecessary variables, change to longer format
data <- data %>% select(-RG, -FG) %>%
                 pivot_longer(cols = c(2:25), names_to = "Year", values_to = "Abundance") %>%
                 mutate(Binomial = gsub(" ", "_", Species)) %>%
                 select(-Species)

# Give format to "Year"
data$Year <- as.integer(gsub("X", "", data$Year))

# Add taxonomic fields
taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

# Check names
unique(data$Binomial) == taxonomy$Binomial

data$Binomial[data$Binomial == "Festuca_glaucina"] <- "Festuca_cinerea"
data$Binomial[data$Binomial == "Hieracium_pilosella"] <- "Pilosella_officinarum"
data$Binomial[data$Binomial == "Euphorbia_cypariassis"] <- "Euphorbia_cyparissias"
data$Binomial[data$Binomial == "Armeria_elongata"] <- "Armeria_maritima"
data$Binomial[data$Binomial == "Thymus_serpillum"] <- "Thymus_serpyllum"
data$Binomial[data$Binomial == "Agrostis_capilaris"] <- "Agrostis_capillaris"
data$Binomial[data$Binomial == "Dianthus_cartusianum"] <- "Dianthus_carthusianorum"
data$Binomial[data$Binomial == "Koelleria_macrantha"] <- "Koeleria_macrantha"
data$Binomial[data$Binomial == "Rumex_acetosela"] <- "Rumex_acetosella"
data$Binomial[data$Binomial == "Erophila_verna"] <- "Draba_verna"
data$Binomial[data$Binomial == "Gallium_sp."] <- "Galium_sp"
data$Binomial[data$Binomial == "Scleranthus_annus"] <- "Scleranthus_annuus"
data$Binomial[data$Binomial == "Achillea_millifolia"] <- "Achillea_millefolium"
data$Binomial[data$Binomial == "Poa_sp."] <- "Poa_sp"
data$Binomial[data$Binomial == "Taraxacum_officinalis"] <- "Taraxacum_officinale"
data$Binomial[data$Binomial == "Anthoxantus_odoratum"] <- "Anthoxanthum_odoratum"
data$Binomial[data$Binomial == "Armeria_elongate"] <- "Armeria_maritima"

unique(data$Binomial)[23]
unique(data$Binomial)[32]

taxonomy <- rgbif::name_backbone_checklist(unique(data$Binomial)) %>%
            select(family, genus, species) %>%
            mutate(Binomial = sub(" ", "_", species)) %>%
            select(-species)

unique(data$Binomial) == taxonomy$Binomial

# Merge
data <- merge(data, taxonomy, by = "Binomial", all.x = TRUE)

# Fix manually the data for plants identified to genus level
data[data$Binomial == "Poa_sp", c("family", "genus")] <- c("Poaceae", "Poa")
data[data$Binomial == "Galium_sp", c("family", "genus")] <- c("Rubiaceae", "Galium")

# Add "Species" field, remove "Binomial"
data <- data %>% mutate(Species = sub(".*_", "", Binomial)) %>%
                 select(-Binomial)

# Correct column names
names(data)[4:5] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(51.583333, length = nrow(data)))
data$Longitude <- as.numeric(rep(11.833333, length = nrow(data)))

# Add sample description
data$SampleDescription <- as.factor(with(data, paste(Year, "Plot", Plot, sep = "_")))
length(levels(data$SampleDescription))

# Add empty columns
data$Biomass <- rep(NA, nrow(data))
data$DepthElevation <- rep(NA, nrow(data))
data$Day <- rep(NA, nrow(data))
data$Month <- rep(NA, nrow(data))
data$StudyID <- rep(NA, nrow(data))

# Reorder to proper format
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

# Check the result
head(data)

# Remove zeros and NAs
data <- data[data$Abundance != 0 & !is.na(data$Abundance), ]

# Checks
dim(data)
summary(data)
is.numeric(data$Abundance)
is.integer(data$Year)
min(data$Abundance)
sum(data$Abundance == "") > 0
summary(data[, "Year"])

# Save data
write.table(data, "output/Halle_SteppicVegetation.csv", sep = ";", dec = ".", row.names = FALSE)
