library(tidyr)
library(dplyr)
library(stringr)
library(rgbif)

# EBD_aerial_counts

# Import data
raw_data <- read.delim("raw_data/EBD_aerial_counts_raw.csv", sep = ",")
str(raw_data)

# Correct characters in variable names
names(raw_data)[c(8, 10, 11)] <- c("Codigo.de.censo", "Año", "Censo.valido")

# Select useful variables. I am selecting the highest level of locations, the other 2 levels are subdivisions
raw_data <- raw_data[c("Fecha", "Especie", "Individuos", "Localidad.nivel.1",
                       "Censo.valido")]

# Filter valid counts
data <- raw_data %>% filter(Censo.valido == "True")

# Create day, month and year variable from date of occurrences
data$Year <- as.integer(str_extract(data$Fecha, "[0-9]{4}"))
data$Month <- as.integer(gsub("-", "", str_extract(data$Fecha, "-[0-9]{2}-")))
data$Day <- as.integer(gsub("-", "", str_extract(data$Fecha, "-[0-9]{2}$")))

# Remove date and valid count variables
data <- data %>% select(-Fecha, -Censo.valido)
head(data)

# Change variable names
names(data)[c(1:3)] <- c("Binomial", "Abundance", "Plot")

# Correct plot names (problem with ñ)
data$Plot[data$Plot == unique(data$Plot)[1]] <- "Parque Nacional de Doñana"
data$Plot[data$Plot == unique(data$Plot)[4]] <- "Salinas de Bonanza"

# Summarise data by date, species and plot
data <- data %>% group_by(Year, Month, Day, Plot, Binomial) %>%
                 summarise(Abundance = sum(Abundance))

# Correct names
data$Binomial[data$Binomial == "Calidris pugnax"] <- "Philomachus pugnax"
data$Binomial[data$Binomial == "Mareca penelope"] <- "Anas penelope"
data$Binomial[data$Binomial == "Mareca strepera"] <- "Anas strepera"
data$Binomial[data$Binomial == "Spatula clypeata"] <- "Anas clypeata"
data$Binomial[data$Binomial == "Spatula querquedula"] <- "Anas querquedula"
data$Binomial[data$Binomial == "Larus melanocephalus"] <- "Ichthyaetus melanocephalus"

# Remove observations not identified to species level
data <- data %>% filter(!Binomial %in% c("Anatidae",
                                         "Charadrius/Calidris",
                                         "Tringa totanus/erythropus",
                                         "Larus michahellis/fuscus",
                                         "Bubulcus/Egretta",
                                         "Chroicocephalus genei/ridibundus"
                                         ))

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
names(data)[c(6, 7)] <- c("Family", "Genus")

# Add Latitude and Longitude
data$Latitude <- as.numeric(rep(NA, length = nrow(data)))
data$Longitude <- as.numeric(rep(NA, length = nrow(data)))

data[data$Plot == "Parque Nacional de Doñana", "Latitude"] <- 37.00
data[data$Plot == "Parque Nacional de Doñana", "Longitude"] <- -6.36
data[data$Plot == "Veta La Palma", "Latitude"] <- 36.98
data[data$Plot == "Veta La Palma", "Longitude"] <- -6.22
data[data$Plot == "Parque Natural Norte", "Latitude"] <- 37.12
data[data$Plot == "Parque Natural Norte", "Longitude"] <- -6.23
data[data$Plot == "Salinas de Bonanza", "Latitude"] <- 36.87
data[data$Plot == "Salinas de Bonanza", "Longitude"] <- -6.32

# Remove observations without specific plot
data <- data %>% filter(!Plot %in% "")

# Add sample description
data$SampleDescription <- as.factor(with(data, paste(Day, Month, Year, gsub(" ", "_", Plot), sep = "_")))
length(levels(data$SampleDescription))

# Add empty columns
data$Biomass <- rep(NA, nrow(data))
data$DepthElevation <- rep(NA, nrow(data))
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
write.table(data, "output/EBD_aerial_counts.csv", sep = ";", dec = ".", row.names = FALSE)
