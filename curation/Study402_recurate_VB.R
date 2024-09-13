## Check 402
## curator: VB
## revision1 stage

library(dplyr)
library(ggplot2)
library(clipr)
require(stringr)

#### read database

BT <- readRDS("queryBTv2_April_2024.rds")
meta <- read.csv("metadataBTv2_April_2024.csv")

names(BT)
names(meta)

st_id <- 402

mt <- meta[meta$STUDY_ID == st_id,]
raw <- BT[BT$STUDY_ID == st_id,]
mt$SAMPLE_DESC_NAME
mt$COMMENTS
mt$CURATOR
mt$METHODS

names(raw)

names(raw) <- c("ID_ALL_RAW_DATA", "Abundance", "Biomass","ID_SPECIES" , "SampleDescriptionO", "Plot","Latitude",
                "Longitude", "Depth", "Day", "Month", "Year", "StudyID", "newID", "valid_name","resolution","taxon")
unique(raw$valid_name)
raw$Genus <- word(raw$valid_name, 1)
raw$Species <- word(raw$valid_name, 2)

raw <- as.data.frame(raw %>% mutate(Family = '', DepthElevation = '', SampleDescription = paste(Year, Plot , sep="_")))


dt_check <- raw %>% group_by_at(vars(-Abundance)) %>%
  summarise(Abundance=sum(Abundance)) %>% ungroup()
nrow(dt) - nrow(dt_check) # no differences in size - assuming it was a mistake.

dt_merged <- raw[,c("Abundance",
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
                    "StudyID","valid_name",
                    "ID_SPECIES", "newID")]
View(dt_merged) # final check :)

clipr::write_clip(dt_merged)
mt$COMMENTS
mt$DATES_CHANGED
mt$LOC_ADDED
