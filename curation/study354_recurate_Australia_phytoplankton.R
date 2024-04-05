

# Curation Script ---------------------------------------------------------

# Dataset: The Australian Phytoplankton Database (1844 - ongoing) - abundance and biovolume 
# Location: Australia
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(maps)
require(stringr)
require(lubridate)
require(readr)

rm(list=ls())

dt <- readr::read_csv('The_Australian_Phytoplankton_Database.csv')
ogdt <- readr::read_csv('The_Australian_Phytoplankton_Database.csv')

# Database needs to split by project ID and then filter some of them out
# use metadata summaries to identify filter
meta <- read.csv('s_study_Davies.csv', header = T)
sort(unique(meta$Comment.taxonomic.resolution.))
# dt <- readr::read_csv_chunked('Originals/study354/The_Australian_Phytoplankton_Database.csv', chunk_size = 10000)

# vector key for project IDs to remove based on assemblage criteria violation
prj_remove <- meta %>% filter(str_detect(Comment.taxonomic.resolution., 'only')) %>% pull(Sample.Name)
sort(unique(meta$Comment.sampling.date.range.))
meta$year_start <- str_extract(meta$Comment.sampling.date.range., '^[:digit:]{4}') %>% as.numeric()
meta$year_end <- str_extract(meta$Comment.sampling.date.range., '(?<=\\/)[:digit:]{4}') %>% as.numeric()

meta <- meta %>% select(Sample.Name, Comment.taxonomic.resolution., 
                        contains('latitude'), contains('longitude'), year_start, year_end) %>% 
  mutate(year_duration = year_end - year_start + 1)
colnames(meta)[1:6] <- c('projectID', 'taxonomic_res', 'Latitude_S', 'Latitude_N', 'Longitude_W', 'Longitude_E')

prj_remove <- meta %>% filter(year_duration == 1) %>% pull(projectID) %>% append(prj_remove, .) # add projectIDs that aren't long enough

# check that the data contains the same project IDs as metadata table
n_distinct(dt$PROJECT_ID) == n_distinct(meta$projectID)
sort(unique(dt$PROJECT_ID)) == sort(unique(meta$projectID))

dt <- dt %>% filter(!PROJECT_ID %in% prj_remove) # filter out by abundance, assemblage and year criteria
length(unique(dt$PROJECT_ID)) # left with 35 projects
dt %>% filter(PRESENCE_ABSENCE == F) %>% head(100) %>% View
dt <- dt %>% filter(PRESENCE_ABSENCE == T)

summary(dt)
# check that each project has a consistent method for any more filtering
dt %>% distinct(PROJECT_ID, SAMPLE_METHOD) %>% View
dt %>% distinct(PROJECT_ID, PROJECT_DESCRIPTION) %>% View

metadata <- dt %>% distinct(CUSTODIAN_DETAILS, PROJECT_ID, PROJECT_DESCRIPTION, IDENTIFICATION_METHOD, SAMPLE_METHOD, PRESERVATIVE, CITATION) %>% 
  inner_join(meta, ., by = c('projectID' = 'PROJECT_ID'))
metadata <- metadata %>% filter(!projectID == 591)
dt <- dt %>% filter(!PROJECT_ID == 591) # remove single species dataset

write.csv(metadata, 'projectmetadata.csv', row.names = F)
rm(meta, prj_remove)

# Structure check ---------------------------------------------------------

# curate all 35 together and then split at the end

colnames(dt) <- str_to_title(colnames(dt))
dt <- dt %>% select(Project_id, Cells_l, Biovolume_um3_l, Taxon_name, Family, Genus, Species,
                    Sample_id, Location, Latitude, Sample_depth, Longitude, 
                    Sample_year, Sample_month, Sample_day, Biovolume_shape)

dt %>% group_by(project_id) %>% summarise(n()) %>% View

str(dt) # check structure
dt$Sample_month <- as.numeric(dt$Sample_month)
dt$Sample_day <- as.numeric(dt$Sample_day)
summary(dt)

dt %>% filter(is.na(Sample_year)) %>% View
dt <- dt[dt$Project_id != 553, ] # remove project 553

# Abundance and/or biomass, latitude and longitude numeric? Y
# Year, month and day must be integers or factors? Y
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
dt %>% group_by(Project_id) %>% distinct(Project_id, Location) %>% View
# location not as meaningful as a sampling event field
dt %>% group_by(Project_id) %>% summarise(samples = n_distinct(Sample_id)) %>% View
dt$Sample_id <- as.factor(dt$Sample_id)
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

dt <- dt %>% rename(Abundance = Cells_l, Biomass = Biovolume_um3_l, DepthElevation = Sample_depth)
colnames(dt) <- str_remove_all(colnames(dt), '^Sample\\_')

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance, na.rm = T) > 0 # check the minimum (no zeroes) Y
dt %>% filter(is.na(Abundance), is.na(Biomass)) %>% View # look at NAs

# proportion of records with NA Abundance or Biomass
dt %>% group_by(Project_id) %>% 
  summarise(AbundanceNA = sum(is.na(Abundance)+0)/n(),
            BiomassNA = sum(is.na(Biomass)+0)/n()) %>% View

remove <- dt %>% group_by(Project_id) %>% 
  summarise(AbundanceNA = sum(is.na(Abundance)+0)/n(),
            BiomassNA = sum(is.na(Biomass)+0)/n()) %>% 
  filter(AbundanceNA == 1, BiomassNA == 1) %>% pull(Project_id)

dt <- dt %>% filter(!Project_id %in% remove)
n_distinct(dt$Project_id) # left with 21 projects

dt[(is.na(dt$Abundance) == T & is.na(dt$Biomass) == T),] %>% summary # 4379 records with NAs in both primary fields
dt <- dt[-which(is.na(dt$Abundance) == T & is.na(dt$Biomass) == T),]

dt %>% filter(is.na(Abundance), is.na(Biomass)) %>% View # check

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[c('year', 'month', 'day')]) # looks good to me

# LAT LONG
# no blanks, no NAs
sum(dt[c('Latitude', 'Longitude')] == "") == 0
sum(is.na(dt[c('Latitude', 'Longitude')]) + 0) == 0

# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')])

require(ggplot2)
world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
world + geom_point(data=dt %>% distinct(Longitude, Latitude, year), 
                   aes(x=Longitude, y=Latitude, color = year), shape=21) + 
  coord_fixed(xlim=c(80, 190), ylim=c(-70, -5))

world + geom_point(data=dt %>% distinct(Project_id, Longitude, Latitude) %>% mutate(Project_id = as.factor(Project_id)), 
                   aes(x=Longitude, y=Latitude, color = Project_id), shape=21) + 
  coord_fixed(xlim=c(80, 190), ylim=c(-70, -5))

rm(list=c('world_map', 'world', 'points', 'points_zoom'))

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Taxon_name) + 0) == 0
sum(is.na(dt$Family) + 0) == 0

dt %>% filter(is.na(Taxon_name), is.na(Family)) %>% View
dt <- dt[-which(is.na(dt$Taxon_name), is.na(dt$Family)),]
dt %>% filter(is.na(Taxon_name) == F, is.na(Family)) %>% View
dt$Taxon_name[which(is.na(dt$Family) == F)] <- NA # only keep the uncertain ones

sort(unique(dt$Taxon_name)) # clean low resolution records
dt <- dt[-str_which(dt$Taxon_name, '(filaments)'),]
dt$Taxon_name <- str_remove_all(dt$Taxon_name, 'Unid')
dt$Taxon_name <- str_remove_all(dt$Taxon_name, '(\\<|\\>)\\s*[:digit:]+\\s*(um|Âµm|µm)(\\s*[:alnum:]+)*$|[:digit:]+\\s*\\-\\s*[:digit:]+\\s*(um|Âµm|µm)(\\s*[:alnum:]+)*')
dt$Taxon_name <- str_remove_all(dt$Taxon_name, '^\\s|\\s$')
dt$Taxon_name[-str_which(dt$Taxon_name, '\\s')] <- str_to_title(dt$Taxon_name[-str_which(dt$Taxon_name, '\\s')])
dt$Family[which(is.na(dt$Family))] <- dt$Taxon_name[which(is.na(dt$Family))]
# remove any size distinctions but keep morphology words

sort(unique(paste(dt$Genus, dt$Species)))
dt$Species <- str_remove(dt$Species, '(?<=\\s)f. [:alnum:]+$|\\scyst(?=\\s)|\\s\\(([:alnum:]+\\s*)+\\)')
dt$Species <- str_replace(dt$Species, 'spp\\.', 'sp')
sort(unique(paste(dt$Genus, dt$Species)))

sort(unique(dt$Family))
dt$Family <- str_remove(dt$Family, 'Cyanobacteral filament')
dt$Taxon_name <- NULL

# Sampling effort and structure check -------------------------------------------

dt %>% group_by(Project_id) %>% summarise(sampleN = length(unique(id))) %>% View
dt %>% group_by(Project_id, year) %>% summarise(sampleN = length(unique(id))) %>% View # sampling effort (annual)
dt %>% group_by(Project_id, year) %>% summarise(sampleN = length(unique(id)), days = length(unique(paste0(month, day)))) %>% View # sampling effort (annual)
dt %>% group_by(Project_id, year, month, day) %>% summarise(length(unique(id))) %>% View()

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())

# visualise by project
for (i in sort(unique(dt$Project_id))) {
  tempdt <- dt %>% filter(Project_id == i) %>% 
    distinct(id, Longitude, Latitude, year)
  p <- world + geom_point(data = tempdt, 
                     aes(x = Longitude, y = Latitude, color = year), shape = 21, alpha = 0.6) + 
    coord_fixed(xlim=c(min(tempdt$Longitude) - 5, max(tempdt$Longitude) + 5), 
                ylim=c(min(tempdt$Latitude) - 5, max(tempdt$Latitude) + 5)) +
    labs(title = paste('Project', i)) + scale_color_viridis_c()
  print(p)
}

# projects with unequal spatial sampling effort through time: 
# 1068, 806, 597, 1053, 597

# just 597
tempdt <- dt %>% filter(Project_id == 597) %>% 
  distinct(id, Longitude, Latitude, year)
world + geom_point(data = tempdt, 
                   aes(x = Longitude, y = Latitude, color = year), shape = 21, size = 2, alpha = 0.6) + 
  coord_fixed(xlim=c(min(tempdt$Longitude) - 5, max(tempdt$Longitude) + 5), 
              ylim=c(min(tempdt$Latitude) - 5, max(tempdt$Latitude) + 5)) +
  labs(title = paste('Project', 597)) +
  scale_color_continuous(type = 'viridis')

rm(list=c('world_map', 'world', 'points', 'tempdt'))

# keep these uneven spatial samples but put a big cautionary note in methods/curation table.

# Prepare raw data --------------------------------------------------------

# aggregate abundance records that are same species, plot, and survey day.
colnames(dt) <- str_to_title(colnames(dt))
dt_merged <- dt %>% select(!c(Biovolume_shape, Location)) %>% 
  group_by(across(-c(Abundance, Biomass))) %>% 
  summarise(Abundance=sum(Abundance), Biomass = sum(Biomass)) %>% ungroup() %>% 
  rename(SampleDescription = Id, DepthElevation = Depthelevation) %>% mutate(Plot = '') %>% 
  arrange(Project_id, SampleDescription, Year, Month, Day, Family, Genus, Species) %>% 
  relocate(Project_id, Abundance, Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year)
  
nrow(dt) - nrow(dt_merged) # any change in aggregating? 2571 records
# now create empty columns needed to fit to template

dataset = 'study354_split_Aus_phytoplankton'
curator = 'CC'

for (i in sort(unique(dt_merged$Project_id))) {
  # split each project ID and save as rawdata
  dt_merged %>% filter(Project_id == i) %>% 
    select(!Project_id) %>% 
    write.csv(., paste(dataset, i, curator, 'rawdata.csv', sep = "_"), row.names = F)
}

# Prepare metadata --------------------------------------------------------


### Spatial Geometry Calculations for BioTIME datasets
# paste this chunk into the end of your curation script

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
meta <- dt_merged %>% distinct(Project_id, Longitude, Latitude) %>% 
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  st_transform(st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>% 
  group_by(Project_id) %>% filter(n() > 2) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(area = st_convex_hull(geometry) %>% st_area()) %>% st_drop_geometry() %>% 
  left_join(metadata, ., by = c("projectID" = "Project_id"))

meta <- meta %>% select(!c(X, Latitude_cent, Longitude_cent))

meta <- dt_merged %>% distinct(Project_id, Longitude, Latitude) %>% 
    st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
    group_by(Project_id) %>% 
    summarise(centroid = st_union(geometry) %>% st_centroid()) %>% 
    mutate(Latitude = st_coordinates(centroid)[,2], 
           Longitude = st_coordinates(centroid)[,1]) %>% st_drop_geometry() %>% 
    left_join(meta, ., by = c("projectID" = "Project_id"))
  
write.csv(meta, 'projectmetadata.csv', row.names = F)  
