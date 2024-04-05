library(tidyverse)
library(taxize)
library(worrms)

spdb<-readRDS("species_list_BT.rds")

### for marine species using worrms

### worrms package does not accept large datasets so may need to 
### run checks in stages

spn<-as.data.frame(spndb %>% slice(1:150)) 

t1<-wm_records_names(name=spn6$valid_name, marine_only=FALSE, fuzzy=FALSE)
t2<-data.frame(do.call(rbind.data.frame, t1))
t3<-select(t2, scientificname, valid_name, kingdom, phylum, 
             class, order, family, genus, match_type)

saveRDS(t3, "species_worrms.rds")

### or for very large datasets can use a loop

i<-1
df<-data.frame()

for(j in 1:length(spndb)) {
  k<-i+149
  sp<-as.data.frame(spndb %>% slice(i:k))
  t1<-wm_records_names(name=sp$Species , fuzzy=FALSE)
  t2<-data.frame(do.call(rbind.data.frame, t1))
  t3<-select(t2, scientificname, valid_name, kingdom, phylum, 
               class, order, family, genus, match_type)
  sp$scientificname<-sp$Species
  spx<-merge(sp, t3, by="scientificname")
  df<-rbind(df, spx)
  i<-i+150
}

saveRDS(df, "species_worrms.rds")


### for the non-marine species - using taxize and the GBIF database

### this is less problematic with larger datasets


t1<-classification(rf$origSpecies, db="gbif", rows=1)
t2<-data.frame(do.call(rbind.data.frame, t1))
t2$species<-gsub("\\..*","", rownames(t2))
t3<-select(t2, rank, species, name)
rownames(t3)<-NULL
t4<-as.data.frame(t3 %>% distinct(rank, species, name))
colnames(t4)<-c("rank", "Species", "name")
t5<-as.data.frame(t4 %>% group_by(Species) %>% spread(rank, name))
t6<-merge(spndb, t5, by="Species")

saveRDS(t6, "species_taxize.rds")


