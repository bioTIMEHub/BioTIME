#######################################################################
# Curation Script: Ammend Study 234 (PART I, data reading)
# Curator: AFE
# Date: April 2023
# NOTE: this script is only to ensure data are properly read from the 
# online source. For the curation steps please 
# see script: STUDY_234_II_AFE.R
#######################################################################


# Libraries:-----------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(stringr)
library(lubridate)

rm(list=ls())

#Read data (BT & raw): -------------------------------------------------
mypath <- getwd()     
mypath


btv1 <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/BIOTIME/BioTIMENewStudiesCuration/0_BioTIMEv1Revision"

# Rawdata: -------------------------------------------------------------
# (code for data reading partially extracted from https://portal.edirepository.org/)

# 1965:-----------------------------------------------------------------
dt1965 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_1965veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                    "Seq","Tag","Dbh","Vigor","AbvBmss",     
                    "BlwBmss","TwotoTen","v_10Area","TwotoTenArea"), check.names=TRUE)
dt1 <- dt1965

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))

dt1965 <- dt1
dt1965$year <- rep(1965, nrow(dt1965))

# 1977:-----------------------------------------------------------------
dt1977 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_1977veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea"), check.names=TRUE)
dt1 <- dt1977

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))

dt1977 <- dt1
dt1977$year <- rep(1977, nrow(dt1977))

# 1982:-----------------------------------------------------------------
dt1982 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_1982veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea"), check.names=TRUE)

dt1 <- dt1982

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))

dt1982 <- dt1
dt1982$year <- rep(1982, nrow(dt1982))

# 1987:-----------------------------------------------------------------
dt1987 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_1987veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea"), check.names=TRUE)

dt1 <- dt1987

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))


dt1987 <- dt1
dt1987$year <- rep(1987, nrow(dt1987))

# 1992:-----------------------------------------------------------------
dt1992 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_1992veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea"), check.names=TRUE)

dt1 <- dt1992

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))

dt1992 <- dt1
dt1992$year <- rep(1992, nrow(dt1992))

# 1997:-----------------------------------------------------------------
dt1997 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_1997veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea"), check.names=TRUE)

dt1 <- dt1997

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))

dt1997 <- dt1
dt1997$year <- rep(1997, nrow(dt1997))

# 2002:-----------------------------------------------------------------
dt2002 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_2002veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea",
                                "ElevB","AnalysisCode"), check.names=TRUE)

dt1 <- dt2002

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)
if (class(dt1$ElevB)!="factor") dt1$ElevB<- as.factor(dt1$ElevB)
if (class(dt1$AnalysisCode)!="factor") dt1$AnalysisCode<- as.factor(dt1$AnalysisCode)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))


dt2002 <- dt1
dt2002$year <- rep(2002, nrow(dt2002))

# 2007:-----------------------------------------------------------------
dt2007 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w62007veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea",
                                "ElevB","AnalysisCode"), check.names=TRUE)

dt1 <- dt2007

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)
if (class(dt1$ElevB)!="factor") dt1$ElevB<- as.factor(dt1$ElevB)
if (class(dt1$AnalysisCode)!="factor") dt1$AnalysisCode<- as.factor(dt1$AnalysisCode)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))

dt2007 <- dt1
dt2007$year <- rep(2007, nrow(dt2007))

# 2012:-----------------------------------------------------------------
dt2012 <-read.table(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w62012veg.txt"),
                    header=F,skip=1,sep=",", 
                    col.names=c("Plot","Zone","Species","SppNum",     
                                "Seq","Tag","Dbh","Vigor","AbvBmss",     
                                "BlwBmss","TwotoTen","v_10Area","TwotoTenArea",
                                "ElevB","AnalysisCode", "bbd"), check.names=TRUE)

dt1 <- dt2012

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)
if (class(dt1$ElevB)!="factor") dt1$ElevB<- as.factor(dt1$ElevB)
if (class(dt1$AnalysisCode)!="factor") dt1$AnalysisCode<- as.factor(dt1$AnalysisCode)
if (class(dt1$bbd)!="factor") dt1$bbd<- as.factor(dt1$bbd)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("0")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("0"))),NA,dt1$TwotoTenArea))


dt2012 <- dt1
dt2012$year <- rep(2012, nrow(dt2012))

# 2017:-----------------------------------------------------------------
dt2017 <-read.csv(file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/w6_2017_vegInventory.csv"),
                  header=F, skip=1, sep=",", quot='"', col.names=c("Plot","Zone",     
                    "Species","SppNum","Seq","Tag","Dbh","Vigor","AbvBmss","BlwBmss",     
                    "TwotoTen","v_10Area","TwotoTenArea","ElevB","AnalysisCode","bbd"), 
                  check.names=TRUE)

dt1 <- dt2017

if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Zone)!="factor") dt1$Zone<- as.factor(dt1$Zone)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$SppNum)!="factor") dt1$SppNum<- as.factor(dt1$SppNum)
if (class(dt1$Seq)!="factor") dt1$Seq<- as.factor(dt1$Seq)
if (class(dt1$Tag)!="factor") dt1$Tag<- as.factor(dt1$Tag)
if (class(dt1$Dbh)=="factor") dt1$Dbh <-as.numeric(levels(dt1$Dbh))[as.integer(dt1$Dbh) ]               
if (class(dt1$Dbh)=="character") dt1$Dbh <-as.numeric(dt1$Dbh)
if (class(dt1$Vigor)!="factor") dt1$Vigor<- as.factor(dt1$Vigor)
if (class(dt1$AbvBmss)=="factor") dt1$AbvBmss <-as.numeric(levels(dt1$AbvBmss))[as.integer(dt1$AbvBmss) ]               
if (class(dt1$AbvBmss)=="character") dt1$AbvBmss <-as.numeric(dt1$AbvBmss)
if (class(dt1$BlwBmss)=="factor") dt1$BlwBmss <-as.numeric(levels(dt1$BlwBmss))[as.integer(dt1$BlwBmss) ]               
if (class(dt1$BlwBmss)=="character") dt1$BlwBmss <-as.numeric(dt1$BlwBmss)
if (class(dt1$TwotoTen)!="factor") dt1$TwotoTen<- as.factor(dt1$TwotoTen)
if (class(dt1$v_10Area)=="factor") dt1$v_10Area <-as.numeric(levels(dt1$v_10Area))[as.integer(dt1$v_10Area) ]               
if (class(dt1$v_10Area)=="character") dt1$v_10Area <-as.numeric(dt1$v_10Area)
if (class(dt1$TwotoTenArea)=="factor") dt1$TwotoTenArea <-as.numeric(levels(dt1$TwotoTenArea))[as.integer(dt1$TwotoTenArea) ]               
if (class(dt1$TwotoTenArea)=="character") dt1$TwotoTenArea <-as.numeric(dt1$TwotoTenArea)
if (class(dt1$ElevB)!="factor") dt1$ElevB<- as.factor(dt1$ElevB)
if (class(dt1$AnalysisCode)!="factor") dt1$AnalysisCode<- as.factor(dt1$AnalysisCode)
if (class(dt1$bbd)!="factor") dt1$bbd<- as.factor(dt1$bbd)

# Convert Missing Values to NA for non-dates

dt1$Tag <- as.factor(ifelse((trimws(as.character(dt1$Tag))==trimws("0")),NA,as.character(dt1$Tag)))
dt1$v_10Area <- ifelse((trimws(as.character(dt1$v_10Area))==trimws("0")),NA,dt1$v_10Area)               
suppressWarnings(dt1$v_10Area <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$v_10Area))==as.character(as.numeric("0"))),NA,dt1$v_10Area))
dt1$TwotoTenArea <- ifelse((trimws(as.character(dt1$TwotoTenArea))==trimws("1")),NA,dt1$TwotoTenArea)               
suppressWarnings(dt1$TwotoTenArea <- ifelse(!is.na(as.numeric("1")) & (trimws(as.character(dt1$TwotoTenArea))==as.character(as.numeric("1"))),NA,dt1$TwotoTenArea))

dt2017 <- dt1
dt2017$year <- rep(2017, nrow(dt2017))

#######################################################################
# Merge:---------------------------------------------------------------

wt6veg <- bind_rows(dt1965, dt1977, dt1982, dt1987, dt1992, dt1997, dt2002, dt2007, dt2012, dt2017)
save(wt6veg, file=paste0(btv1, "/STUDY_234_AFE/RawData_retrieved_2023/wt6veg.RData"))

#End of script ########################################################
