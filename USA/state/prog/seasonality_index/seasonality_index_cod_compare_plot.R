rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
# cod <- as.character(args[7])

#year.start = 1980 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean'

# length of analysis period
num.years <- year.end - year.start + 1

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
# DIRECTORY CREATION AND DATA
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
file.loc.regional <- paste0('../../output/seasonality_index/regional/')
ifelse(!dir.exists(file.loc.regional), dir.create(file.loc.regional, recursive=TRUE), FALSE)

# 1. ORIGINAL METHOD

# load files for all cause and main sub-causes
lin.reg.grad.weight = data.frame()
for(i in cod.broad){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight = rbind(lin.reg.grad.weight,dat.temp)
}
lin.reg.grad.weight$cause <- gsub('AllCause', 'All cause', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('External', 'Injuries', lin.reg.grad.weight$cause)
lin.reg.grad.weight$age = as.numeric(lin.reg.grad.weight$age)

# load files for cardio sub-causes
lin.reg.grad.weight.cardio = data.frame()
for(i in cod.cardio){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight.cardio = rbind(lin.reg.grad.weight.cardio,dat.temp)
}
lin.reg.grad.weight.cardio$age = as.numeric(lin.reg.grad.weight.cardio$age)

# load files for injury sub-causes
lin.reg.grad.weight.injury = data.frame()
for(i in cod.injuries){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight.injury = rbind(lin.reg.grad.weight.injury,dat.temp)
}
lin.reg.grad.weight.injury$age = as.numeric(lin.reg.grad.weight.injury$age)

# load files for other sub-causes
lin.reg.grad.weight.other = data.frame()
for(i in cod.other){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight.other = rbind(lin.reg.grad.weight.other,dat.temp)
}
lin.reg.grad.weight.other$age = as.numeric(lin.reg.grad.weight.other$age)

dat.old = rbind(lin.reg.grad.weight, lin.reg.grad.weight.cardio, lin.reg.grad.weight.injury, lin.reg.grad.weight.other)

# 2. NEW METHOD (RECOMMENDED BY ELIFE REVISIONS)

# load files for all cause and main sub-causes
lin.reg.grad.weight = data.frame()
for(i in cod.broad){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight = rbind(lin.reg.grad.weight,dat.temp)
}
lin.reg.grad.weight$cause <- gsub('AllCause', 'All cause', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('External', 'Injuries', lin.reg.grad.weight$cause)
lin.reg.grad.weight$age = as.numeric(lin.reg.grad.weight$age)
lin.reg.grad.weight$pvalue = lin.reg.grad.weight$`p-value`


# load files for cardio sub-causes
lin.reg.grad.weight.cardio = data.frame()
for(i in cod.cardio){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight.cardio = rbind(lin.reg.grad.weight.cardio,dat.temp)
}
lin.reg.grad.weight.cardio$age = as.numeric(lin.reg.grad.weight.cardio$age)
lin.reg.grad.weight.cardio$pvalue = lin.reg.grad.weight.cardio$`p-value`


# load files for injury sub-causes
lin.reg.grad.weight.injury = data.frame()
for(i in cod.injuries){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight.injury = rbind(lin.reg.grad.weight.injury,dat.temp)
}
lin.reg.grad.weight.injury$age = as.numeric(lin.reg.grad.weight.injury$age)
lin.reg.grad.weight.injury$pvalue = lin.reg.grad.weight.injury$`p-value`

# load files for other sub-causes
lin.reg.grad.weight.other = data.frame()
for(i in cod.other){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight.other = rbind(lin.reg.grad.weight.other,dat.temp)
}
lin.reg.grad.weight.other$age = as.numeric(lin.reg.grad.weight.other$age)
lin.reg.grad.weight.other$pvalue = lin.reg.grad.weight.other$`p-value`

dat.new = rbind(lin.reg.grad.weight, lin.reg.grad.weight.cardio, lin.reg.grad.weight.injury, lin.reg.grad.weight.other)
