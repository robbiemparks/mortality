rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
age.break.arg <- as.numeric(args[3])

# library(dplyr)
library(plyr)

# source files
source('../../data/objects/objects.R')

# load original output file as RDS
dat = readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))

# add tag of whether age over chosen age or under
dat$age.tag = ifelse(dat$age<age.break.arg,0,age.break.arg)

# resummarise over new age banding
dat.merged = ddply(dat,.(sex,age.tag,year,month,fips,cause),summarise,deaths=sum(deaths),deaths.adj=sum(deaths.adj),pop=sum(pop),pop.adj=sum(pop.adj))

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths.adj / dat.merged$pop.adj

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod"), dir.create("../../output/prep_data_cod"), FALSE)

# output file as RDS
saveRDS(dat.merged,paste0('../../output/prep_data_cod/datus_state_rates_cod_age_split',age.break.arg,'_',year.start.arg,'_',year.end.arg))
