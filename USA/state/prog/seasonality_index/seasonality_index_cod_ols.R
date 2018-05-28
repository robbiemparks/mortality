rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# correct location to start at
setwd('~/git/mortality/USA/state/prog/00_bash')

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cod <- as.character(args[7])

#year.start = 1980 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean'
#cod ='Cardiopulmonary'

# length of analysis period
num.years <- year.end - year.start + 1

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
if(cod!='AllCause'){
    dat <- subset(dat,cause==cod)
}

# fix names of causes
dat$cause <- gsub('Allcause', 'All cause', dat$cause)
dat$cause <- gsub('External', 'Injuries', dat$cause)
dat$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause)

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
# DATA PROCESSING
###############################################################

# 1. NATIONAL

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# model suggested by eLife is the following
# --------------------- -------------------------------------
# Yit∼ Poisson(Nit ⋅ λit)
# log(λit)= μi + βit + Mit ⋅ [α1i ⋅ cos(2πt/12)
# +α2i ⋅ sin(2πt/12)+α3i ⋅ cos(2πt/6)
# +α4i ⋅ sin(2πt/6)]
# Mit= ρi + γit
# --------------------- -------------------------------------

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.national[,c('year', 'month')])
dat.year.month <- dat.year.month[order(dat.year.month$year,dat.year.month$month),]
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.national <- merge(dat.national,dat.year.month, by=c('year','month'))
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# add sinusoidal terms
dat.national$cos.12 = with(dat.national,cos(2*pi*month/12))
dat.national$sin.12 = with(dat.national,sin(2*pi*month/12))
dat.national$cos.6  = with(dat.national,cos(2*pi*month/6))
dat.national$sin.6  = with(dat.national,sin(2*pi*month/6))

# apply Poisson glm with population offsetting
dat.pois.summary <- ddply(dat.national,.(sex,age), function(z)coef(summary(glm(deaths.pred ~ 1 + year.month + (1 + year)*(cos(2*pi*year.month/12)+sin(2*pi*year.month/12)+cos(2*pi*year.month/6)+sin(2*pi*year.month/6)), offset=log(pop.adj),family=poisson(link="log"),data=z))))

# testing below
# dat.national.test = subset(dat.national,age==0&sex==1)
#dat.pois.summary = glm(deaths.pred ~ 1 + year.month + (1 + year)*(cos(2*pi*month/12)+sin(2*pi*month/12)+cos(2*pi*month/6)+sin(2*pi*month/6)), offset=log(pop.adj),family=poisson(link="log"),data=dat.national.test)