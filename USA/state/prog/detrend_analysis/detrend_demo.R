rm(list=ls())

library(forecast)
library(foreign)
library(plyr)
library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# add sourced data
source('../../data/objects/objects.R')

# create directories for output
file.loc <- paste0('../../output/detrend/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

# unique fips code
fips.lookup <- sort(unique(dat$fips))
age.lookup <- sort(unique(dat$age))

# empty dataframe to fill with loop below
dat.seasonal <- data.frame(age=numeric(0), sex=numeric(0), fips=numeric(0),month=numeric(0),amplitude=numeric(0))

# subset data for particular sex, age and state
dat.sub <- subset(dat,age==75 & sex==2 & fips==6)

# isolate the time series of death rates
rates <- dat.sub$rate.adj

# establish the trend so it can be extracted from time series
# fixed season of 12
trend = ma(rates, order=12, centre=T)

# find detrended time series
detrend = rates/trend

# plot and save
pdf(paste0(file.loc,'detrended_demo.pdf'),paper='a4r',height=0,width=0)

# raw rates with trend
plot(rates,t='l',col='red')
lines(trend,col='blue')

# detrended seasonal component
plot(detrend,col='green')
abline(a=1,b=0)

dev.off()
