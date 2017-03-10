rm(list=ls())

library(AnomalyDetection)
library(forecast)
library(foreign)
library(plyr)
library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# add sourced data
# maybe add a library source file?
source('../../data/objects/objects.R')

# create directories for output
file.loc <- paste0('../../output/anomaly/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

# unique fips code and ages
fips.lookup <- sort(unique(dat$fips))
age.lookup <- sort(unique(dat$age))

pdf(paste0(file.loc,'anomaly_test.pdf'),paper='a4r',height=0,width=0)

# function to loop through each age,sex and all states
for(i in age.lookup) {
    for(j in c(1,2)) {
        for(k in fips.lookup) {

            # subset data for particular sex, age and state
            dat.sub <- subset(dat,age==i & sex==j & fips==k)
            #dat.sub <- subset(dat,sex==sex & age==age & fips==7)

            # isolate the time series of death rates
            rates <- as.data.frame(dat.sub$rate.adj)

            res = AnomalyDetectionVec(rates, max_anoms=0.02, direction='both', period=12, plot=TRUE)

            # plot results
            # create a grid to
            print(res$plot)
        }
    }
}

dev.off()