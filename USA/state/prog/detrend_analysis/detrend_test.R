rm(list=ls())

library(foreign)
library(plyr)
library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
#sex <- as.numeric(args[3])
#age <- as.numeric(args[4])

# load data
dat <- readRDS('datus_state_rates_1980_2013')
#dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

# unique fips code
fips.lookup <- sort(unique(dat$fips))
age.lookup <- sort(unique(dat$age))

dat.seasonal <- data.frame(age=numeric(0), sex=numeric(0), fips=numeric(0),month=numeric(0),amplitude=numeric(0))

# function to loop through each age,sex and all states
for(i in age.lookup) {
    for(j in c(1,2)) {
        for(k in fips.lookup) {

            # subset data for particular sex, age and state
            dat.sub <- subset(dat,age==i & sex==j & fips==k)
            #dat.sub <- subset(dat,sex==sex & age==age & fips==7)

            # isolate the time series of death rates
            rates <- dat.sub$rate.adj

            # establish the trend so it can be extracted from time series
            # fixed season of 12
            trend = ma(rates, order=12, centre=T)

            # find detrended time series
            detrend = rates/trend

            # create a matrix of death rates, each row representing one period (12 months)
            m = t(matrix(data = detrend, nrow = 12))

            # find average seasonality of each column
            seasonal = colMeans(m, na.rm = T)

            # create dataframe to append
            dummy <- data.frame(age=i, sex=j, fips=k, month=c(1:12), amplitude=seasonal)

            # add to dataframe with
            dat.seasonal <- rbind(dat.seasonal,dummy)
        }
    }
}

# plot

ggplot() +
geom_line(data=subset(dat.seasonal,sex==1),color='forestgreen',aes(x=month,y=amplitude,group=as.factor(fips))) +
guides(color=FALSE) +
geom_hline(yintercept=1, lintype=2) +
facet_wrap(~age,scales='free') +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short) +
theme_bw()

ggplot() +
geom_line(data=subset(dat.seasonal,sex==2),color='forestgreen',aes(x=month,y=amplitude,group=as.factor(fips))) +
guides(color=FALSE) +
geom_hline(yintercept=1) +
facet_wrap(~age,scales='free') +
theme_bw()
