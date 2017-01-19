rm(list=ls())

library(foreign)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
state.arg <- as.character(args[3])

# load data
filename <- paste0('~/data/mortality/US/state/processed/county/deaths',year.start.arg,'.dta')
dat <- read.dta(filename)

# create state and county fips
dat$state.fips <- substr(dat$fips,1,2)
dat$county.fips <- substr(dat$fips,3,5)

# filter by chosen state and create unique county fips list
county.fips <- subset(dat,state.fips==state.arg)
county.fips <- data.frame(new=sort(unique(county.fips$county.fips)))

# rename column to include year
colnames(county.fips)[ncol(county.fips)] <- paste0('county.fips.',year.start.arg)


# define years to loop over
loop.years <- c((year.start.arg+1):year.end.arg)

for(year.current in loop.years){

    # load data
    filename <- paste0('~/data/mortality/US/state/processed/county/deaths',year.current,'.dta')
    dat <- read.dta(filename)

    # create state and county fips
    dat$state.fips <- substr(dat$fips,1,2)
    dat$county.fips <- substr(dat$fips,3,5)

    # filter by chosen state and create unique county fips list
    temp <- subset(dat,state.fips==state.arg)
    temp <- data.frame(new=sort(unique(county.fips$county.fips)))

    colnames(temp)[ncol(temp)] <- paste0('county.fips.',year.current)
    temp$dummy <- temp[,ncol(temp)]
    
    test <- merge(county.fips,temp,by.x=('dummy'),by.y=(paste0('dummy')),all.x=TRUE,all.y=TRUE)
    test$dummy <- NULL
}
