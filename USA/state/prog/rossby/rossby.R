rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# add sourced data
source('../../data/objects/objects.R')

# create directories for output
file.loc <- paste0('../../output/rossby/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

residuals  <- function(dat,age.sel,sex.sel,fips.sel){

    # subset data for particular sex, age and state
    dat.sub <- subset(dat,age==age.sel & sex==sex.sel & fips==fips.sel)

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

    # find residuals remaining
    residuals <- rates /( trend * seasonal)

    return(residuals)
}

# isolate a western state and an eastern state
resid.ca <- residuals(dat,75,1,6)
resid.ny <- residuals(dat,75,1,36)

# export pdf
pdf(paste0(file.loc,'resid_ny_ca_test.pdf'),paper='a4r',height=0,width=0)
plot(resid.ca,resid.ny)
dev.off()