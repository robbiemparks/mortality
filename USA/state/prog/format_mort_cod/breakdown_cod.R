rm(list=ls())

library(foreign)
library(readr)
library(dplyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
print(year)

# read files
dat = read.dta(paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))

if(year<1999){
    # SOMETHING FOR ICD 9
}
if(year>=1999){

    ########################################
    ### 1. breakdown of cod by letter
    ########################################

    # strip letter from front of cause
    dat$letter = substr(dat$cause,1,1)

    # establish the percentage of deaths
    dat.count = count(dat,letter)
    dat.count$percentage = with(dat.count,(n/sum(n))*100)

    ########################################
    ### 2. breakdown of cod by gbd
    ########################################
}

