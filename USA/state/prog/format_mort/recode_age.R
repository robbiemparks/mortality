rm(list=ls())

library(foreign)
library(readr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
file.type <- as.character(args[2])

# read file
dat <- read.dta(paste0('~/data/mortality/US/state/raw/cdc/',year,'/MULT',year,'.',file.type,'.processed.no_foreign.dta'))

# filter out years which represent missing values
dat <- subset(dat,dat$age_detail!=1999)
dat <- subset(dat,dat$age_detail!=9999)
dat$age.sub <- substr(dat$age_detail,1,1)

# transfer coded age to age in years
dat$age <- ifelse(dat$age.sub ==1, substr(dat$age_detail,2,4),0)		
dat$age <- as.numeric(dat$age)

dat$age.sub <- NULL

# output file
write.dta(dat,paste0('~/data/mortality/US/state/raw/cdc/',year,'/MULT',year,'.',file.type,'.processed.age_recode.dta'))
