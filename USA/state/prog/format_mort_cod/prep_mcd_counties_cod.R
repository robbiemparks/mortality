rm(list=ls())

library(foreign)
library(readr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
file.type <- as.character(args[2])

# file name
if((year >= 1989) & (year<=1997))
{
    file.name 	<- paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_counties.dta')
} else
{
    file.name   <- paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'.dta')
}

# load file
dat <- read.dta(file.name)

# filter foreign deaths and incomplete resident records
dat <- dat[dat$resident <= 3,]
dat <- dat[is.na(dat$resident) == FALSE,]

# remove unknown ages
dat <- subset(dat,dat$age_detail!=1999)
dat <- subset(dat,dat$age_detail!=9999)
dat <- dat[dat$age != 999,]

# transfer coded age to age in years
dat$age.sub <- substr(dat$age_detail,1,1)
if(year <= 2002){
    dat$age <- ifelse(dat$age.sub==0 | dat$age.sub==1, substr(dat$age_detail,1,3),0)
    dat$age <- as.numeric(dat$age)
    }
if(year >= 2003){
    dat$age <- ifelse(dat$age.sub ==1, substr(dat$age_detail,2,4),0)
    dat$age <- as.numeric(dat$age)
}
dat$age.sub <- NULL

# correct fips codes
if((year >= 1982) & (year <= 1999)){
    # SOMETHING
}
if(year >= 2000) {
    # SOMETHING
}

# Fix ICD codes (ICD switch starts in 1999, i.e 1998 uses ICD 9)
switch.year = 1999

# filter only relevant values of interest
# ICD 9 years
if(year<switch.year) {
    dat <- dat[, c('monthdth','age','stateres_fips','countyres_fips','sex','year','icd9')]
}
# ICD 10 years
if(year %in% c(1999,2000,2001)){
    dat <- dat[, c('monthdth','age','stateres_fips','countyres_fips','sex','year','icd10')]
    # only take first letter of COD NOT SURE ASK MAJID
    dat$cause_letter = substr(dat$icd10,1,1)
}
if(year>=2002){
    dat <- dat[, c('monthdth','age','stateres_fips','countyres_fips','sex','year','cause')]
    # only take first letter of COD NOT SURE ASK MAJID
    dat$cause_letter = substr(dat$cause,1,1)
}

# concatenate state and fips codes
dat$fips <- paste0(dat$stateres_fips,dat$countyres_fips)

# summarise by 5-year age group
dat$age <-
ifelse (dat$age<5,   0,
ifelse (dat$age<10,  5,
ifelse (dat$age<15,  10,
ifelse (dat$age<20,  15,
ifelse (dat$age<25,  20,
ifelse (dat$age<30,  25,
ifelse (dat$age<35,  30,
ifelse (dat$age<40,  35,
ifelse (dat$age<45,  40,
ifelse (dat$age<50,  45,
ifelse (dat$age<55,  50,
ifelse (dat$age<60,  55,
ifelse (dat$age<65,  60,
ifelse (dat$age<70,  65,
ifelse (dat$age<75,  70,
ifelse (dat$age<80,  75,
ifelse (dat$age<85,  80,
85)))))))))))))))))

library(plyr)
library(dplyr)

dat$dummy <- 1

# ICD 9 years
if(year<=switch.year){
    dat.summarised <- dplyr::summarise(group_by(dat,icd9,monthdth,age,fips,sex,year),sum(dummy))
    dat.summarised <- plyr::rename(dat.summarised,c('icd9'='cause'))
}
# ICD 10 years
if(year %in% c(1999,2000,2001)){
    dat.summarised <- dplyr::summarise(group_by(dat,icd10,monthdth,age,fips,sex,year),sum(dummy))
    dat.summarised <- plyr::rename(dat.summarised,c('icd10'='cause'))
}
if(year>=2002){
    dat.summarised <- dplyr::summarise(group_by(dat,cause,monthdth,age,fips,sex,year),sum(dummy))
}
dat.summarised <- plyr::rename(dat.summarised,c('sum(dummy)'='deaths'))


# convert month of death into number
dat.summarised$monthdth <- as.numeric(dat.summarised$monthdth)

# make sure year is 4-digits
dat.summarised$year <- year

# output file for next stage of processing
write.dta(dat.summarised,paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))
