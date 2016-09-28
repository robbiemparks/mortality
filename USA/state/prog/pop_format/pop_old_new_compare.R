rm(list=ls())

library(foreign)
library(readr)
library(dplyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year.arg <- as.numeric(args[1])

# load 2 files to compare
dat.old <- read.dta(paste0("../../data/population/original/countyPopulations.dta"))
dat.new <- read.dta(paste0("../../output/pop_format/popcounty",year.arg,'.dta'))

# filter for year of interest and get rid of 99 age group
dat.old <- subset(dat.old,year==year.arg & age!=99)

# merge files
merged <- merge(dat.old,dat.new,by=c('fips','sex','age','year','stateFips','countyFips'))

# rows which do not match death counts in merged file
merged$diff <- abs(merged$pop.x - merged$pop.y)
dat.diff <-  merged[merged$diff!=0,]
dat.diff$percent.change <- 100*dat.diff$diff/dat.diff$pop.x

# check unmatched rows from new and old files
unmatched.old <- anti_join(dat.old,dat.new,by=c('fips','sex','age','year','stateFips','countyFips'))
unmatched.new <- anti_join(dat.new,dat.old,by=c('fips','sex','age','year','stateFips','countyFips'))

# create output directory
ifelse(!dir.exists("../../output/pop_old_new_compare"), dir.create("../../output/pop_old_new_compare"), FALSE)
ifelse(!dir.exists(paste0("../../output/pop_old_new_compare/",year.arg)), dir.create(paste0("../../output/pop_old_new_compare/",year.arg)), FALSE)

# plot population against each other to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/old_against_new_population_',year.arg,'.png'))
print(
ggplot(merged,aes(x=pop.x,y=pop.y)) +
geom_point() +
geom_abline(slope=1) +
ggtitle(paste0('old against new population ',year.arg)) +
xlab('original population') +
ylab('new population') +
theme_bw()
)
dev.off()

# output files of mismatched data
write.csv(unmatched.old,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_old_',year.arg,'.csv'))
write.csv(unmatched.new,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_new_',year.arg,'.csv'))
write.csv(dat.diff,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_population_',year.arg,'.csv'))


