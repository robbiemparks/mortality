rm(list=ls())

library(foreign)
library(readr)
library(dplyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])

# load 2 files to compare
dat.old <- read.dta(paste0("/home/rmp15/data/mortality/US/state/processed/county/deaths",year,'_original.dta'))
dat.new <- read.dta(paste0("/home/rmp15/data/mortality/US/state/processed/county/deaths",year,'.dta'))

# merge files
merged <- merge(dat.old,dat.new,by=c('monthdth','age','fips','sex'))

# rows which do not match death counts in merged file
merged$diff <- abs(merged$deaths.x - merged$deaths.y) 
dat.diff <-  merged[merged$diff!=0,]

# check unmatched rows from new and old files
unmatched.old <- anti_join(dat.old,dat.new,by=c('monthdth','age','sex','fips'))
unmatched.new <- anti_join(dat.new,dat.old,by=c('monthdth','age','sex','fips'))

# create output directory
ifelse(!dir.exists("../../output/old_against_new"), dir.create("../../output/old_against_new"), FALSE)
ifelse(!dir.exists(paste0("../../output/old_against_new/",year)), dir.create(paste0("../../output/old_against_new/",year)), FALSE)

# plot death counts against each other to check
png(paste0('../../output/old_against_new/',year,'/old_against_new_death_rates_',year,'.png'))
print(
ggplot(merged,aes(x=deaths.x,y=deaths.y)) +
geom_point() +
geom_abline(slope=1) +
ggtitle(paste0('old against new death rates ',year)) +
xlab('original death counts') +
ylab('new death counts') +
theme_bw()
)
dev.off()

# output files of mismatched data
write.csv(unmatched.old,paste0('../../output/old_against_new/',year,'/unmatched_old_',year,'.csv'))
write.csv(unmatched.new,paste0('../../output/old_against_new/',year,'/unmatched_new_',year,'.csv'))
write.csv(dat.diff,paste0('../../output/old_against_new/',year,'/unmatched_death_counts_',year,'.csv'))


