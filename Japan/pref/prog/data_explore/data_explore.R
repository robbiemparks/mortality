rm(list=ls())

library(plyr)
library(ggplot2)
library(zoo)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# gender state and age lookup
gender.lookup <- c('Men','Women')

# NATIONAL

############################
# organise popualaton data
############################

# load population data
filename <- paste0('../../data/population/original/datpopjapan20160307')
dat.pop <- readRDS(filename)

# only keep national data
dat.pop.nat <- subset(dat.pop,pref=='0')

# rename columns
dat.pop.nat <- rename(dat.pop.nat,c('deathyear'='year'))

# rename sexes
dat.pop.nat$sex <- as.numeric(as.character(revalue(dat.pop.nat$sex, c("Male"="1", "Female"="2"))))

# create grid to attach population values to and interpolate missing values
years  <-    c(min(dat.pop.nat$year):max(dat.pop.nat$year))
sexes <- c(1:2)
ages   <-    c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)

#complete.grid <- expand.grid(year=years,month=months,sex=sexes,age=ages)
complete.grid <- expand.grid(year=years,sex=sexes,age=ages)

# merge population with complete grid to highlight missing years
dat.pop.nat.complete <- merge(complete.grid,dat.pop.nat,by=c('year','sex','age'),all.x='TRUE')

# reorder complete file
dat.pop.nat.complete <- dat.pop.nat.complete[order(dat.pop.nat.complete$age,dat.pop.nat.complete$sex,dat.pop.nat.complete$year),]
rownames(dat.pop.nat.complete) <- 1:nrow(dat.pop.nat.complete)

# remove IHME and pref column
dat.pop.nat.complete$pref_IHME <- dat.pop.nat.complete$pref <- NULL

# interpolate missing populations using zoo and ddply package
dat.pop.nat.complete <- ddply(dat.pop.nat.complete,.(sex,age),function(z) (na.approx(zoo(z))))

############################
# organise mortality data
############################

# load mortality data
filename <- paste0('../../data/mortality/original/datmortjapan20160307')
dat.mort <- readRDS(filename)

# summarise mortality data nationally
dat.mort.nat <- ddply(dat.mort,.(age5,sex,deathyear),summarize,deaths=sum(deaths))

# OUTPUT PLOTS

# create directory for output
file.loc <- paste0('../../output/data_explore/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# plot national deaths by age group over time
pdf(paste0(file.loc,'japan_nat_deaths_1969_2011','.pdf'),height=0,width=0,paper='a4r')
ggplot(data=dat.mort.nat,aes(x=year,y=deaths)) +
geom_line(aes(color=as.factor(age5))) +
facet_wrap(~sex)
dev.off()

# plot national population by age group over time
pdf(paste0(file.loc,'japan_nat_pop_1975_2013','.pdf'),height=0,width=0,paper='a4r')
ggplot(data=dat.pop.nat,aes(x=year,y=population)) +
geom_line(aes(color=as.factor(age))) +
facet_wrap(~sex)
dev.off()

pdf(paste0(file.loc,'japan_nat_pop_1975_2013_interpolated','.pdf'),height=0,width=0,paper='a4r')
ggplot() +
geom_line(dat=na.omit(dat.pop.nat.complete),aes(x=year,y=population,color=as.factor(age))) +
facet_wrap(~sex)
dev.off()
