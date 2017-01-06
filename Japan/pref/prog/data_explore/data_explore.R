rm(list=ls())

library(plyr)
library(ggplot2)
library(zoo)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# gender state and age lookup
gender.lookup <- c('Men','Women')

#age.new.lookup <- data.frame(age5=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
#                            age.new=c(0,5,5,15,15,25,25,35,35,45,45,55,55,65,65,75,80,85,85,95,95))

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


# create grid to attach population values to and interpolate missing values
#months <- 	c(1:12)
years  <-    c(min(dat.pop.nat$year):max(dat.pop.nat$year))
sexes  <- 	 c('Male','Female')
ages   <-    c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)

#complete.grid <- expand.grid(year=years,month=months,sex=sexes,age=ages)
complete.grid <- expand.grid(year=years,sex=sexes,age=ages)

# merge population with complete grid to highlight missing years
dat.pop.nat.complete <- merge(complete.grid,dat.pop.nat,by=c('year','sex','age'),all.x='TRUE')

# reorder complete file
dat.pop.nat.complete <- dat.pop.nat.complete[order(dat.pop.nat.complete$age,dat.pop.nat.complete$sex,dat.pop.nat.complete$year),]

# interpolate using zoo and ddply package LIKE BELOW BUT NEEDS TO BE CHANGED
dat.pop.nat.complete <- ddply(dat.pop.nat.complete,.(sex,age),function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))

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
ggplot(data=dat.mort.nat,aes(x=deathyear,y=deaths)) +
geom_line(aes(color=as.factor(age5))) +
facet_wrap(~sex)
dev.off()

# plot national population by age group over time
pdf(paste0(file.loc,'japan_nat_pop_1975_2013','.pdf'),height=0,width=0,paper='a4r')
ggplot(data=dat.pop.nat,aes(x=deathyear,y=population)) +
geom_line(aes(color=as.factor(age))) +
facet_wrap(~sex)
dev.off()
