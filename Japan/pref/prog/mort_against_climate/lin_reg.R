rm(list=ls())

library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.mort.arg <- as.numeric(args[3])
year.end.mort.arg <- as.numeric(args[4])
year.start.clim.arg <- as.numeric(args[5])
year.end.clim.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric1.arg <- as.character(args[8])
metric2.arg <- as.character(args[9])

# range of years
years <- (max(year.start.mort.arg,year.start.clim.arg):min(year.end.mort.arg,year.end.clim.arg))
year.start.arg <- min(years)
year.end.arg <- max(years)

# lookups
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# create files for output
file.loc <- '../../output/lin_reg/'
file.loc <- paste0(file.loc,dname.arg)
#file.loc <- paste0(file.loc,'/',metric.arg)
file.loc <- paste0(file.loc,'/',year.start.arg,'_',year.end.arg)
file.loc <- paste0(file.loc,'/',sex.lookup[sex.arg])
file.loc <- paste0(file.loc,'/',age.arg)
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load USA mortality data and filter for relevant years
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.mort.arg,'_',year.end.mort.arg))
dat <- subset(dat, year %in% years & sex == sex.arg & age== age.arg)

# load climate data and filter for relevant years
dat.climate.1 <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric1.arg,'_',dname.arg,'/state_weighted_summary_',metric1.arg,'_',dname.arg,'_',year.start.clim.arg,'_',year.end.clim.arg,'.rds'))
dat.climate.1 <- subset(dat.climate.1, year %in% years)
dat.climate.1$state.fips <- as.numeric(dat.climate.1$state.fips)
dat.climate.2 <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric2.arg,'_',dname.arg,'/state_weighted_summary_',metric2.arg,'_',dname.arg,'_',year.start.clim.arg,'_',year.end.clim.arg,'.rds'))
dat.climate.2 <- subset(dat.climate.2, year %in% years)
dat.climate.2$state.fips <- as.numeric(dat.climate.2$state.fips)

# merge mortality and climate data and reorder
dat.merged <- merge(dat,dat.climate.1,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- merge(dat.merged,dat.climate.2,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# exclude Hawaii and Alaska
state.lookup <- subset(state.lookup, fips !=2) 
state.lookup <- subset(state.lookup, fips !=15)
state.names <- as.character(state.lookup$full_name)
dat.merged <- subset(dat.merged, fips !=2) 
dat.merged <- subset(dat.merged, fips !=15) 

# generalise variable names
names(dat.merged)[grep(metric1.arg,names(dat.merged))] <- 'variable1'
names(dat.merged)[grep(metric2.arg,names(dat.merged))] <- 'variable2'

# rename rows and remove unnecessary columns
dat.merged$id <- NULL
rownames(dat.merged) <- 1:nrow(dat.merged)

library(plyr)

# variables for pretty and easy to read plots
dat.merged$month.short <- mapvalues(dat.merged$month,from=unique(dat.merged$month),to=month.short)
dat.merged$month.short <- reorder(dat.merged$month.short,dat.merged$month)
# variables for pretty and easy to read plots
dat.merged$state.name <- mapvalues(dat.merged$fips,from=unique(dat.merged$fips),to=state.names)
dat.merged$state.name <- reorder(dat.merged$state.name,dat.merged$fips)

#############################################
# LIN REG FITS
#############################################

# variable*fips
v1.state <-summary(lm(rate.adj~variable1*fips,data=dat.merged))$r.squared

# variable2*fips
v2.state <- summary(lm(rate.adj~variable2*fips,data=dat.merged))$r.squared

# variable1*fips + variable2*fips
v1.state.v2.state <- summary(lm(rate.adj~variable1*fips + variable2*fips,data=dat.merged))$r.squared

# variable1*fips*month
v1.state.month <-  summary(lm(rate.adj~variable1*fips*month,data=dat.merged))$r.squared

# variable2*fips*month
v2.state.month <- summary(lm(rate.adj~variable2*fips*month,data=dat.merged))$r.squared

# variable1*fips*month + variable2*fips
v1.state.month.v2.state <- summary(lm(rate.adj~variable1*fips*month + variable2*fips,data=dat.merged))$r.squared

# variable1*fips + variable2*fips*month
v1.state.v2.state.month <- summary(lm(rate.adj~variable1*fips + variable2*fips*month,data=dat.merged))$r.squared

# variable1*fips*month + variable2*fips*month
v1.state.month.v2.state.month <- summary(lm(rate.adj~variable1*fips*month + variable2*fips*month,data=dat.merged))$r.squared

# construct data frame with results
names.reg <- c( paste0(metric1.arg,'.state'),paste0(metric2.arg,'.state'),paste0(metric1.arg,'.state+',metric2.arg,'.state'),paste0(metric1.arg,'.state.month'),
                paste0(metric2.arg,'.state.month'),paste0(metric1.arg,'.state.month+',metric2.arg,'.state'),paste0(metric1.arg,'.state+',metric2.arg,'.state.month'),paste0(metric1.arg,'.state.month+',metric2.arg,'.state.month'))
rsquared.reg <- c(v1.state,v2.state,v1.state.v2.state,v1.state.month,v2.state.month,v1.state.month.v2.state,v1.state.v2.state.month,v1.state.month.v2.state.month)

dat.reg <- data.frame(type=names.reg,r.squared=rsquared.reg)

write.csv(dat.reg,paste0(file.loc,'/','rsquared_',dname.arg,'_',metric1.arg,'_against_',metric2.arg,'_',sex.lookup[sex.arg],'_',age.arg,'_',year.start.arg,'_',year.end.arg,'.csv'))

##########################################################
# 1. PLOT CLIMATE VARIABLES AGAINST EACH OTHER NATIONALLY
##########################################################

library(ggplot2)
library(RColorBrewer)

pdf(paste0(file.loc,'/',dname.arg,'_',metric1.arg,'_against_',metric2.arg,'_',sex.lookup[sex.arg],'_',age.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable1,y=variable2,color=factor(month))) +
stat_smooth(aes(x=variable1,y=variable2)) +
ggtitle(paste0(year.start.arg,'-',year.end.arg,' ',dname.arg,' ',metric1.arg,' against ',metric2.arg, ' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric1.arg)) +
ylab(paste0(dname.arg,' ',metric2.arg)) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
theme_bw()
dev.off()

##########################################################
# 2. PLOT CLIMATE VARIABLES AGAINST EACH OTHER BY STATE
##########################################################

pdf(paste0(file.loc,'/',dname.arg,'_',metric1.arg,'_against_',metric2.arg,'_by_state_',sex.lookup[sex.arg],'_',age.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) +
geom_point(aes(x=variable1,y=variable2,color=factor(month))) +
stat_smooth(aes(x=variable1,y=variable2)) +
ggtitle(paste0(year.start.arg,'-',year.end.arg,' ',dname.arg,' ',metric1.arg,' against ',metric2.arg, ' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric1.arg)) +
ylab(paste0(dname.arg,' ',metric2.arg)) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
theme_bw() +
facet_wrap(~state.name)
dev.off()

##########################################################
# 3. PLOT CLIMATE VARIABLES AGAINST EACH OTHER BY MONTH
##########################################################

pdf(paste0(file.loc,'/',dname.arg,'_',metric1.arg,'_against_',metric2.arg,'_by_month_',sex.lookup[sex.arg],'_',age.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) +
geom_point(aes(x=variable1,y=variable2)) +
stat_smooth(aes(x=variable1,y=variable2)) +
ggtitle(paste0(year.start.arg,'-',year.end.arg,' ',dname.arg,' ',metric1.arg,' against ',metric2.arg, ' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric1.arg)) +
ylab(paste0(dname.arg,' ',metric2.arg)) +
#scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
theme_bw() +
facet_wrap(~month.short)
dev.off()


