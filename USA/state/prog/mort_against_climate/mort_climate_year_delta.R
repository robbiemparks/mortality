rm(list=ls())

library(RColorBrewer)
library(plyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
year.start.mort.arg <- as.numeric(args[3])
year.end.mort.arg <- as.numeric(args[4])
year.start.clim.arg <- as.numeric(args[5])
year.end.clim.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric.arg <- as.character(args[8])

# range of years
years <- (max(year.start.mort.arg,year.start.clim.arg):min(year.end.mort.arg,year.end.clim.arg))
year.start.arg <- min(years)
year.end.arg <- max(years)

# lookups
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),age.print=age.print)
month.names <- c('January','February','March','April','May','June',
                 'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# create files for output
file.loc <- '../../output/mort_climate_year_delta/'
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load data
dat <- read.csv('~/git/mortality/USA/state/output/mort_against_climate/t2m/mean/1982_2013/mort_against_climate_complete.csv')

# find deltas between temperature and mortality rates for equivalent neighbouring months between years
dat.diff <- ddply(dat, .(sex,age,month,fips), summarize, diff_mort=diff(rate.adj),diff_clim=diff(variable))

# merge state names
dat.diff <- merge(dat.diff, state.lookup, by='fips')
dat.diff <- merge(dat.diff, age.code, by='age')
dat.diff$age.print <- reorder(dat.diff$age.print,dat.diff$age)

# all facet by age
pdf(paste0(file.loc,'/','facet_by_age.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.diff) +
geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(month))) +
facet_wrap(~age)
dev.off()

# all facet by state
pdf(paste0(file.loc,'/','facet_by_state.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.diff) +
geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(full_name))) +
facet_wrap(~full_name) +
guides(color=FALSE) +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# by state facet by age
pdf(paste0(file.loc,'/','facet_by_age_by_state.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.diff$fips)) {
    print(
    ggplot(data=subset(dat.diff,fips==i)) +
    ggtitle(state.lookup$full_name[state.lookup$fips==i]) +
    geom_point(aes(x=100000*diff_mort,y=diff_clim)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Difference in mortality rate (per 100,000)')+
    ylab('Difference in temperature') +
    geom_vline(xintercept=0, linetype=2,alpha=0.5) +
    facet_wrap(~age.print, scales='free') +
    guides(color=FALSE))
}
dev.off()

# by state facet by age colour by month
pdf(paste0(file.loc,'/','facet_by_age_by_state_colour_month.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.diff$fips)) {
    print(
    ggplot(data=subset(dat.diff,fips==i)) +
    ggtitle(state.lookup$full_name[state.lookup$fips==i]) +
    geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(month))) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_vline(xintercept=0, linetype=2,alpha=0.5) +
    xlab('Difference in mortality rate (per 100,000)')+
    ylab('Difference in temperature') +
    facet_wrap(~age.print, scales='free') +
    guides(color=FALSE))
}
dev.off()

# by state and month facet by age colour by month
pdf(paste0(file.loc,'/','facet_by_age_by_state_by_month.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.diff$fips)) {
    for(j in c(1:12)) {
        print(
        ggplot(data=subset(dat.diff,fips==i & month==j)) +
        ggtitle(paste0(state.lookup$full_name[state.lookup$fips==i],' ',month.short[j])) +
        geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(month))) +
        geom_hline(yintercept=0, linetype=2,alpha=0.5) +
        geom_vline(xintercept=0, linetype=2,alpha=0.5) +
        xlab('Difference in mortality rate (per 100,000)')+
        ylab('Difference in temperature') +
        facet_wrap(~age.print, scales='free') +
        guides(color=FALSE))
        }
    }
dev.off()