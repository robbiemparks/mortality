rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)
library(grid)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
contig <- as.numeric(args[7])
num.draws <- as.numeric(args[8])

# source variables
source('../../data/objects/objects.R')

# Load national model values
year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=1 ; num.draws = 5000
model <- models[model]

file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_cardio/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

additional.deaths = readRDS(paste0(file.loc,'additional_deaths_age_draws.rds'))
additional.deaths.cause.together = ddply(additional.deaths,.(sex,age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.cause.together = ddply(additional.deaths.cause.together,.(sex,age),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
additional.deaths.cause.together = additional.deaths.cause.together[,c(1,2,4,5,6)]
names(additional.deaths.cause.together) = c('sex','age','deaths.added.mean.nat','deaths.added.ll.nat','deaths.added.ul.nat')

# load subnational model values
year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 11 ; dname = 't2m' ; metric = 'meanc3' ; contig=1 ; num.draws = 1000
model <- models[model]

file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_cardio/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

additional.deaths = readRDS(paste0(file.loc,'additional_deaths_age_draws.rds'))
additional.deaths.cause.together.subnational = ddply(additional.deaths,.(sex,age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.cause.subnational = ddply(additional.deaths.cause.together.subnational,.(sex,age),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
additional.deaths.cause.subnational = additional.deaths.cause.subnational[,c(1,2,4,5,6)]
names(additional.deaths.cause.subnational) = c('sex','age','deaths.added.mean.sub','deaths.added.ll.sub','deaths.added.ul.sub')

# merge national and subnational dataset then plot to compare
additional.deaths.compare = merge(additional.deaths.cause.together,additional.deaths.cause.subnational)

additional.deaths.compare = merge(additional.deaths.cause.together,additional.deaths.cause.subnational)
additional.deaths.compare$sex.long <- mapvalues(additional.deaths.compare$sex,from=sort(unique(additional.deaths.compare$sex)),to=c('Both','Male','Female'))
additional.deaths.compare$sex.long <- reorder(additional.deaths.compare$sex.long,additional.deaths.compare$sex)
additional.deaths.compare$diff <- with(additional.deaths.compare,deaths.added.mean.sub-deaths.added.mean.nat)

# output directory
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/compare_nat_sub/non_contig/all_cardio/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/compare_nat_sub/contig/all_cardio/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

pdf(paste0(file.loc,'compare_nat_sub.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(additional.deaths.compare,sex!=0),aes(x=deaths.added.mean.nat,y=deaths.added.mean.sub,color=sex.long)) +
    geom_point() +
    geom_errorbar(aes(ymin=deaths.added.ll.sub,ymax=deaths.added.ul.sub)) +
    geom_errorbarh(aes(xmin=deaths.added.ll.nat,xmax=deaths.added.ul.nat)) +
    geom_abline(linetype='dotted') +
    coord_equal() +
    labs(color = "Sex\n") +
    scale_color_manual(labels=c('Male','Female'), values = c("#2a78c1", "#c1892a")) +
    # facet_wrap(~sex.long) +
    xlab('Change in cardiorespiratory deaths\nestimated from national model (based on 2016 population)') + ylab('Change in cardiorespiratory deaths\nestimated from subnational model (based on 2016 population)') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()