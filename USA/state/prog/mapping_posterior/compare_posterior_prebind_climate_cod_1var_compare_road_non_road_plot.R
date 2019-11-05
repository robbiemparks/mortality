rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4]) ; model.2 = as.numeric(args[4])
dname <- as.character(args[6])
metric <- as.character(args[7])
cause <- as.character(args[8]) ; cause <- gsub('_',' ',cause)
cause.2 <- as.character(args[9]) ; cause.2 <- gsub('_',' ',cause.2)
contig.arg <- as.numeric(args[10])
pw.arg <- as.numeric(args[11])

# for model testing
# year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model = 27 ; dname='t2m' ; metric='meanc4'
# cause = 'Transport accidents' ; cause.2 = 'Road traffic accidents' ;  cause.3 = 'Other transport accidents'
# contig.arg = 1 ; pw.arg = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# library(INLA)

# create dataframe with each of the national terms for entire group of age and sexes
dat <- data.frame()
# create dataframe of all the actual parameter terms
dat.parameters <- data.frame()

# output file location to recover data from
file.loc.git <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'_road_non_road_comparison/parameters/')

# load files
save.name.param.1 <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_1_fast_contig.csv')
dat.1 = read.csv(paste0(file.loc.git,save.name.param.1))
dat.1$X = NULL

save.name.param.2 <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')
dat.2 = read.csv(paste0(file.loc.git,save.name.param.2))
dat.2$X = NULL

save.name.param.3 <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_1_fast_contig.csv')
dat.3 = read.csv(paste0(file.loc.git,save.name.param.3))
dat.3$X = NULL

save.name.param.4 <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_2_fast_contig.csv')
dat.4 = read.csv(paste0(file.loc.git,save.name.param.4))
dat.4$X = NULL

save.name.param.5 <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.3,'_1_fast_contig.csv')
dat.5 = read.csv(paste0(file.loc.git,save.name.param.5))
dat.5$X = NULL

save.name.param.6 <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.3,'_2_fast_contig.csv')
dat.6 = read.csv(paste0(file.loc.git,save.name.param.6))
dat.6$X = NULL

# combine male and female for each
dat.transport = rbind(dat.1,dat.2)
dat.road = rbind(dat.3,dat.4)
dat.nonroad = rbind(dat.5,dat.6)

# add month ID
dat.transport$month = seq(1:12) ; dat.road$month = seq(1:12) ; dat.nonroad$month = seq(1:12)

# combine all files to compare
dat.road.compare = merge(dat.transport,dat.road,by=c('month','age','sex'),all.x=TRUE)
dat.nonroad.compare = merge(dat.transport,dat.nonroad,by=c('month','age','sex'),all.x=TRUE)

# plot results
library(ggplot2)
#
# dat.parameters$sex.long <- plyr::mapvalues(dat.parameters$sex,from=sort(unique(dat.parameters$sex)),to=c('Male','Female'))
# dat.parameters$sex.long <- with(dat.parameters,reorder(dat.parameters$sex.long,sex))
#
# dat.parameters$age.long <- plyr::mapvalues(dat.parameters$age,from=sort(unique(dat.parameters$age)),to=as.character(age.code[,2]))
# dat.parameters$age.long <- reorder(dat.parameters$age.long,dat.parameters$age)
#
pdf(paste0(file.loc.git,country,'_correlations_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_all_transport_against_road_only_fast_contig.pdf'),paper='a4r',height=0,width=0)
print(
    ggplot(data=dat.road.compare,aes(x=model.2.mean.x,y=model.2.mean.y)) +
    geom_point() +
    geom_errorbar(aes(ymin=model.2.ll.y,ymax=model.2.ul.y),alpha=0.2) +
    geom_errorbarh(aes(xmin=model.2.ll.x,xmax=model.2.ul.x),alpha=0.2) +
    geom_abline() +
    coord_equal() +
    xlab('Temperature parameters from all transport runs') + ylab('Temperature parameters from road traffic only runs') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)
dev.off()

pdf(paste0(file.loc.git,country,'_correlations_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_all_transport_against_non-road_only_fast_contig.pdf'),paper='a4r',height=0,width=0)
print(
    ggplot(data=dat.nonroad.compare,aes(x=model.2.mean.x,y=model.2.mean.y)) +
    geom_point() +
    geom_errorbar(aes(ymin=model.2.ll.y,ymax=model.2.ul.y),alpha=0.2) +
    geom_errorbarh(aes(xmin=model.2.ll.x,xmax=model.2.ul.x),alpha=0.2) +
    geom_abline() +
    coord_equal() +
    xlab('Temperature parameters from all transport runs') + ylab('Temperature parameters from non-road traffic only runs') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)
dev.off()

