rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
sex.arg <- as.numeric(args[1])
year.start.arg <- as.numeric(args[2])
year.end.arg <- as.numeric(args[3])
type.arg <- as.numeric(args[4])
cluster.arg <- as.numeric(args[5])
year.start.analysis.arg <- as.numeric(args[6])
year.end.analysis.arg <- as.numeric(args[7])
cod.arg <- as.character(args[8]) ; cod.arg <- gsub('_',' ',cod.arg)
pw.arg <- as.numeric(args[9])

# break down the arguments from Rscript
cause <- 'AllCause'
year.start <- 1982
year.end <- 2017
country <- 'USA'
model <- 27
pw <- 1
dname <- 't2m'
metric <- 'mean'

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# temporary workaround to avoid GLIBC error (???) from:
# https://www.mn.uio.no/math/english/services/it/help/status/2018-07-26-inla-r.html
INLA:::inla.dynload.workaround()

# load INLA
library(INLA)

# create directories for output
file.loc <- paste0('../../output/additional_deaths_maricopa/',year.start,'_',year.end,
'/',dname,'/',metric,'/pw/type_',model,'/non_contig/all_injuries/no_draws/')

#  directory for input
if(pw==0){
    file.loc.input <- paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname,'/',metric,'/non_pw/type_',model,'/all_ages/')
}
if(pw==1){
    file.loc.input <- paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname,'/',metric,'/pw/type_',model,'/all_ages/')
}

# load the full model for a particular sex
if(cause!='AllCause'){
    # TO FINISH
}
if(cause=='AllCause'){
    input.string.males = paste0('maricopa_rate_pred_type',model,'_',sex.lookup[1],'_',
    year.start,'_',year.end,'_',dname,'_',metric,'_parameters')
    file.name.males <- paste0(file.loc.input,input.string)

    input.string.females = paste0('maricopa_rate_pred_type',model,'_',sex.lookup[2],'_',
    year.start,'_',year.end,'_',dname,'_',metric,'_parameters')
    file.name.females <- paste0(file.loc.input,input.string)
}

# load model results for processing
print(paste0('Reading ',file.name.males))
model.current.males <- readRDS(file.name.males)

print(paste0('Reading ',file.name.females))
model.current.females <- readRDS(file.name.females)

model.1d.males <- model.current.males$summary.random$month5
model.1d.males$month=c(1:12) ; model.1d.males$ID = NULL
model.1d.males$age=rep(c(0,5,15,25,35,45,55,65,75,85),each=12)
model.1d.males$sex.long = 'Male'

model.1d.females <- model.current.females$summary.random$month5
model.1d.females$month=c(1:12) ; model.1d.females$ID = NULL
model.1d.females$age=rep(c(0,5,15,25,35,45,55,65,75,85),each=12)
model.1d.females$sex.long = 'Female'

dat.parameters = rbind(model.1d.males,model.1d.females)

# plot results
library(ggplot2)

dat.parameters$age.long <- plyr::mapvalues(dat.parameters$age,from=sort(unique(dat.parameters$age)),to=as.character(age.code[,2]))
dat.parameters$age.long <- reorder(dat.parameters$age.long,dat.parameters$age)

names(dat.parameters) = c('mean','sd','mean.ll','median','mean.ul','mode','kld','month','age','sex.long','age.long')

# print and fix!
pdf(paste0(file.loc.git,country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
print(ggplot(data=subset(dat.parameters,sex.long=='Male'),aes(x=month,y=mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=mean.ll,ymax=mean.ul)) +
    geom_abline() +
    facet_wrap(~age.long) +
    # xlab('Temperature parameters from original model') + ylab('Temperature parameters from model\nwith adjusted hyperpriors') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)
