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
# year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model.1 = 27 ; model.2 = 29 ; dname='t2m' ; metric='meanc4'
# contig.arg = 1 ; pw.arg = 0

# directories for output
file.loc.git.1 <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model.1,'/parameters/')

causes = c('Accidental drowning and submersion','Assault','Transport accidents','Accidental falls','Intentional self-harm')

for(cause in causes){

    save.name.param <- paste0(country,'_parameters_',model.1,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')
    save.name.intercept <- paste0(country,'_intercepts_',model.1,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')
    save.name.abs.temp <- paste0(country,'_abs_temp_values_',model.1,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')

    dat.param.1 = read.csv(paste0(file.loc.git.1,save.name.param))
    dat.intercept.1 = read.csv(paste0(file.loc.git.1,save.name.intercept))
    dat.abs.temp.1 = read.csv(paste0(file.loc.git.1,save.name.abs.temp))

}



# source variables
source('../../data/objects/objects.R')
model <- models[model]

library(INLA)

# create dataframe with each of the national terms for entire group of age and sexes
dat <- data.frame()
# create dataframe of all the actual parameter terms
dat.parameters <- data.frame()
dat.intercepts <- data.frame()
dat.abs.temps  <- data.frame()




# save bound posterior and summaries
save.name.param <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')
save.name.intercept <- paste0(country,'_intercepts_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')
save.name.abs.temp <- paste0(country,'_abs_temp_values_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')

