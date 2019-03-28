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
contig.arg <- as.numeric(args[9])
pw.arg <- as.numeric(args[10])

# for model testing
# year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; model.2 = 26 ; dname='t2m' ; metric='meanc3'
# cause = 'Transport accidents' ;  contig.arg = 1 ; pw.arg = 0




if(pw.arg==0){
    # create dataframe with each of the national terms for entire group of age and sexes
    dat <- data.frame()
    
    # find the posterior exponential mean
    for (i in seq(length(sex.filter))) {
        for (j in seq(length(age.filter))) {
            # load data
            if(cause!='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
                year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
                file.name.2 <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model.2,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model.2,'_',age.filter[j],'_',sex.lookup[i],'_',
                year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
            }
            if(cause=='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
                file.name.2 <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model.2,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model.2,'_',age.filter[j],'_',sex.lookup[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
            }

            # load parameters
            model.current <- readRDS(file.name)
            model.current.2 <- readRDS(file.name.2)
            current.file <- model.current$summary.random$month5
            current.file.2 <- model.current.2$summary.random$month5

            # obtain correlation values
            corr.current = data.frame(age=age.filter[j],sex=i,correlation=cor(current.file$mean,current.file.2$mean))

            dat = rbind(dat,corre.current)

        }
    }

# create directories for output
file.loc.local <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.local), dir.create(file.loc.local, recursive=TRUE), FALSE)
file.loc.git <- paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior
if(cause!='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig')
}
if(cause=='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig')
}

saveRDS(dat,paste0(file.loc.local,save.name))
saveRDS(dat,paste0(file.loc.git,save.name))

}