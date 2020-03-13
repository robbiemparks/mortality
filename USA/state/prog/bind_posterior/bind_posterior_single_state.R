# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
type.arg <- as.numeric(args[5])
cluster.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric.arg <- as.character(args[8])
year.start.analysis.arg <- as.numeric(args[9])
year.end.analysis.arg <- as.numeric(args[10])
cod.arg <- as.character(args[11]) ; cod.arg <- gsub('_',' ',cod.arg)
fast.arg <- as.numeric(args[12])
contig.arg <- as.numeric(args[13])
pw.arg <- as.numeric(args[14])

country='USA'

# for test runs
# year.start.arg = 1980 ; year.end.arg = 2017 ; type.arg = 27 ;
# cluster.arg = 0 ; dname.arg = 't2m' ; metric.arg = 'meanc4' ; year.start.analysis.arg = 1980 ;
# year.end.analysis.arg = 2017 ; cod.arg = 'AllCause'; fast.arg = 1 ; contig.arg = 1
# pw.arg=0 ; state.arg = 4

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8','1d9','1d10')
type.selected <- types[type.arg]

print(paste(year.start.analysis.arg,year.end.analysis.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
require(mailR)
state.name = as.character(subset(fips.lookup,fips==state.arg)[1,1])

# load parameters file
output.string = paste0(state.name,'_rate_pred_type',type.selected,'_',year.start.analysis.arg,'_',year.end.analysis.arg,'_',dname.arg,'_',metric.arg)
parameters.name <- paste0(output.string)
parameters.name = paste0(parameters.name,'_parameters')
file.loc <- paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages/')

model.current = readRDS(paste0(file.loc,parameters.name))

# load inla paradiso (what on earth is this?)
library(INLA)

# find mean and CIs of transformed distributions and probability of increased odds from posterior marginal
dat.mean.exp <- data.frame(ID=numeric(0),odds.mean=numeric(0),odds.ll=numeric(0),odds.ul=numeric(0),odds.prob=numeric(0))
for(k in c(1:length(model.current$marginals.random$month5))) {
    # find the exponentiated means and CIs
    marginal.exp <- inla.tmarginal(function(x) exp(x), model.current$marginals.random$month5[[k]])
    odds.mean <- inla.emarginal(function(x) x,marginal.exp) - 1
    odds.ll <- inla.qmarginal(0.025,marginal.exp) - 1
    odds.ul <- inla.qmarginal(0.975,marginal.exp) - 1

    # find the probability of increased odds from posterior marginal
    odds.prob <- 1 - inla.pmarginal(1,marginal.exp)
    # dat.temp <- data.frame(ID=k,odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob) OLD
    dat.temp <- data.frame(odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
    dat.mean.exp <- rbind(dat.mean.exp,dat.temp)
}

# rename
dat <- dat.mean.exp

# create directories for output
file.loc <- paste0('../../output/bind_posterior_climate_single_state/',year.start.arg,'_',year.end.arg,
'/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/non_contig/all_cause/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# save bound posterior
save.name <- paste0(state.name,'_parameters_rate_pred_type',type.selected,'_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',type.selected,'_fast_contig')

saveRDS(dat,paste0(file.loc,save.name,'.rds'))
write.csv(dat,paste0(file.loc,save.name,'.csv'))
