# FOR ALL AGES AND ALL CAUSES TOGETHER IN ONE STATE

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

# MAKE 1000 DRAWS
num.draws <- 1000

# load inla paradiso (what on earth is this?)
library(INLA)

# make draws from the model for the parameters
print(paste0('Making ',num.draws, ' draws...'))
draws.current = try(inla.posterior.sample(num.draws,model.current,selection=list('month5'=1:12)))

# CALCULATE ADDITIONAL DEATHS BASED ON POSITIVE ANOMALY

# SUM UP

# ORDER 1000 RESULTS TO GET 95% CI