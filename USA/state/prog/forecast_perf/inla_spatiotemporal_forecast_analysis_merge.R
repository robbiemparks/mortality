rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
type.arg <- as.numeric(args[5])

#pwl.arg <- as.numeric(args[5])
#forecast.length.arg <- as.numeric(args[7])
#knot.year.arg <- as.numeric(args[8])
#month.dist.arg <- as.numeric(args[9])
#month.cyclic.arg <- as.numeric(args[10])

# temp
age=age.arg ; sex = sex.arg ; year.start = year.start.arg ; year.end = year.end.arg ; type = type.arg
#pwl = pwl.arg ; forecast.length = forecast.length.arg ; knot.year = knot.year.arg; age.sel <- age.arg ; month.dist = month.dist.arg
#month.cyclic = month.cyclic.arg

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','4a')
type.selected <- types[type.arg]
pwl.lookup <- c('nopwl','pwl')
dist.lookup <- c('rw1','iid')
cyclic.lookup <- c('ncyclic','cyclic')

# lookups
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')

# file location and load
file.loc <- '../../output/forecast_perf/'
file.loc <- paste0(file.loc,'national/forecast/type_',type.selected,'/age_groups/')
file.loc <- paste0(file.loc,age.arg,'/')

# loop and load performance data
# need to do pwl and no pwl separately
temp.1 <- data.frame()
for (i in c(1)) {                           # non-pwl
    for (j in c(0)) {                       # knot years
        for (k in c(10)) {                  # forecast lengths
            for (l in c(1,2)) {             # distribution of months
                for (m in c(0,1)) {         # cylical or not
                    RDS.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[i],'knot',j,
                    '_forecast',k,'_monthterms',dist.lookup[l],cyclic.lookup[m+1],'_',year.start,'_',year.end)
                    try(dummy <- readRDS(paste0(file.loc,RDS.name,'_performance')))
                    print(dummy)
                    temp.1 <- rbind(temp.1,dummy)
                }}}}}
# remove duplicated
temp.1<- unique(temp.1)

temp.2 <- data.frame()
for (i in c(2)) {                           # pwl
    for (j in c(10,15,20)) {                # knot years
        for (k in c(10)) {                  # forecast lengths
            for (l in c(1,2)) {             # distribution of months
                for (m in c(0,1)) {         # cylical or not
                    RDS.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[i],'knot',j,
                    '_forecast',k,'_monthterms',dist.lookup[l],cyclic.lookup[m+1],'_',year.start,'_',year.end)
                    try(dummy <- readRDS(paste0(file.loc,RDS.name,'_performance')))
                    print(dummy)
                    temp.2 <- rbind(temp.2,dummy)
                }}}}}
# remove duplicated
temp.2 <- unique(temp.2)

# combine pwl and non-pwl
dat.anal <- rbind(temp.1,temp.2)

# create output file location
file.loc.output <- '../../output/forecast_perf/'
file.loc.output <- paste0(file.loc.output,'national/forecast/type_',type.selected,'/age_groups/')
file.loc.output <- paste0(file.loc.output,age.arg,'/')

# name file
RDS.name <- paste0(age.arg,'_',sex.lookup[sex.arg],'_comparison')

# output file
saveRDS(dat.anal,paste0(file.loc.output,RDS.name))





