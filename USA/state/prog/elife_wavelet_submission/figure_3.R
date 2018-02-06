rm(list=ls())

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start.arg = as.numeric(args[1])   ; year.end.arg = as.numeric(args[2])
age.arg = as.numeric(args[3])          ; sex.arg = as.numeric(args[4])

# load required packages
packages = c('plyr', 'CircStats')
lapply(packages, require, character.only=TRUE)

# create output directories
output.loc = paste0("/output/com/",year.start.arg,'_',year.end.arg,"/national/values/entire_period/")
ifelse(!dir.exists(output.loc), dir.create(output.loc,recursive=TRUE), FALSE)

# relevant objects
sex.lookup = c('Men','Women')
source('../../data/objects/objects.R')

# load data
input.loc = 'file_here'
dat = readRDS(input.loc)

# source com functions
source('../01_functions/com_functions.R')

# functions to find centre of mass of max and minimum mortality
circular_max = function(age.selected,sex.selected) {

    # print current age-gender combination
    print(paste0('Working on max COM for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp = subset(dat,age==age.selected & sex==sex.selected)

    # calculate rates per million and then round
    dat.temp$rate.scaled = round(1000000*(dat.temp$rate.adj))

    # take months column and repeat death column times
    dat.temp = rep(dat.temp$month,dat.temp$rate.scaled)

    # convert months -> radians
    conv = 2*pi/12
    dat.conv = dat.temp*conv

    # find circular mean in circular world
    dat.mean = (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent = dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample = vector()
    for(i in 1:1000){
        sample = sample(dat.conv.cent, replace = T)
        dat.temp.mean = circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] = dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap = sort(dat.sample)
    COM.bootstrap.5 = COM.bootstrap[25]
    COM.bootstrap.95 = COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean = (dat.mean)/conv
    COM.bootstrap.5 = (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95 = (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame = data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    saveRDS(dat.frame,paste0(output.loc,'max_',sex.lookup[sex.selected],'_',age.selected))

    return(dat.frame)
}
circular_min <- function(age.selected,sex.selected) {

    print(paste0('Working on max COM for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,dat.temp$rate.inv)

    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv

    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    saveRDS(dat.frame,paste0(output.loc,'min_',sex.lookup[sex.selected],'_',age.selected))

    return(dat.frame)
}

# perform functions for age-gender combination
mapply(circular_max, age.selected=age.arg,sex.selected=sex.arg)
mapply(circular_min, age.selected=age.arg,sex.selected=sex.arg)

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/com_rate_cod_',tolower(sex.filter[k]),'_',i,'_',cod.arg))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(file.loc.nat.output,'max_',year.start.arg,'_',year.end.arg))

# NATIONAL DEATH RATES
# INV COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/anti_com_rate_cod_',sex.filter[k],'_',i,'_',cod.arg))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

