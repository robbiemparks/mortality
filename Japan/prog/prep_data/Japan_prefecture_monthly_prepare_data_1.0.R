rm(list=ls())

library(dplyr)
library(foreign)

# function to summarise a year's data..
monthsummary.JPN  <- function(x=-1) {
    
    rds   <- "datmortjapan20160307"
    dat <- readRDS(rds)
    
    dat <- dat[c('deathyear','deathmonth','sex','age5','pref','deaths')]
    colnames(dat) <- c('year','month','sex','age','pref','deaths')
    dat.omitted <- dat[is.na(dat$age) | is.na(dat$year) | is.na(dat$month) | dat$pref==sort(unique(dat$pref))[1],]
    dat <- na.omit(dat)
    dat <- subset(dat,pref!=sort(unique(dat$pref))[1])
    # fix age classes. 999 appearing means something is going wrong
    dat$age <- ifelse (dat$age==0,   0,
    ifelse (dat$age==5,     5,
    ifelse (dat$age==10,    5,
    ifelse (dat$age==15,   15,
    ifelse (dat$age==20,   15,
    ifelse (dat$age==25,   25,
    ifelse (dat$age==30,   25,
    ifelse (dat$age==35,   35,
    ifelse (dat$age==40,   35,
    ifelse (dat$age==45,   45,
    ifelse (dat$age==50,   45,
    ifelse (dat$age==55,   55,
    ifelse (dat$age==60,   55,
    ifelse (dat$age==65,   65,
    ifelse (dat$age==70,   65,
    ifelse (dat$age==75,   75,
    ifelse (dat$age==80,   75,
    ifelse (dat$age==85,   85,
    ifelse (dat$age==90,   85,
    ifelse (dat$age==95,   85,
    ifelse (dat$age==100,  85,
    999)))))))))))))))))))))
    library(dplyr)
    dat <- dplyr::summarise(group_by(dat,year,month,sex,age,pref),sum(deaths))
    colnames(dat)[6] <- c('deaths')
    #dat$month <- month.name[as.numeric(dat$month)]
    return(dat)
}

dat.appended <- monthsummary.JPN()

# only retain 1980 - 2011 and up to 65 - 74 age group
dat.appended$age <- as.numeric(as.character(dat.appended$age))
dat.appended <- dat.appended[dat.appended$year>=1980 & dat.appended$age<75,]

# Add JPN label
dat.appended$iso3 <- "JPN"

# reorder appended data
dat.appended <- dat.appended[order(dat.appended$pref,dat.appended$sex,dat.appended$age,dat.appended$year,dat.appended$month),]
dat.appended <- dat.appended[,c(3,4,1,2,5,7,6)]

# fix sex label
levels(dat.appended$sex) <- c(2,1)
dat.appended$sex <- as.integer(as.character(dat.appended$sex))

# add inferred population data by day
pop.pref <- readRDS('prefPopulations_infer_by_days')

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.pref,by=c('sex','age','year','month','pref'))
 
# extract unique table of year and months to generate year.month
dat.merged.year.month <- unique(dat.merged[,c('year', 'month')])
dat.merged.year.month$month <- as.integer(dat.merged.year.month$month)
dat.merged.year.month <- dat.merged.year.month[order(dat.merged.year.month$year,dat.merged.year.month$month),]
dat.merged.year.month$year.month <- seq(nrow(dat.merged.year.month))

# merge year.month table with population table to create year.month id
dat.merged <- merge(dat.merged,dat.merged.year.month, by=c('year','month'))

# reorder
dat.merged <- dat.merged[order(dat.merged$pref,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

# plot to check rates
plot(dat.merged$rate,dat.merged$rate.adj)

# output file as RDS
saveRDS(dat.merged,'datjpn_pref_rates_1980_2011')
