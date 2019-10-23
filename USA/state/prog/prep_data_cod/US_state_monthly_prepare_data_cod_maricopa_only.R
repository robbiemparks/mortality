rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(dplyr)
library(plyr)
library(foreign)

# source only the 'intentional' variable (better way to do this a la python?)
source('../../data/objects/objects.R')

# Function to summarise a year's data. x is the year.
# y is the number of rows. default (-1) is all rows.
yearsummary_cod  <- function(x=2000) {

	print(paste0('year ',x,' now being processed'))
  
	# load year of deaths
	file.loc <- '~/data/mortality/US/state/processed/cod/'
	dat.name <- paste0(file.loc,"deathscod",x,".dta")
	dat <- read.dta(dat.name)

	# isolate Maricopa county (could generalise to any county)
	dat = subset(dat, fips=='04013')

	# fix sex classification if in certain years
	if(x %in% c(2003:2010,2012)){
		dat$sex = plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1))
	}

	# add extra label for CODs based on relevant ICD year
	start_year = 1999
	if(x<start_year) {
		dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$cause.numeric = as.numeric(dat$cause)
		dat$cause.group = 	ifelse(dat$cause.numeric>=1400&dat$cause.numeric<=2399,'Cancer',
							ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Cardiopulmonary',
							ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=5199,'Cardiopulmonary',
							ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'External',
							'Other'))))
        dat$cause.group = as.character(dat$cause.group)

        # move deaths due to weather-based heat/cold to 'Other'
        dat$cause.group = ifelse(as.numeric(substr(dat$cause.numeric,1,3))==900|as.numeric(substr(dat$cause.numeric,1,3))==901,'Other',dat$cause.group)

		dat.merged = dat
	}
	if(x>=start_year){
		# merge cod in ICD 10 coding
        dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$letter = substr(dat$cause,1,1)
		dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)
        dat.merged$cause.group = as.character(dat.merged$cause.group)

        # move deaths due to weather-based heat/cold to 'Other'
        dat.merged$cause.group = ifelse((dat.merged$cause=='X300'|dat.merged$cause=='X310'),'Other',as.character(dat.merged$cause.group))

        # numerical cause
        dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))

        # fix 'other neoplasms' to be cancer
        dat.merged$cause.group = ifelse(dat.merged$letter=='D'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=489,'Cancer',dat.merged$cause.group)

        # move deaths due to Ottis Media to 'Cardiopulmonary'
        dat.merged$cause.group =
                            ifelse((dat.merged$letter=='H'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=669),'Cardiopulmonary',
                            as.character(dat.merged$cause.group))

        # to fix poisioning deaths
        dat.merged$cause.group = ifelse(dat.merged$letter=='X'&(dat.merged$cause.numeric==410|dat.merged$cause.numeric==420|dat.merged$cause.numeric==450|dat.merged$cause.numeric==490),'Other',dat.merged$cause.group)

        dat.merged$cause.numeric = NULL
	}

	# add agegroup groupings
  	dat.merged$agegroup <-  
              	ifelse (dat.merged$age<5,   0,
                ifelse (dat.merged$age<15,  5,        
                ifelse (dat.merged$age<25,  15, 
                ifelse (dat.merged$age<35,  25, 
                ifelse (dat.merged$age<45,  35, 
                ifelse (dat.merged$age<55,  45, 
                ifelse (dat.merged$age<65,  55, 
                ifelse (dat.merged$age<75,  65, 
                ifelse (dat.merged$age<85,  75,
                   	85)))))))))

	# summarise by state,year,month,sex,agegroup
    dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
    
  	names(dat.summarised)[1:7] <- c('cause','fips','year','month','sex','age','deaths')
	dat.summarised <- na.omit(dat.summarised)

	# create an exhaustive list of location sex age month
	fips 	=	c('04013')
	month 	= 	c(1:12)
	sex 	= 	c(1:2)
	age 	= 	c(0,5,15,25,35,45,55,65,75,85)
	cause 	=	c('Cancer','Cardiopulmonary','External','Other')

	complete.grid <- expand.grid(fips=fips,month=month,sex=sex,age=age,cause=cause)
	complete.grid$year <- unique(dat.summarised$year)

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('cause','fips','year','month','sex','age'),all.x='TRUE')

	# assign missing deaths to have value 0
	dat.summarised.complete$deaths <- ifelse(is.na(dat.summarised.complete$deaths)==TRUE,0,dat.summarised.complete$deaths)

	# print statistics of cause groups
	print(ddply(dat.summarised.complete,.(cause),summarise,deaths=sum(deaths)))

	print(paste0('total deaths ',sum(dat$deaths),' ',sum(dat.merged$deaths),' ',sum(dat.summarised.complete$deaths)))

  	return(dat.summarised.complete)
}

# Function to append all the years desired to be summarised into one file
appendyears  <- function(x=1980, y=1981) {
  	years <- x:y
  	z     <- x
  	dat   <- data.frame()
  	while (z %in% years) {
    		dat <- rbind(dat, yearsummary_cod(z))
    		print(paste0(z," done"))
    		z <- z+1
  	}
  	return(dat)
}

# append summarised dataset
dat.appended <- appendyears(year.start.arg,year.end.arg)

# reorder appended data
dat.appended <- dat.appended[order(dat.appended$cause,dat.appended$fips,dat.appended$sex,dat.appended$age,dat.appended$year,dat.appended$month),]

# Filter missing age results and complete results
#dat.appended.NA <- dat.appended[is.na(dat.appended$age),]
dat.appended   <- na.omit(dat.appended)

# Add USA label
dat.appended$iso3 <- "USA"

dat.appended$fips <- as.integer(as.character(dat.appended$fips))

# add inferred population data by day
pop.state <- readRDS('../../output/pop_us_infer/countyPopulations_infer_by_days_new_years')
pop.state$fips <- as.integer(pop.state$fips)

# merge deaths and population files FIX FFROM HERE
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','month','fips'))

# reorder
dat.merged <- dat.merged[order(dat.merged$cause,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
dat.merged$id = NULL

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

dat = dat.merged

# move old adjusted rate
dat$rate.adj.old <- dat$rate.adj

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat$leap <- as.integer(is.leapyear(dat$year))

# adjust deaths to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
dat$deaths.adj <- ifelse(dat$month %in% c(1,3,5,7,8,10,12), dat$deaths,
                  ifelse(dat$month %in% c(4,6,9,11), dat$deaths*(31/30),
                  ifelse((dat$month==2 & dat$leap==0), dat$deaths*(31/28),
                  ifelse((dat$month==2 & dat$leap==1), dat$deaths*(31/29),
                  'ERROR'
                  ))))
dat$deaths.adj <- as.numeric(dat$deaths.adj)

# calculate new rate.adj
dat$rate.adj <- dat$deaths.adj / dat$pop.adj

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod"), dir.create("../../output/prep_data_cod"), FALSE)

# output file as RDS
saveRDS(dat,paste0('../../output/prep_data_cod/datus_maricopa_rates_cod_',year.start.arg,'_',year.end.arg))

# test ggplot
library(ggplot2)

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat[,c('year', 'month')])
dat.year.month <- dat.year.month[order(dat.year.month$year,dat.year.month$month),]
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat <- merge(dat,dat.year.month, by=c('year','month'))

pdf('~/Desktop/death_rates_maricopa.pdf',paper='a4r',height=0,width=0)

ggplot() +
	geom_line(data=subset(dat,sex==2),aes(x=year.month,y=rate.adj*100000,color=cause)) +
	facet_wrap(~age,scale='free') +
	xlab('Time') +
	ylab('Death rate (per 100,000)') +
	ggtitle('Females Maricopa county 1982-2017')

ggplot() +
	geom_line(data=subset(dat,sex==1),aes(x=year.month,y=rate.adj*100000,color=cause)) +
	facet_wrap(~age,scale='free') +
	xlab('Time') +
	ylab('Death rate (per 100,000)') +
	ggtitle('Males Maricopa county 1982-2017')

dev.off()

pdf('~/Desktop/deaths_maricopa.pdf',paper='a4r',height=0,width=0)

ggplot(data=subset(dat,sex==2),aes(x=year.month,y=deaths,fill=cause)) +
	geom_bar(width = 0.9, stat = "identity") +
	facet_wrap(~age,scale='free') +
	xlab('Time') +
	ylab('Deaths') +
	ggtitle('Femles Maricopa county 1982-2017')

ggplot(data=subset(dat,sex==1),aes(x=year.month,y=deaths,fill=cause)) +
	geom_bar(width = 0.9, stat = "identity") +
	facet_wrap(~age,scale='free') +
	xlab('Time') +
	ylab('Deaths') +
	ggtitle('Males Maricopa county 1982-2017')

 dev.off()

