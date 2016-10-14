rm(list=ls())

print('running population_weighted_mean.R')

library(ggplot2)
library(plyr)
library(foreign)
library(dplyr)

# create directory to place output files into
file.loc <- "../../output/plot_county_against_time/"
ifelse(!dir.exists(file.loc), dir.create(file.loc), FALSE)

# load county based population data
pop.county <- read.dta("~/data/mortality/US/state/processed/county/countyPopulationsnewyears.dta")

plot.function <- function(age.arg,sex.arg,state.arg) {
	print(
	ggplot(data=subset(pop.county,age==age.arg & sex==sex.arg & stateFips==state.arg)) + 
	geom_line(aes(x=year,y=pop,color=factor(fips))) +
	ggtitle(paste0(state.arg,'_',sex.arg,'_',age.arg)) + 
	theme_bw()
	)
}

for(i in unique(pop.county$stateFips)) {
	pdf(paste0(file.loc,i,'_',min(pop.county$year),'_',max(pop.county$year),'.pdf'),paper='a4r')
	for(j in unique(pop.county$sex)){
		for(k in unique(pop.county$age)){
			plot.function(k,j,i)
	}}
	dev.off()
}


