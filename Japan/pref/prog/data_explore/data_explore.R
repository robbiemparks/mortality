rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# load data
filename <- paste0('../../output/prep_data/datjp_pref_rate_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# load prefecture lookup
dat.pref <- readRDS('../../data/pref/pref_lookup')

# add sourced data
source('../../data/objects/objects.R')

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat <- merge(dat,dat.year.month, by=c('year','month'))

# merge pref name to main file for plotting
dat <- merge(dat,dat.pref,by='pref_id',all.x=TRUE)

####

library(ggplot2)

# graph data by age for a particular state and sex
plot.state <- function(state=1,sex=1) {
   	ggplot(dat[dat$pref_id==state & dat$sex==sex,],aes(x=year.month)) +
    	geom_line(aes(y=100000*rate.adj),color='forestgreen') +
    	xlab(label='time') +
    	ylab(label='mortality rate (per 100,000)') +
        ggtitle(paste0(dat.pref[dat.pref$pref_id==state,][[1]],', ',sex.lookup[sex],': mortality rates by age-group')) +
    	facet_wrap(~age, scale='free') +
    	scale_colour_brewer(palette = "Set3") +
    	theme_bw()
}

# create output directory
ifelse(!dir.exists("../../output/data_explore"), dir.create("../../output/data_explore"), FALSE)

# plot all states for males
pdf(paste0('../../output/data_explore/states_by_age_male_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in dat.pref$pref_id) {
	print(plot.state(i,1))
}
dev.off()

# plot all states for females
pdf(paste0('../../output/data_explore/states_by_age_female_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in dat.pref$pref_id) {
	print(plot.state(i,2))
}
dev.off()

# graph data by state for a particular age and sex
plot.age <- function(age=0,sex=1) {
	dat$pref <- as.factor(dat$pref)
   	ggplot(dat[dat$age==age & dat$sex==sex,],aes(x=year.month)) +
    	geom_line(aes(y=rate.adj),color='forestgreen') +
    	xlab(label='time') +
    	ylab(label='mortality rate (per 100,000)') +
    	ggtitle(paste0(age.code[age.code$age==age,][[2]],', ',sex.lookup[sex],': mortality rates by agegroup')) +
    	facet_wrap(~pref) +
    	scale_colour_brewer(palette = "Set3") +
    	theme_bw()
}

# plot all ages for males
pdf(paste0('../../output/data_explore/age_by_state_male_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in c(0,5,15,25,35,45,55,65,75,85)) {
	print(plot.age(i,1))
}
dev.off()

# plot all ages for females
pdf(paste0('../../output/data_explore/age_by_state_female_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in c(0,5,15,25,35,45,55,65,75,85)) {
	print(plot.age(i,2))
}
dev.off()



