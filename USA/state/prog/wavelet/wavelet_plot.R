rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])

library(rgeos)
require(ggplot2)
library(rgdal)

# add fips lookup
fips.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
age.print=age.print)
month.names <- c('January','February','March','April','May','June',
'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
sex.lookup <- c('Men','Women')

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# 1. NATIONAL

# load national data
file.loc.nat <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
file.loc.nat <- paste0(file.loc.nat,num.sim,'_sim/')
ifelse(!dir.exists(paste0(file.loc.nat,'plots/')), dir.create(paste0(file.loc.nat,'plots/'),recursive=TRUE), FALSE)
dat.nat <- readRDS(paste0(file.loc.nat,'12_month_values/combined_results/power_12_months_national_',year.start.arg,'_',year.end.arg))

# output plot of wavelet 12 month value from first period against second
pdf(paste0(file.loc.nat,'12_month_power_national_comparison_xy_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.nat) +
geom_jitter(aes(x=twelve.month.value.1,y=twelve.month.value.2,color=as.factor(sex)),width = 10) +
xlab(paste0('12-month power from ',min(year.group.1),'-',max(year.group.1))) +
ylab(paste0('12-month power from ',min(year.group.2),'-',max(year.group.2))) +
ggtitle(paste0('National change in power at 12 months between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
theme_bw()
dev.off()

# output plot of wavelet 12 month value difference between first period and second
pdf(paste0(file.loc.nat,'12_month_power_national_comparison_change_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.nat) +
geom_jitter(aes(x=age,y=abs(twelve.month.value.1-twelve.month.value.2),color=as.factor(sex))) +
xlab('Age group') +
ylab('Change in power at 12 months') +
ggtitle(paste0('National change in power at 12 months between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
theme_bw()
dev.off()

# 2. STATE

# load state data
file.loc.state <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/state/")
file.loc.state <- paste0(file.loc.state,num.sim,'_sim/')
dat.state <- readRDS(paste0(file.loc,'12_month_values/combined_results/power_12_months_state_',year.start.arg,'_',year.end.arg))

###############################################################
# PREPARING MAP
###############################################################

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

# load shapefile
us <- readOGR(dsn="../../data/shapefiles",layer="states")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska,rotate=-50)
alaska <- elide(alaska,scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old versions of Alaska and Hawaii and put new ones back in
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]
us_aea <- rbind(us_aea, alaska, hawaii)

# fortify to prepare for ggplot
map <- fortify(us_aea)

# extract data from shapefile
shapefile.data <- us_aea@data

# add climate regions from Karl and Koss
shapefile.data$climate_region <- c('Northwest','Northern Rockies and Plains','Northeast','Northern Rockies and Plains','Northern Rockies and Plains',
'Northern Rockies and Plains','Upper Midwest','Northwest','Northeast','Upper Midwest',
'Northwest','Northeast','Upper Midwest','Northeast','Northern Rockies and Plains',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Ohio Valley','West','Southwest','West','Ohio Valley',
'Ohio Valley','Northeast','Northeast','Ohio Valley','Northeast',
'Southwest','Ohio Valley','South','Southeast','Ohio Valley',
'Southwest','South','Southeast','Ohio Valley','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','Upper Midwest','Northwest',
'West')

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))
