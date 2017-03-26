rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f')
model <- models[model]

# create directories for output
file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load the data
dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric))

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
#model <- paste0('type',model)

# OBJECTIVES
# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
# for state model, plot climate parameters on map all on one page, one for men and one for women

if(model=='1d'){
    

# attach long age names
dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
dat$age.long <- reorder(dat$age.long,dat$age)


# national month intercept male
pdf(paste0(file.loc,'climate_month_params_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    

    print(ggplot(data=subset(dat,sex==1)) +
    geom_line(aes(x=ID,y=mean)) +
    geom_ribbon(aes(x=ID,ymax=`0.975quant`,ymin=`0.025quant`),alpha=0.1,fill='red') +
    geom_hline(yintercept=0,alpha=0.5,linetype=2) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab('month') +
    ylab('month intercept') +
    ggtitle(paste0('Men month intercepts ',metric,' ',dname)) +
    scale_colour_discrete(name="State") +
    guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
    facet_wrap(~age.long) +
    theme(legend.position="bottom"))
    
    dev.off()
    
    pdf(paste0(file.loc,'climate_month_params_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    
    
    print(ggplot(data=subset(dat,sex==2)) +
    geom_line(aes(x=ID,y=mean)) +
    geom_ribbon(aes(x=ID,ymax=`0.975quant`,ymin=`0.025quant`),alpha=0.1,fill='red') +
    geom_hline(yintercept=0,alpha=0.5,linetype=2) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab('month') +
    ylab('month intercept') +
    ggtitle(paste0('Men month intercepts ',metric,' ',dname)) +
    scale_colour_discrete(name="State") +
    guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
    facet_wrap(~age.long) +
    theme(legend.position="bottom"))
    
    dev.off()
    
}

if(model=='1e'){
    
    ###############################################################
    # PREPARING MAP
    ###############################################################
    
    # for theme_map
    #devtools::source_gist("33baa3a79c5cfef0f6df")
    theme_map <- function(base_size=9, base_family=""){
        require(grid)
        theme_bw(base_size=base_size,base_family=base_family) %+replace%
        theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.margin=unit(0,"lines"),
        plot.background=element_blank(),
        legend.justification = c(0,0),
        legend.position = c(0,0)
        )
    }
    
    
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
    
    shapefile.data$SUB_REGION <- c('Far West','Rocky Mountain','New England','Plains','Plains',
				'Rocky Mountain','Great Lakes','Rocky Mountain','New England','Plains',
                'Far West','New England','Plains','New England','Plains',
                'Mideast','Mideast','New England','New England','Mideast',
                'Great Lakes','Far West','Rocky Mountain','Far West','Great Lakes',
                'Great Lakes','Mideast','Mideast','Southeast','Mideast',
                'Rocky Mountain','Southeast','Plains','Southeast','Plains',
                'Southwest','Southwest','Southeast','Southeast','Southwest',
                'Southwest','Southeast','Southeast','Southeast','Southeast',
                'Southeast','Southeast','Southeast','Great Lakes','Far West',
                'Far West')
                
                shapefile.data$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
                'West North Central','Upper Midwest','Northwest','Northeast','Upper Midwest',
                'Northwest','Northeast','Upper Midwest','Northeast','West North Central',
                'Northeast','Northeast','Northeast','Northeast','Northeast',
                'East North Central','West','Southwest','West','East North Central',
                'East North Central','Northeast','Northeast','East North Central','Northeast',
                'Southwest','East North Central','South','Southeast','East North Central',
                'Southwest','South','Southeast','East North Central','South',
                'Southwest','Southeast','South','Southeast','Southeast',
                'South','South','Southeast','Upper Midwest','Northwest',
                'West')
                
                # merge selected data to map dataframe for colouring of ggplot
                USA.df <- merge(map, shapefile.data, by='id')
                USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))
                
                # extract coordinates of states
                USA.coords <- as.data.frame(coordinates(us))
                names(USA.coords)[1:2] <- c('long','lat')
                USA.coords$DRAWSEQ <- 1:nrow(USA.coords)
                USA.coords <- merge(USA.coords, shapefile.data, by='DRAWSEQ')
                USA.coords$STATE_FIPS <- as.numeric(as.character(USA.coords$STATE_FIPS))
                
                # attach coordinate information for colouring
                dat <- merge(dat, USA.coords, by.x='fips',by.y='STATE_FIPS')
                
                # set colour scheme for geographical colour map
                map.region.colour <- colorRampPalette(rev(brewer.pal(12,"Accent")[c(1:3,5,6)]))(length(unique(USA.df$SUB_REGION)))
                names(map.region.colour) <- levels(as.factor(USA.df$SUB_REGION))
                map.climate.colour <- colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5)]
                
                # merge selected data to map dataframe for colouring of ggplot
                plot <- merge(USA.df,dat,by.x=c('STATE_FIPS','DRAWSEQ'),by.y=c('fips','DRAWSEQ'))
                plot.median <- with(plot, plot[order(sex,age,DRAWSEQ,order),])
                
                # function to plot age for all months subnationally
                plot.function.age <- function(sex.sel,age.sel) {
                    
                    # find limits for plot
                    min.plot <- min(plot$mean)
                    max.plot <- max(plot$mean)
                    
                    # attach long month names
                    plot$month.short <- mapvalues(plot$ID,from=sort(unique(plot$ID)),to=month.short)
                    plot$month.short <- reorder(plot$month.short,plot$ID)

                    # long age name for title
                    age.long <- as.character(age.code[age.code$age==age.sel,2])
                    
                    # plotting
                    print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long.x,y=lat.x,group=group)) +
                    geom_polygon(aes(fill=mean),color='black',size=0.01) +
                    scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = '')) +
                    facet_wrap(~month.short) +
                    ggtitle(sex.sel) +
                    ggtitle(paste0(age.long,' ',sex.lookup[sex.sel],' : ',metric,' ',dname,' risk by month ',year.start,'-',year.end)) +
                    theme_map() +
                    theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
                }
                
                # male output to pdf
                pdf(paste0(file.loc,'climate_month_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
                for(i in sort(unique(dat$age))){plot.function.age(1,i)}
                dev.off()
                
                # female output to pdf
                pdf(paste0(file.loc,'climate_month_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
                for(i in sort(unique(dat$age))){plot.function.age(2,i)}
                dev.off()
                
                
                # function to plot age for all months subnationally
                plot.function.month <- function(sex.sel,month.sel) {
                    
                    # find limits for plot
                    min.plot <- min(plot$mean)
                    max.plot <- max(plot$mean)
                    
                    # attach long month names
                    plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
                    plot$age.long <- reorder(plot$age.long,plot$age)
                    
                    print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long.x,y=lat.x,group=group)) +
                    geom_polygon(aes(fill=mean),color='black',size=0.01) +
                    scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = '')) +
                    facet_wrap(~age.long) +
                    ggtitle(sex.sel) +
                    ggtitle(paste0(sex.lookup[sex.sel],' : ',metric,' ',dname,' risk by age for ',month.short[month.sel],' ',year.start,'-',year.end)) +
                    theme_map() +
                    theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
                    
                }
                
                # male output to pdf
                pdf(paste0(file.loc,'climate_age_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
                for(i in c(1:12)){plot.function.month(1,i)}
                dev.off()
                
                # female output to pdf
                pdf(paste0(file.loc,'climate_age_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
                for(i in sort(unique(dat$age))){plot.function.month(2,i)}
                dev.off()
                
                

}

