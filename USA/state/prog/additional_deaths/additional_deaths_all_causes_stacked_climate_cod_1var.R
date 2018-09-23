rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
#library(maptools)
#library(mapproj)
#library(rgeos)
#library(rgdal)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
# cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[7])
# num.draws <- as.numeric(args[9])

#year.start = 1980 ; year.end = 2013 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=0

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_causes/stacked/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_causes/stacked/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# # fix name for plotting
# cod.print = ifelse(cause=='AllCause', 'All cause',
#         ifelse(cause=='Cancer', 'Cancers',
#         ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
#         ifelse(cause=='External', 'Injuries',
#         ifelse(cause=='Other', 'Other',
#         ifelse(cause=='Intentional','Intentional injuries',
#         ifelse(cause=='Unintentional','Unintentional injuries',
#         ifelse(cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
#         ifelse(cause=='Transport accidents','Transport',
#         ifelse(cause=='Intentional self-harm','Intentional self-harm', # TO ASK MAJID. 'Suicides' ???
#         ifelse(cause=='Accidental falls','Falls',
#         ifelse(cause=='Accidental drowning and submersion','Drownings',
#         ifelse(cause=='Assault','Assault','NA'
#         )))))))))))))

# causes.injury = c('External')
# causes.intention = c('Intentional','Unintentional')
# causes.intentional = c('Assault','Intentional self-harm')
# causes.unintentional = c('Accidental falls', 'Accidental drowning and submersion', 'Transport accidents', 'Other external causes of injury')
# causes.all = c(causes.injury,causes.intention,causes.intentional,causes.unintentional)

causes.all = c('Cardiopulmonary','Cancer','External','Other')

# load the data for each set of causes
for(i in causes.all){
    if(contig==1){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',i,'_fast_contig'))
    }
    if(contig==0){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',i,'_fast'))
    }
    j=gsub(' ','_',i) ; j=gsub('-','_',j)
    try(do.call("<-", list(paste0('parameters.',j), dat)))
}

# for national model, plot additional deaths (with CIs) all on one page, one for men and one for women
if(model%in%c('1d','1d2')){

    # # load death rate data and create national death rates
    # dat.injury <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
    # dat.injury <- subset(dat.injury,cause=='External')
    #
    # # dat.intention <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start,'_',year.end))
    #
    # dat.sub = readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    # dat.sub$cause.sub = NULL ; names(dat.sub)[6] = 'cause'
    # dat.intentional = subset(dat.sub,cause=='Intentional')
    # dat.unintentional = subset(dat.sub,cause=='Unintentional')
    #
    # dat.sub <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    # dat.sub$cause.group = NULL ; names(dat.sub)[6] = 'cause'

    # load death rate data and create national death rates
    dat.all = readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_2016'))

    # get rid of Alaska and Hawaii
    only.contiguous = function(dat){
        dat.filtered = subset(dat,!(fips%in%c(2,15)))
        return(dat.filtered)
    }

    dat.all = only.contiguous(dat.all)

    dat.cancer = subset(dat.all,cause=='Cancer')
    dat.cardio = subset(dat.all,cause=='Cardiopulmonary')
    dat.injuries = subset(dat.all,cause=='External')
    dat.other = subset(dat.all,cause=='Other')

    # make for national data
    make.national = function(data){
        data$deaths.pred <- with(data,pop.adj*rate.adj)
        dat.national <- ddply(data,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
        dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
        dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
        return(dat.national)
    }

    dat.cancer.nat = make.national(dat.cancer)
    dat.cardio.nat = make.national(dat.cardio)
    dat.injuries.nat = make.national(dat.injuries)
    dat.other.nat = make.national(dat.other)

    # additional deaths by cause, age and sex
    additional.deaths.function = function(dat,parameters, year.selected){

    dat.injury.merged <- merge(dat,parameters,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)

    # calculate additional deaths
    dat.injury.merged$deaths.added <- with(dat.injury.merged,odds.mean*deaths.pred)
    dat.injury.merged$deaths.added.two.deg <- with(dat.injury.merged,((odds.mean+1)^2-1)*deaths.pred)

    # take one year
    dat.injury.merged.sub <- subset(dat.injury.merged,year==year.selected)

    dat.by.age.sex = ddply(dat.injury.merged.sub,.(sex,age),summarise,deaths.added=sum(deaths.added.two.deg))

    return(dat.by.age.sex)

    }

    # summary for each cause of death and grouping for 2 degrees scenario by age and sex
    dat.cardio.summary      = additional.deaths.function(dat.cardio.nat,parameters.Cardiopulmonary,year.end) ; dat.cardio.summary$cause = 'Cardiorespiratory'
    dat.other.summary       = additional.deaths.function(dat.cardio.nat,parameters.Other,year.end) ; dat.other.summary$cause = 'Other'
    dat.injury.summary      = additional.deaths.function(dat.injuries.nat,parameters.External,year.end) ; dat.injury.summary$cause = 'Injuries'
    dat.cancer.summary      = additional.deaths.function(dat.cancer.nat,parameters.Cancer,year.end) ; dat.cancer.summary$cause = 'Cancer'

    dat.all.summary = rbind(dat.cardio.summary, dat.other.summary, dat.injury.summary, dat.cancer.summary)
    dat.all.summary.diff = ddply(dat.all.summary,.(sex,age),summarise,deaths.added.total=sum(deaths.added))

    # additional deaths by cause, age and sex
    additional.deaths.function.month = function(dat,parameters, year.selected){

    dat.injury.merged <- merge(dat,parameters,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)

    # calculate additional deaths
    dat.injury.merged$deaths.added <- with(dat.injury.merged,odds.mean*deaths.pred)
    dat.injury.merged$deaths.added.two.deg <- with(dat.injury.merged,((odds.mean+1)^2-1)*deaths.pred)

    # take one year
    dat.injury.merged.sub <- subset(dat.injury.merged,year==year.selected)

    dat.by.month.sex = ddply(dat.injury.merged.sub,.(sex,month),summarise,deaths.added=sum(deaths.added.two.deg))

    return(dat.by.month.sex)

    }

        # summary for each cause of death and grouping for 2 degrees scenario by age and sex

    dat.cardio.month.summary      = additional.deaths.function.month(dat.cardio.nat,parameters.Cardiopulmonary,year.end) ; dat.cardio.month.summary$cause = 'Cardiorespiratory'
    dat.other.month.summary       = additional.deaths.function.month(dat.cardio.nat,parameters.Other,year.end) ; dat.other.month.summary$cause = 'Other'
    dat.injury.month.summary      = additional.deaths.function.month(dat.injuries.nat,parameters.External,year.end) ; dat.injury.month.summary$cause = 'Injuries'
    dat.cancer.month.summary      = additional.deaths.function.month(dat.cancer.nat,parameters.Cancer,year.end) ; dat.cancer.month.summary$cause = 'Cancer'

    dat.all.month.summary = rbind(dat.cardio.month.summary, dat.other.month.summary, dat.injury.month.summary, dat.cancer.month.summary)
    dat.all.summary.month.diff = ddply(dat.all.month.summary,.(sex,month),summarise,deaths.added.total=sum(deaths.added))

    # functions for plotting
    add.sex.long = function(dat){
        dat$sex.long = mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Male','Female'))
        dat$sex.long <- reorder(dat$sex.long,dat$sex)

        return(dat)
    }
    add.age.long = function(dat){
       dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
       dat$age.long <- reorder(dat$age.long,dat$age)

        return(dat)
    }
    add.month.short = function(dat){
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=c(as.character(month.short)))
        dat$month.short <- reorder(dat$month.short,dat$month)

        return(dat)
    }

    # processing for plotting
    # SEX NAMES
    dat.all.summary = add.sex.long(dat.all.summary)
    dat.all.summary.diff = add.sex.long(dat.all.summary.diff)
    dat.all.month.summary = add.sex.long(dat.all.month.summary)
    dat.all.month.summary.diff = add.sex.long(dat.all.summary.month.diff)

    # LONG AGES
    dat.all.summary = add.age.long(dat.all.summary)
    dat.all.summary.diff = add.age.long(dat.all.summary.diff)

    # SHORT MONTH NAMES
    dat.all.month.summary = add.month.short(dat.all.month.summary)
    dat.all.month.summary.diff = add.month.short(dat.all.month.summary.diff)

    # life expectancy in 2016 for years of life lost
    le.male = 76.1
    le.female = 81.1
    dat.all.summary$age.mean = dat.all.summary$age + 5
    dat.all.summary.male = subset(dat.all.summary,sex==1) ;  dat.all.summary.female = subset(dat.all.summary,sex==2)
    dat.all.summary.male$yll = ifelse((le.male-dat.all.summary.male$age.mean)>0,le.male-dat.all.summary.male$age.mean,0)
    dat.all.summary.female$yll = ifelse((le.female-dat.all.summary.female$age.mean)>0,le.female-dat.all.summary.female$age.mean,0)
    dat.all.summary = rbind(dat.all.summary.male,dat.all.summary.female)
    dat.all.summary$yll.total = dat.all.summary$deaths.added * dat.all.summary$yll

    min.plot = -5000
    max.plot = 1500

    # 1. All causes -> 4 main causes
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_allcause_to_causes_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.all.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        # geom_point(data=dat.all.summary,aes(x=as.factor(age.long),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.broad.cod) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 500),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_allcause_to_causes_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.all.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total),size=2) +
        # geom_point(data=dat.all.month.summary,aes(x=as.factor(month.short),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.broad.cod) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 500),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_allcause_to_causes_yll_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.summary, aes(x=as.factor(age.long),y=yll.total,fill=cause), stat='identity') +
        # geom_point(data=dat.all.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        # geom_point(data=dat.all.summary,aes(x=as.factor(age.long),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Years of life lost with 2 degrees \n additional warming (based on 2016 population)') +
        scale_fill_manual(values=colors.broad.cod) +
        scale_y_continuous(breaks = seq(-15000, 25000, by = 5000),limits=c(-15000,25000),label=comma) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()


    # 2. Intentional -> suicide and assault
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.self.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.intentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.self.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.self.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_monthly_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.intentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.self.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()


    # 3. Unintentional -> transport, falls, drownings, other
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.unintentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.unintentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.unintentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_monthly_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.unintentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    # # save output as csvs
    # write.csv(additional.deaths.summary,paste0(file.loc,country,'_rate_pred_type',model,
    #         '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig_by_age.csv'))
    # write.csv(additional.deaths.monthly,paste0(file.loc,country,'_rate_pred_type',model,
    #         '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig_by_month.csv'))
}  #