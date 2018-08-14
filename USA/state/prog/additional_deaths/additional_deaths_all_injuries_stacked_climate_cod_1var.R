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

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=1

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output DELETE OLD DIRECTORIES
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_injuries/stacked/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/stacked/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# fix name for plotting
cod.print = ifelse(cause=='AllCause', 'All cause',
        ifelse(cause=='Cancer', 'Cancers',
        ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
        ifelse(cause=='External', 'Injuries',
        ifelse(cause=='Other', 'Other',
        ifelse(cause=='Intentional','Intentional injuries',
        ifelse(cause=='Unintentional','Unintentional injuries',
        ifelse(cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
        ifelse(cause=='Transport accidents','Transport',
        ifelse(cause=='Intentional self-harm','Intentional self-harm', # TO ASK MAJID. 'Suicides' ???
        ifelse(cause=='Accidental falls','Falls',
        ifelse(cause=='Accidental drowning and submersion','Drownings',
        ifelse(cause=='Assault','Assault','NA'
        )))))))))))))

causes.injury = c('External')
causes.intention = c('Intentional','Unintentional')
causes.intentional = c('Assault','Intentional self-harm')
causes.unintentional = c('Accidental falls', 'Accidental drowning and submersion', 'Transport accidents')     #, 'Other external causes of injury')
causes.all = c(causes.injury,causes.intention,causes.intentional,causes.unintentional)

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

    # load death rate data and create national death rates
    dat.injury <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
    dat.injury <- subset(dat.injury,cause=='External')

    # dat.intention <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start,'_',year.end))

    dat.sub = readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    dat.sub$cause.sub = NULL ; names(dat.sub)[6] = 'cause'
    dat.intentional = subset(dat.sub,cause=='Intentional')
    dat.unintentional = subset(dat.sub,cause=='Unintentional')

    dat.sub <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    dat.sub$cause.group = NULL ; names(dat.sub)[6] = 'cause'

    # get rid of Alaska and Hawaii
    dat.injury = subset(dat.injury,!(fips%in%c(2,15)))
    # dat.intention = subset(dat.intention,!(fips%in%c(2,15)))
    dat.unintentional = subset(dat.unintentional,!(fips%in%c(2,15)))
    dat.intentional = subset(dat.intentional,!(fips%in%c(2,15)))
    dat.sub = subset(dat.sub,!(fips%in%c(2,15)))

    dat.drowning = subset(dat.sub,cause=='Accidental drowning and submersion')
    dat.falls = subset(dat.sub,cause=='Accidental falls')
    dat.transport = subset(dat.sub,cause=='Transport accidents')
    dat.other = subset(dat.sub,cause=='Other external causes of injury')
    dat.suicide = subset(dat.sub,cause=='Intentional self-harm')
    dat.assault = subset(dat.sub,cause=='Assault')

    # make for national data
    make.national = function(data){
        data$deaths.pred <- with(data,pop.adj*rate.adj)
        dat.national <- ddply(data,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
        dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
        dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
        return(dat.national)
    }

    dat.injury.nat = make.national(dat.injury)
    dat.intention.nat = make.national(dat.intention)
    dat.intentional.nat = make.national(dat.intentional)
    dat.unintentional.nat = make.national(dat.unintentional)
    dat.sub.nat = make.national(dat.sub)
    dat.drowning.nat = make.national(dat.drowning)
    dat.falls.nat = make.national(dat.falls)
    dat.transport.nat = make.national(dat.transport)
    dat.other.nat = make.national(dat.other)
    dat.suicide.nat = make.national(dat.suicide)
    dat.assault.nat = make.national(dat.assault)

    # additional deaths by cause
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
    dat.injury.summary      = additional.deaths.function(dat.injury.nat,parameters.External,year.end)

    dat.intentional.summary = additional.deaths.function(dat.intentional.nat,parameters.Intentional,year.end) ; dat.intentional.summary$cause = 'Intentional'
    dat.unintentional.summary = additional.deaths.function(dat.unintentional.nat,parameters.Unintentional,year.end) ; dat.unintentional.summary$cause = 'Unintentional'
    dat.intent.summary = rbind(dat.intentional.summary,dat.unintentional.summary) ;
    dat.intent.summary.diff = ddply(dat.intent.summary,.(sex,age),summarise,deaths.added.total=sum(deaths.added))

    dat.assault.summary = additional.deaths.function(dat.assault.nat,parameters.Assault,year.end) ; dat.assault.summary$cause = 'Assault'
    dat.suicide.summary = additional.deaths.function(dat.suicide.nat,parameters.Intentional_self_harm,year.end) ; dat.suicide.summary$cause = 'Intentional self-harm'
    dat.self.summary = rbind(dat.assault.summary,dat.suicide.summary)

    # FINISH TO UNINTENTIONAL DEATHS AFTER OTHER CAUSES HAS RUN

    # processing for plotting
    # SEX NAMES
    dat.intent.summary$sex.long <- mapvalues(dat.intent.summary$sex,from=sort(unique(dat.intent.summary$sex)),to=c('Male','Female'))
    dat.intent.summary$sex.long <- reorder(dat.intent.summary$sex.long,dat.intent.summary$sex)

    dat.injury.summary$sex.long <- mapvalues(dat.injury.summary$sex,from=sort(unique(dat.injury.summary$sex)),to=c('Male','Female'))
    dat.injury.summary$sex.long <- reorder(dat.injury.summary$sex.long,dat.injury.summary$sex)

    dat.intent.summary.diff$sex.long <- mapvalues(dat.intent.summary.diff$sex,from=sort(unique(dat.intent.summary.diff$sex)),to=c('Male','Female'))
    dat.intent.summary.diff$sex.long <- reorder(dat.intent.summary.diff$sex.long,dat.intent.summary.diff$sex)

    # LONG AGES
    dat.intent.summary$age.long <- mapvalues(dat.intent.summary$age,from=sort(unique(dat.intent.summary$age)),to=c(as.character(age.code[,2])))
    dat.intent.summary$age.long <- reorder(dat.intent.summary$age.long,dat.intent.summary$age)

    dat.injury.summary$age.long <- mapvalues(dat.injury.summary$age,from=sort(unique(dat.injury.summary$age)),to=c(as.character(age.code[,2])))
    dat.injury.summary$age.long <- reorder(dat.injury.summary$age.long,dat.injury.summary$age)

    dat.intent.summary.diff$age.long <- mapvalues(dat.intent.summary.diff$age,from=sort(unique(dat.intent.summary.diff$age)),to=c(as.character(age.code[,2])))
    dat.intent.summary.diff$age.long <- reorder(dat.intent.summary.diff$age.long,dat.intent.summary.diff$age)

    # SHORT MONTH NAMES
    # additional.deaths.summary.monthly$month.short <- mapvalues(additional.deaths.summary.monthly$month,from=sort(unique(additional.deaths.summary.monthly$month)),to=c(as.character(month.short),'All months'))
    # additional.deaths.summary.monthly$month.short <- reorder(additional.deaths.summary.monthly$month.short,additional.deaths.summary.monthly$month)

    # 1. External -> intentional and unintentional
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.intent.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intent.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        geom_point(data=dat.injury.summary,aes(x=as.factor(age.long),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees additional warming (based on 2016 population)') +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        facet_wrap(~sex.long) +
        ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.intent.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.injury.summary,aes(x=as.factor(age.long),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees additional warming (based on 2016 population)') +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        facet_wrap(~sex.long) +
        ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
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
        geom_bar(data=dat.self.summary, aes(x=as.factor(age),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intentional.summary,aes(x=as.factor(age),y=deaths.added),size=2) +
        xlab('Age (years)') + ylab('Additional deaths with 2 degrees additional warming (based on 2016 population)') +
        facet_wrap(~sex) +
        scale_fill_manual(values=c('green','brown')) +
        ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    # 3. Unintentional -> transport, falls, drownings, other FINISH once other runs done and processed


    # # save output as csvs
    # write.csv(additional.deaths.summary,paste0(file.loc,country,'_rate_pred_type',model,
    #         '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig_by_age.csv'))
    # write.csv(additional.deaths.monthly,paste0(file.loc,country,'_rate_pred_type',model,
    #         '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig_by_month.csv'))
    #
}