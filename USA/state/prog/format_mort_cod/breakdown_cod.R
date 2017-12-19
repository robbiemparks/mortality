rm(list=ls())

library(foreign)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
print(year)

# source variables
source('../../data/objects/objects.R')

# read files
dat = read.dta(paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))

# create directories for output
file.loc <- paste0('../../output/breakdown_cod/',year,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA",
"#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1",
"#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5",
"#5E738F", "#D1A33D", "#8A7C64", "#599861" )

if(year<1999){
    # SOMETHING FOR ICD 9
}
if(year>=1999){

    ##################################################
    ### 1. breakdown of broad groups cod by letter
    ##################################################

    # strip letter from front of cause
    dat$letter = substr(dat$cause,1,1)

    # COD look-up
	cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
								cause.group=c('Other','Other','Cancer','Cancer','Other', # A-E
											'Other','Other','Other','Cardiopulmonary','Cardiopulmonary', # F-J
											'Other','Other','Other','Other','Other', # K-O
											'Other','Other','Other','External','External', # P-T
											'Other','External','External','External','External', # U-Y
											'External'), # Z
								description=c('Infectious/Parasitic I','Infectious/Parasitic II','Neoplasms','Blood/Immune','Endocrine/Nutritional/Metabolic', # A-E
											'Mental/Behavioural/Neurodevelopmental','Nervous','Eye/Adnexa/Ear/Mastoid','Circulatory','Respiratory', # F-J
											'Digestive','Skin/Subcutaneous','Musculoskeletal/Connective Tissue','Genitourinary','Pregnancy/Childbirth/Puerperium', # K-O
											'Perinatal','Malformations/Deformations/Chromosomal','Abnormal Clinical/Lab','Injury','Poisoning', # P-T
											'Unknown','Transport','Slipping/Tripping/Exposure','Exposure/Suicide','War/Terrorism/Medical Misadventures', # U-Y
											'Unknown'))

    # merge cod in ICD 10 coding
    dat.merged = merge(dat,cod.lookup.10[c('letter','cause.group')],by.x='letter',by.y='letter',all.x=1)

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

    # establish the percentage of deaths by broad cause of death
    dat.count = as.data.frame(dplyr::count(dat.merged,agegroup,sex,cause.group,letter))
    dat.count = plyr::ddply(dat.count,c('agegroup','sex','cause.group'),mutate,percentage=round(100*n/sum(n),1))
    dat.count$age.long = plyr::mapvalues(dat.count$age,from=sort(unique(dat.count$agegroup)),to=as.character(age.code[,2]))
    dat.count$age.long <- reorder(dat.count$age.long,dat.count$agegroup)

    dat.count.app = as.data.frame(dplyr::count(dat.merged,sex,cause.group,letter))
    dat.count.app = plyr::ddply(dat.count.app,c('sex','cause.group'),mutate,percentage=round(100*n/sum(n),1))
    dat.count.app$age.long = 'All ages'
    dat.count.app$agegroup = -1

    dat.count = rbind(dat.count.app,dat.count)

    dat.count = merge(dat.count,cod.lookup.10,by=c('cause.group','letter'),all.x=1)

    # friendly names for plotting
    dat.count$sex.long = plyr::mapvalues(dat.count$sex,from=sort(unique(dat.count$sex)),to=c('Men','Women'))

    # function to plot absolutely or relatively
    plot = function(age.sel=-1,y.measure=1){

        dat.count.sub = subset(dat.count,agegroup==age.sel)

        p = ggplot(data=subset(dat.count.sub,letter!='U')) +
        facet_wrap(~sex.long) +
        xlab('Cause Group') + ylab('Number of deaths') + ggtitle(paste0(year,', ',unique(dat.count.sub$age.long))) +
        scale_fill_manual(values=mycols) +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=30), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center', legend.text=element_text(size=8),
        legend.background = element_rect(fill="gray90", size=.4, linetype="dotted"))

        if(y.measure==1){p = p + geom_bar(aes(y = n, x = cause.group, fill=description),stat="identity")+ ylab('Number of deaths')}
        if(y.measure==2){p = p + geom_bar(aes(y = percentage, x = cause.group, fill=description),stat="identity")+ ylab('Percentage')}

        print(p)

    }

    pdf(paste0(file.loc,year,'_absolute.pdf'),height=0,width=0,paper='a4r')
    for(i in c(-1,0,5,15,25,35,45,55,65,75,85)){
        plot(i,1)
    }
    dev.off()

    pdf(paste0(file.loc,year,'_relative.pdf'),height=0,width=0,paper='a4r')
    for(i in c(-1,0,5,15,25,35,45,55,65,75,85)){
        plot(i,2)
    }
    dev.off()

    ##################################################
    ### 2. breakdown of cod by gbd
    ##################################################

    # potentially another way
    #c('J00',sprintf('J%s',seq(1:99)))

    # Intentional injuries
    intentional = c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
    'X70','X71','X72','X73','X74','X741','X742','X743','X744','X749',
    'X75','X76','X77','X78','X79','X80','X81','X82','X83','X84',
    'X85','X86', 'X87','X88','X89','X90','X91','X92','X93','X94',
    'X95','X951', 'X952','X953','X954','X959','X96','X97','X98','X99',
    'Y00','Y01', 'Y02','Y03','Y04','Y05','Y06','Y060','Y061','Y062',
    'Y068','Y069', 'Y07','Y070','Y071','Y072','Y073','Y078','Y079','Y08',
    'Y09','Y35', 'Y350','Y351','Y352','Y353','Y354','Y355','Y356','Y357',
    'U011','Y36', 'Y360','Y361','Y362','Y363','Y364','Y365','Y366','Y367',
    'Y368','Y369', 'Y870','Y871', 'Y890','Y891')

    # Unintentional injuries TO FINISH TEMP SOLUTION
    dat.merged$cause.description = ifelse(dat.merged$cause %in% intentional, 'Intentional','Other')

    # establish the percentage of deaths by broad cause of death
    dat.count = as.data.frame(dplyr::count(dat.merged,agegroup,sex,cause.group,cause.description))
    dat.count = plyr::ddply(dat.count,c('agegroup','sex','cause.group'),mutate,percentage=round(100*n/sum(n),1))
    dat.count$age.long = plyr::mapvalues(dat.count$age,from=sort(unique(dat.count$agegroup)),to=as.character(age.code[,2]))
    dat.count$age.long <- reorder(dat.count$age.long,dat.count$agegroup)

    dat.count.app = as.data.frame(dplyr::count(dat.merged,sex,cause.group,cause.description))
    dat.count.app = plyr::ddply(dat.count.app,c('sex','cause.group'),mutate,percentage=round(100*n/sum(n),1))
    dat.count.app$age.long = 'All ages'
    dat.count.app$agegroup = -1

    dat.count = rbind(dat.count.app,dat.count)

    #dat.count = merge(dat.count,cod.lookup.10,by=c('cause.group','letter'),all.x=1)

    # friendly names for plotting
    dat.count$sex.long = plyr::mapvalues(dat.count$sex,from=sort(unique(dat.count$sex)),to=c('Men','Women'))

      # function to plot absolutely or relatively
    plot = function(age.sel=-1,y.measure=1){

        dat.count.sub = subset(dat.count,agegroup==age.sel)

        p = ggplot(data=subset(dat.count.sub)) +
        facet_wrap(~sex.long) +
        xlab('Cause Group') + ylab('Number of deaths') + ggtitle(paste0(year,', ',unique(dat.count.sub$age.long))) +
        scale_fill_manual(values=mycols) +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=30), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center', legend.text=element_text(size=8),
        legend.background = element_rect(fill="gray90", size=.4, linetype="dotted"))

        if(y.measure==1){p = p + geom_bar(aes(y = n, x = cause.group, fill=cause.description),stat="identity")+ ylab('Number of deaths')}
        if(y.measure==2){p = p + geom_bar(aes(y = percentage, x = cause.group, fill=cause.description),stat="identity")+ ylab('Percentage')}

        print(p)

    }

    pdf(paste0(file.loc,year,'_absolute_external_highlight.pdf'),height=0,width=0,paper='a4r')
    for(i in c(-1,0,5,15,25,35,45,55,65,75,85)){
        plot(i,1)
    }
    dev.off()

    pdf(paste0(file.loc,year,'_relative_external_highlight.pdf'),height=0,width=0,paper='a4r')
    for(i in c(-1,0,5,15,25,35,45,55,65,75,85)){
        plot(i,2)
    }
    dev.off()



}

