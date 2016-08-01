rm(list=ls())

dat <- readRDS('USA_rate_pred_type1a_1982_2010')

library(ggplot2)

dat.calif <- subset(dat,fips==6 & age==75 & sex==1)
dat.calif$time <- seq(1:nrow(dat.calif))

ggplot(data=dat.calif) + 
geom_line(color='blue',aes(x=time,y=rate.pred)) +
geom_ribbon(alpha=0.3,fill='blue',aes(x=time,ymax=rate.pred+1.96*sd,ymin=rate.pred-1.96*sd)) + 
ggtitle('75-84 males: California') + 
+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),rect = element_blank())
