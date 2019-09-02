rm(list=ls())

# load data
dat = readRDS('~/Desktop/test_data_wavelet.rds')

library(RColorBrewer)
library(plyr)
library(scales)
library(ggplot2)

year.start.arg=1980
year.end.arg=2016

# this is the code which creates the colours from the palette you choose
colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# Plot below example
ggplot(dat=dat, aes(x=month,y=100000*rate.adj,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=c(1:12))   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    # scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 3,title = paste0("Year"))) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

