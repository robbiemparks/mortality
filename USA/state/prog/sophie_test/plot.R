# create dummy data
set.seed(12423) ; t = seq(1:100)
set.seed(21341) ; y = rnorm(100)
dat = data.frame(t=t,y=y)

# call ggplot
library(ggplot2)

# plot pretty graph
# I recommend going through and just figuring out what each thing does but copying and pasting the code
# with one section at a time missing
ggplot(data=dat) +
    geom_point(aes(x=t,y=y,color='red')) +
    geom_line(aes(x=t,y=y,color='blue')) +
    xlab('Time axis label') +
    ylab('Climate variable label') +
    guides(color=FALSE) +
    geom_hline(linetype=2, yintercept = 0, alpha=0.5) +
    ggtitle('Title here') +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))