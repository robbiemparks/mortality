rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(scales)
# library(ggtree)

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)



# plot the tree diagram
pdf(paste0(file.loc,'tree_structure_cods.pdf'),paper='a4r',height=0,width=0)
    # code for tree diagram here
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

