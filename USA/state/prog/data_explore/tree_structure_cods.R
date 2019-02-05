rm(list=ls())

library(ggplot2)
library(RColorBrewer)
library(scales)

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

pdf(paste0(file.loc,'tree_structure_cods.pdf'),paper='a4r',height=0,width=0)


dev.off()

