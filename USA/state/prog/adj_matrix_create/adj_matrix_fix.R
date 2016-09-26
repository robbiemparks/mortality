rm(list=ls())

# load text file with adjacency matrix
dat <- read.table('~/git/mortality/USA/state/output/adj_matrix_create/USA.graph',header=F, sep= ' ', col.names = (paste0('V',seq_len(10))),fill=T)

# adjust the file to fix Hawaii and Alaska connections
# connect Hawaii to California, and vice versa
dat[2,c(2,3)] <- c(1,25)
dat[26,c(2:6)] <- c(4,1,12,23,37)
# connect Alaska to Washington, and vice versa
dat[3,c(2:5)] <- c(3,9,12,51)
dat[52,c(2,3)] <- c(1,2)

# replace NAs with blanks
dat[is.na(dat)] <- ' '

# save file as txt file
# FIX OUTPUT BEFORE IMPLEMENTING IN BASH FILE
write.table(dat, '~/git/mortality/USA/state/output/adj_matrix_create/USA.graph.edit.test', sep="\t", row.names=FALSE,col.names=FALSE)
