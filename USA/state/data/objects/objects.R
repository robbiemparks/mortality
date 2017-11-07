# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85), age.print=age.print)
month.names <- c('January','February','March','April','May','June', 'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
sex.lookup <- c('Men','Women')

# lookups
sex.lookup <- c('male','female')
sex.lookup2 <- c('Men','Women')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),age.print=age.print)
month.names <- c('January','February','March','April','May','June',
'July','August','September','October','November','December')

# add fips lookup
fips.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# INLA model versions
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0')

# lookups for temperature units
temp = c("10percc3", "90percc3", "meanc3")
episodes = c("number_of_min_3_day_above_+5_jumpupwaves_2", "number_of_min_3_day_above_nonnormal_90_upwaves_2", "number_of_min_3_day_below_+5_jumpdownwaves_2", "number_of_min_3_day_below_nonnormal_90_downwaves_2")
#unit.name = ifelse(metric %in% temp, paste0('°C'), ifelse(metric %in% episodes, ' episode(s)','error'))

# set color ramps
gr <- colorRampPalette(c("darkgreen","green","lightgreen"))(200)
bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
pr <- colorRampPalette(c("plum","orchid","darkmagenta"))(200)
yl <- colorRampPalette(c("lightgoldenrod", "gold","darkorange"))(200)
sm <- colorRampPalette(c("tan1","salmon2","salmon4"))(200)
