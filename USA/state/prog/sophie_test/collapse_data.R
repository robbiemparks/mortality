############################
# YEARLY DATA
############################

# number of location
n = 100

# load data
# create dummy data (where you would load data)
set.seed(12423) ; loc = seq(1:n)
set.seed(21341) ; y = rnorm(n,mean=20)
set.seed(1121)  ; z = rnorm(n,mean=21)
dat = data.frame(loc=loc,y=y,z=z)

# create means of columns for country-wide daily averages
# it's 2:3 here because there are only two 'days' (y,z)
dat.mean = t(as.data.frame(colMeans(dat)[2:3]))

# collapse by year
dat.year = rowMeans(dat.mean)

# output file
write.csv(dat.year,'~/Desktop/file_name_yearly.csv')

############################
# MULTIYEAR DATA
############################

# create empty dataframe where summarised values will go
dat.years = data.frame(year=numeric(0),mean=numeric(0))

# number of years
x = 10

for(year in seq(1:x)){

    # create dummy data (where you would load 1 year's worth of data)
    # dat = load.csv(paste0('FILELOC',x,'.csv'))
    t = seq(1:100)
    y = rnorm(n,mean=20)
    z = rnorm(n,mean=21)
    dat = data.frame(t=t,y=y,z=z)

    # create means of columns for country-wide daily averages
    # it's 2:3 here because there are only two 'days' (y,z)
    dat.mean = t(as.data.frame(colMeans(dat)[2:3]))

    # collapse by year
    dat.year.value = as.numeric(rowMeans(dat.mean))
    dat.year = data.frame(year=year,mean=dat.year.value)

    # bind to set of values that have been previously up to this point
    dat.years = rbind(dat.years,dat.year)

}

# output file
write.csv(dat.years,'~/Desktop/file_name_multiyear.csv')