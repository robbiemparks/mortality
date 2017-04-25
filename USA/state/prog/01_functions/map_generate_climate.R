library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
age.print=age.print)

###############################################################
# PREPARING MAP
###############################################################

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
    )
}

# load shapefile
us <- readOGR(dsn="../../data/shapefiles",layer="states")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska,rotate=-50)
alaska <- elide(alaska,scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old versions of Alaska and Hawaii and put new ones back in
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]
us_aea <- rbind(us_aea, alaska, hawaii)

# fortify to prepare for ggplot
map <- fortify(us_aea)

# extract data from shapefile
shapefile.data <- us_aea@data

# add climate regions from Karl and Koss
shapefile.data$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
'West North Central','East North Central','Northwest','Northeast','East North Central',
'Northwest','Northeast','East North Central','Northeast','West North Central',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Central','West','Southwest','West','Central',
'Central','Northeast','Northeast','Central','Northeast',
'Southwest','Central','South','Southeast','Central',
'Southwest','South','Southeast','Central','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','East North Central','Northwest',
'West')

# reinsert shapefile.data with climate regions back into shapefile
us_aea@data <- shapefile.data

# extract superregions
for(i in unique(shapefile.data$climate_region)){
    temp <- us_aea[us_aea$climate_region==i,]
    assign(gsub(' ','_',i),temp)
}

# function to create borders around superregions
borders <- function(superregion) {
    lps <- getSpPPolygonsLabptSlots(superregion)
    IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
    dissolve   <- unionSpatialPolygons(superregion ,IDOneBin)
}

# combine all superregions
superregions <- rbind(  borders(Northwest),borders(West_North_Central),borders(East_North_Central),borders(Northeast),
borders(West),borders(Southwest),borders(South),borders(Central),borders(Southeast))

# fortify to prepare for ggplot
map.superregions <- fortify(superregions)

# find coordinates of centroids of superregions
superregion.coords <- as.data.frame(coordinates(superregions))
rownames(superregion.coords) <- 1:nrow(superregion.coords)
names(superregion.coords) <- c('long.txt','lat.txt')
superregion.coords$id <- c(1:9)
superregion.coords$region <- c('Northwest','West_North_Central','East_North_Central','Northeast',
'West','Southwest','South','Central','Southeast')

# create for every age
dat.super.temp <- data.frame()
for(j in c(1,2)) {
    for(i in c(0,5,15,25,35,45,55,65,75,85)) {
        dummy <- superregion.coords
        dummy$age <- i
        dummy$age.print <- age.code[age.code$age==i,2]
        dummy$sex <- j
        dat.super.temp <- rbind(dat.super.temp,dummy)
    }}
dat.super.temp.inv <- dat.super.temp

# plot superregions
#ggplot() +
#geom_polygon(data=map,aes(x=long,y=lat,group=group),fill='white',color='Black',size=1) +
#geom_polygon(data=subset(map.superregions),aes(x=long,y=lat,group=group),alpha=0,fill='Red',color='Red',size=1.2) +
#geom_text(data=superregion.coords,aes(x=long.txt,y=lat.txt,label=region))

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))
