# 1. R data type and basic syntax
# 2. R and statistics
# 3. R package
# 4. R and GIScience (nearest neighbor, spatial weight/lag)


# Ctrl + R to run selected lines, Ctrl + L to clear the screen
a = 531evvv
b <- 4.3
x = 'LSU'


# "class" command returns data type
# "str" command returns data structure
# "summary" command returns some detailed(statistical/descriptive) information
class(a)
class(b)
class(x)
str(a)
summary(a)


# R data type: Array
# use c command to define array
a = c(1,2,3,6,7)
a
a[1]     # index starts from 1
a[-1]    # index can be negative

#other way to generate array
1:10
seq(from=0,to=1, by=0.01)
rep(x=3,time=10)
rep(c(3,4),time=10)
# ?function


#indexing in array, can be very handy if used right
a = c(8,40,9,6,7,39,11,31,75,92)
length(a)
a[1:4]  # a[c(1,2,3,4)], not a[1,2,3,4]
a[c(1,5,2)]

sort(a)     # sort the element in array a
order(a)    # return the order of element in a
order(a)[1:4]   # what is this??   
a[order(a)]     # what is this??
 



#nearest 4 neighbor
#suppose the array a stores distances from a spatial point to its neighbors??
a[order(a)[1:4]]     #the first 4 smallest number in a, ordered
a[sort(order(a)[1:4])]   # the first 4 smallest number in a, unordered(original)
a[order(a)[(-1):(-4)]] # what is this??
a[sort(order(a)[(-1):(-4)])]  # what is this??



#a tricky one
a
a[sort(order(a)[1:4])] = 1/a[sort(order(a)[1:4])] 
a[sort(order(a)[(-1):(-4)])] = 0
a 



#R data type: list, very useful for multiple returns from a function
data = list(1,c(4,2),'LSU',a)
class(data)
data
data[[1]]

data = list(name='Quan', school='LSU', age=28)
data
data[[1]]
data[['name']]




#R data type: factor (for handling categorical data)
b <- c("rain","not rain","not rain","not rain","rain")
class(b)
b = factor(c("rain","not rain","not rain","not rain","rain"))
class(b)
str(b)
nlevels(b)
levels(b)


#difference between landuse1(2) (1 as water, 2 as vegetation, 3 as impervious surface)
landuse1 = c(1,3,2,1,3,2,2,2,2,2,1,1,3,2,2,2)          #invalid model input
landuse2 = factor(c(1,3,2,1,3,2,2,2,2,2,1,1,3,2,2,2))  #valid model input




#R data type: matrix
m <- matrix(1:9, nrow=3, ncol=3)
class(m)
dim(m)
# matrix indexing
m[1,2]   # first row, second row
m[1,]    # first row, all columns
m[,1]    # all rows, first column
m[1:2, c(1,3)]  # how about this?
m[1:2, c(1,3)] = -99 # what just happened?



#R data type: data.frame
x = c(1,4,2,4,10,33,4)
y = c(2,3,4,5,1,0,1)
dat = data.frame(x,y)
names(dat)  # return the column name
class(dat)
dat$x       
dat = read.csv(file='F:/Doctoral/Wang/trend-profile-threshold/trend.csv', header=T)
names(dat)
dat[1:5,]  # the first 5 rows of the data


#r build-in data frame: mtcars
mtcars
class(mtcars)
names(mtcars)
dim(mtcars)
#all kinds of indexing
mtcars[,1:2]
mtcars$mpg
mtcars[,c('mpg','wt')]


#define a r function
summation <- function (n) {
sum = 0
for (i in 1:n){  # <- this is a for loop
sum = sum + i
}
sum   # function return
}

#call to the function
cal <- summation(10)
cal

# &, |, !,  while loop, for loop, if...else...










#2. R and statistics
set.seed(100)    #make your work reproducible

# R generates random variables from given distributions
runif(10)       #uniform distribution
rnorm(n=10)     #standard normal distribution
rnorm(n=10,mean=10,sd=1)

# sample command draws samples from a given dataset
sample(1:10,size=10,replace=F)

a = c(32,45,1,343,5,98,675,3,-1:-10)
index <- sample(1:length(a),size=8,replace=F)
index
a[index]

#statistic analysis and plots
fit <- lm(mpg~wt,data=mtcars)
summary(fit)
plot(mtcars$mpg, mtcars$wt, main = "data", xlab="Mile per gallon", ylab="Weight", pch=19)



# R produces high-quality plots
bmp(file="d:/f1.bmp",res=600,width=2500,height=3600)
par(mfrow=c(1,1),mar=c(4,4,4,1),las =1, pch=1,font=6,font.lab=6,cex=1,mgp = c(2,0.5,0))
plot(mtcars$mpg, mtcars$wt, main = "data", xlab="Mile per gallon", ylab="Weight", pch=19)
dev.off()

#another example, source command executes a given r scripts
source("F:/Doctoral/chapter6/r-code/r_plot.R")

#others R commands for statistical analysis
#t.test, anova, cor
#lm (linear model)
#qda(quadratic discriminant analysis)
#lda (linear discriminant anaysis)
#princomp (pricipal component analysis)
#factanal(factor analysis)
#kmeans (k-means clustering)
#hclust (cluster analysis)
#...






#R packages
install.packages("rpart")
library(rpart)

install.packages("Rcmdr")  #SPSS-like GUI
library(Rcmdr)

install.packages("rattle")  #R for data mining
library(rattle)
rattle()


#some packages for GIS
#sp:    class library for storing spatial data 
getClass('Spatial')
getClass('SpatialPoints')
getClass('SpatialPointsDataFrame')
getClass('CRS')


#gstat: idw, variogram and kriging
#spdep: R-version of GeoDa. spatial regression/autocorrelation/weight
#rgdal: reading vector and raster files
#spgwr: geographically weighted regression
#...





#R and GIScience
#1. nearest neighbor matirx and spatial lag
x = c(1,4,2,4,10,33,4)
y = c(2,3,4,5,1,0,1)
dat = data.frame(x,y)
 

# dist commands returns point distance
d = dist(dat, diag=T, upper=T)
m = as.matrix(d)
diag(m) <- 10000000



d = m[1,]
n=3
d[sort(order(d)[1:n])] = 1/d[sort(order(d)[1:n])]
d[sort(order(d)[(-1):(-n)])] = 0 
d = d/sum(d)


# apply function in r,
# Returns a vector obtained by applying a function to margins of an matrix

IDW <-function (d){
power = -1 
d = d^power
d = d/sum(d)  #row standardization
d  
}

NN3 <-function (d){
n = 3  #nearest three neighbors
d[sort(order(d)[1:n])] = 1/d[sort(order(d)[1:n])]
d[sort(order(d)[(-1):(-n)])] = 0 
d = d/sum(d)  #row standardization
d  
}

# MARGIN = 1 indicates "by row", MARGIN = 2 indicates "by column"
W <- t(apply(m, MARGIN=1,FUN=IDW))  #spatial weight matrix W for IDW
W <- t(apply(m, MARGIN=1,FUN=NN3))  #spatial weight matrix W for nearest 3 neighbors





# use R to read a shapefile
library(rgdal)
shp <- readOGR(dsn="e:", layer="bhrain97")
str(shp)
class(shp)
attr_data = shp@data
loc = shp@coords   #coordinates(shp)
extent = shp@bbox
llCRS = shp@proj4string
plot(shp)




d = dist(loc , diag=T, upper=T)
m = as.matrix(d)
dim(m)
m[1:5,1:5]
diag(m) <-100000000
m[1:5,1:5]
W <- t(apply(m, MARGIN=1,FUN=IDW))

names(attr_data)
SUM = attr_data$SUM    
SUM_LAG= W %*% SUM  #spatial lag  = W*y, spatial weighted average of y
cor(SUM, SUM_LAG)


output = data.frame(attr_data,SUM_LAG)
data_sp = SpatialPointsDataFrame(loc, output, proj4string=llCRS)
writeOGR(data_sp , "d:/", "SW", driver="ESRI Shapefile")





data=readGDAL("F:/landuse/vis_lst.img")
str(data)
summary(data)
dim(data@data)

band1 = matrix(data@data$band4,nrow=1342,ncol=987)
band1<-band1[,c(987:1)]  #reverse the order of row
#install.packages("fields")
library(fields)
image.plot(band1,main="", col=gray((0:32)/32),axes=FALSE,xlab="Temperature")




