n= 500
library(sp)
library(rgdal)

set.seed(100)
#llCRS = CRS("+proj=longlat +ellps=WGS84")
llCRS = CRS("+proj=utm +zone=15 +datum=NAD83")


x_l = runif(n,min=4000, max=4500)
y_l = runif(n,min=4000, max=5000)
x_r = runif(n,min=4500, max=5000)
y_r = runif(n,min=4000, max=5000)


x1_l = x1_r = runif(n, min=0,max=10)
x2_l = x2_r = runif(n, min=5,max=6)
x3_l = x3_r = runif(n, min=-3,max=3)
class_l = class_r = rep('a',n)

class_l[which(x1_l>7)] = 'a'
class_l[which(x1_l<7)] = 'b'


class_r[which(x3_r>0)] = 'a'
class_r[which(x3_r<0)] = 'b'

data_l = data.frame(x_l, y_l,x1_l,x2_l,x3_l,class_l)
colnames(data_l) = c("x","y","x1","x2","x3","class")
data_sp_l = SpatialPointsDataFrame(data_l[,1:2],data_l,proj4string=llCRS)
writeOGR(data_sp_l, "d:/", "simulation_l", driver="ESRI Shapefile")


data_r = data.frame(x_r , y_r,x1_r,x2_r,x3_r,class_r)
colnames(data_r) = c("x","y","x1","x2","x3","class")
data_sp_r = SpatialPointsDataFrame(data_r[,1:2] ,data_r ,proj4string=llCRS)
writeOGR(data_sp_r , "d:/", "simulation_r", driver="ESRI Shapefile")



data = rbind(data_r,data_l)
data_sp = SpatialPointsDataFrame(data[,1:2] ,data ,proj4string=llCRS)
writeOGR(data_sp , "d:/", "simulation", driver="ESRI Shapefile")




if (FALSE) {library(rpart)
fit = rpart(class_l~x1_l+x2_l+x3_l, data=data_l,control=rpart.control(minsplit=2,maxdepth=3,cp=0.01))
plot(fit,compress=TRUE,margin=0.03,Uniform=FALSE)
text(fit,use.n=TRUE)


fit = rpart(class_r~x1_r+x2_r+x3_r, data=data_r,control=rpart.control(minsplit=2,maxdepth=3,cp=0.01))
plot(fit,compress=TRUE,margin=0.03,Uniform=FALSE)
text(fit,use.n=TRUE)
}
