library(rgdal)
library(rpart)

set.seed(100)
shp <- readOGR(dsn="d:", layer="data")
data = shp@data
n = dim(data)[1]

data = data[sample(1:n,1000,replace=F),]
n = dim(data)[1]

data$Type2 = as.factor(data$Type2)
data$Type1 = as.factor(data$Type1)

train_index = sample(1:n, 0.7*n)
test_index = setdiff(1:n, train_index)

#train_data = data[train_index,]
#test_data = data[test_index,]

train_data = SpatialPointsDataFrame(data[train_index,1:2],data[train_index,],proj4string=shp@proj4string)
test_data = SpatialPointsDataFrame(data[test_index,1:2],data[test_index,],proj4string=shp@proj4string)


fit = rpart(Type1~Wetness+Solar+Elevation+VegIndex, data=train_data,control=rpart.control(minsplit=10,cp=0.01))
plot(fit,compress=TRUE,margin=0.03,Uniform=FALSE)
text(fit,use.n=TRUE) 

pred = predict(fit,newdata=test_data@data,type="class")
confuse = table(pred,test_data$Type2)
sum(diag(confuse))/sum(confuse)






trees1 = gwRF(Type2~Wetness+Solar+Elevation+VegIndex, data=train_data, control=rpart.control(minsplit=10,cp=0.01))
result4 = gwRF.predict(trees1, test_data )



confuse2 = table(result4,test_data$Type2)
sum(diag(confuse2))/sum(confuse2)

