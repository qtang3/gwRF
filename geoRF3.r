library(rgdal)
library(rpart)

set.seed(100)
shp <- readOGR(dsn="d:", layer="simulation")
data = shp@data
n = dim(data)[1]

train_index = sample(1:n, 0.8*n)
test_index = setdiff(1:n, train_index)

#train_data = data[train_index,]
#test_data = data[test_index,]

train_data = SpatialPointsDataFrame(data[train_index,1:2],data[train_index,],proj4string=shp@proj4string)
test_data = SpatialPointsDataFrame(data[test_index,1:2],data[test_index,],proj4string=shp@proj4string)


fit = rpart(class~x1+x2+x3, data=train_data,control=rpart.control(minsplit=10,cp=0.01))
plot(fit,compress=TRUE,margin=0.03,Uniform=FALSE)
text(fit,use.n=TRUE) 

pred = predict(fit,newdata=test_data,type="class")
confuse = table(pred,test_data$class)
sum(diag(confuse))/sum(confuse)






trees1 = gwRF(class~x1+x2+x3, data=train_data, control=rpart.control(minsplit=10,cp=0.01))
result4 = gwRF.predict(trees1, test_data )



confuse2 = table(result4,test_data$class)
sum(diag(confuse2))/sum(confuse2)

