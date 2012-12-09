library(rgdal)
library(rpart)

set.seed(100)
Beta=1
shp <- readOGR(dsn="d:", layer="simulation")
data = shp@data
n = dim(data)[1]

train_index = sample(1:n, 0.8*n)
test_index = setdiff(1:n, train_index)

train_data = data[train_index,]
test_data = data[test_index,]

fit = rpart(class~x1+x2+x3, data=train_data,control=rpart.control(minsplit=10,cp=0.001))
plot(fit,compress=TRUE,margin=0.03,Uniform=FALSE)
text(fit,use.n=TRUE) 

pred = predict(fit,newdata=test_data,type="class")
confuse = table(pred,test_data$class)
sum(diag(confuse))/sum(confuse)







d_matrix = as.matrix(dist(data[,1:2],diag=T))
d_train_matrix = d_matrix[train_index,train_index]
trees = NULL
split_var = rep("c",dim(train_data)[1])
split_ = rep(0,dim(train_data)[1])

for (i in 1:dim(train_data)[1]){
w1 = 1/(d_train_matrix[i,]^Beta)
w1[i] = sum(w1[-i])/2
data1 = dupli_w(train_data,w1)
fit = rpart(class~x1+x2+x3, data=data1,control=rpart.control(minsplit=10,cp=0.01))
trees[[length(trees )+1]] <- fit
split_var[i] = as.character(fit$frame$var[1])
if (split_var[i]!="<leaf>") split_[i]=fit$splits[1,4]
}

#data2 = data.frame(train_data,split_var,split_)
#data_fit = SpatialPointsDataFrame(data2[,1:2] ,data2 ,proj4string=shp@proj4string)
#writeOGR(data_fit , "d:/", "fit22", driver="ESRI Shapefile")


result = rep("c",dim(test_data)[1])
d_cross_matrix = d_matrix[test_index,train_index]
d_cross_matrix = 1/(d_cross_matrix^Beta)


cls = levels(train_data$class)



for (j in 1:dim(test_data)[1]){
vote = rep(0,nlevels(train_data$class))
  for (i in 1:dim(train_data)[1]){
   
     pred = as.character(predict(trees[[i]],newdata=test_data[j,],type="class"))
    
    vote[which(cls==pred)] = vote[which(cls==pred)] + d_cross_matrix[j,i]
   }
result[j] = cls[which(vote==max(vote))]
}




#plot(trees[[1]],compress=TRUE,margin=0.03,Uniform=FALSE)
#text(trees[[1]],use.n=TRUE) 

confuse2 = table(result,test_data$class)
sum(diag(confuse2))/sum(confuse2)

