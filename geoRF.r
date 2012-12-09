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

fit = rpart(class~x1+x2+x3, data=train_data,control=rpart.control(minsplit=10,cp=0.01))
#plot(fit,compress=TRUE,margin=0.03,Uniform=FALSE)
#text(fit,use.n=TRUE) 

pred = predict(fit,newdata=test_data,type="class")
confuse = table(pred,test_data$class)
sum(diag(confuse))/sum(confuse)





d_matrix = as.matrix(dist(data[,1:2],diag=T))
d_train_matrix = d_matrix[train_index,train_index]
trees = NULL

for (i in 1:dim(train_data)[1]){
w1 = 1/(d_train_matrix[i,]^Beta)
w1[i] = sum(w1[-i])/2
data1 = dupli_w(train_data,w1)
fit = rpart(class~x1+x2+x3, data=data1,control=rpart.control(minsplit=10,cp=0.01))
trees[[length(trees )+1]] <- fit
}

result2 = rep("c",dim(test_data)[1])
d_cross_matrix = d_matrix[test_index,train_index]
d_cross_matrix = 1/(d_cross_matrix^Beta)
for (j in 1:dim(test_data)[1]){
s= 0
  for (i in 1:dim(train_data)[1]){
   
     pred = predict(trees[[i]],newdata=test_data[j,],type="class")
     if (as.character(pred) == "a") {
       s = s + d_cross_matrix[j,i]
   }
    if (as.character(pred) == "b") {
       s = s - d_cross_matrix[j,i]

   }


}
print(s)
if (s>0) result2[j] = "a"
if (s<0) result2[j] = "b"

}

confuse3 = table(result2,test_data$class)
sum(diag(confuse3))/sum(confuse3)


plot(trees[[33]],compress=TRUE,margin=0.03,Uniform=FALSE)
text(trees[[33]],use.n=TRUE)
