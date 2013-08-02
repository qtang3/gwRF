dupli_w <- function (data, w)
 {
     w <- round(100*w/sum(w))
     r <- data[1,]
     for (i in 1:dim(data)[1]) {
         j <- 0
         while (j < w[i]) {
             r <- rbind(r, c(data[i,]))
             j <- j+1
         }
     }
     z <- r[-1,]
     row.names(z) <- 1:dim(z)[1]
     z
}



gwRF <-function (formula, data, coords, control, Beta = 1)
{

#data <- SpatialPointsDataFrame(train_data[,1:2],train_data,proj4string=shp@proj4string)
#formula=class~x1+x2+x3
#control=rpart.control(minsplit=10,cp=0.01)

   if (is(data, "Spatial")) {
        if (!missing(coords)) 
            warning("data is Spatial* object, ignoring coords argument")
        coords <- coordinates(data)
   }
   d_matrix <- as.matrix(dist(coords, diag = T))
   n <- dim(data)[1]
   rawdata <- as(data, "data.frame")           # data <- data@data
   trees <- NULL
   for (i in 1:n) {
       w <- 1/(d_matrix[i,]^Beta)
       w[i] <- sum(w[-i])/2
       data1 <- dupli_w(rawdata, w)
       trees[[length(trees)+1]] <- rpart(formula, data = data1,control = control)
   }
   z <- list(forest=trees,train.data=data)
   class(z) <- "gwRF"
   z
}



gwRF.predict <- function(object, testdata, coords, Beta = 1)
{

#object=trees1
#testdata=test_data
#loc=test_data[,1:2]
   if (is(testdata, "Spatial")) {
        if (!missing(coords)) 
            warning("data is Spatial* object, ignoring coords argument")
        coords <- coordinates(testdata)
   }
   n1 <- dim(coordinates(object$train.data))[1]
   n2 <- dim(testdata)[1]
   coords <- rbind(coordinates(object$train.data),coords)

   result <- rep("NA",n2)
   d_matrix <- as.matrix(dist(coords,diag=T))
   d_cross_matrix <- 1/(d_matrix[(n1+1):(n1+n2),1:n1]^Beta)
   
   ylevels = attr(object$forest[[1]],"ylevels")

   for (j in 1:n2){
        vote = rep(0, length(ylevels))
        for (i in 1:n1){
             pred = as.character(predict(object$forest[[i]], newdata = testdata[j,], type = "class"))
             vote[which(ylevels == pred)] = vote[which(ylevels == pred)] + d_cross_matrix[j,i]
        }
        result[j] = ylevels[which(vote==max(vote))]
   }
   result
}
