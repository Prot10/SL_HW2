mat_corr <- matrix(NA,600,6670)

for (j in 1:600){
  mat <- cor(mod[[j]])
  mat_corr[j,] <- mat[lower.tri(mat)]
  #mat_corr[j,1:116] <- mat_corr[j,1:116]/sum(mat_corr[j,1:116])*100
}

to_del <- unique(which(is.na(mat_corr), arr.ind = T)[,1])
mat_corr <- mat_corr[-to_del,]

mat_corr <- cbind(mat_corr,train$sex[-to_del],train$age[-to_del])


numWorkers <- detectCores()  
cl <- makeCluster(numWorkers)
registerDoParallel(cl)

rf <- foreach(ntree=rep(1000, numWorkers),
              .combine=randomForest::combine,
              .multicombine=TRUE, 
              .packages='randomForest') %dopar% {
                randomForest(mat_corr[1:400,],
                             factor(train$y[-to_del][1:400], levels = c(0,1)), 
                             ntree = ntree)
              }

stopCluster(cl)

rf <- randomForest(mat_corr[1:400,primi],
                   factor(train$y[-to_del][1:400], levels = c(0,1)), 
                   ntree = 5000)

#primi <- order(rf$importance, decreasing = T)[1:10]

sum(abs(train$y[-to_del][1:400]-as.numeric(predict(rf,mat_corr[1:400,primi]))+1))/400

sum(abs((as.numeric(predict(rf,mat_corr[401:584,primi]))-1)-train$y[-to_del][401:584]))/184
