#### Prova correlazioni----

mod <- list()

start <- 5

for (i in 1:600){
  mod[[i]] <- matrix(nrow = 115, ncol = 116)
}

for (i in 1:(N_ROIs)){
  sub_set <- train[, start:(start+ROI_length-1)]
  for (j in 1:600){ 
    mod[[j]][,i] <- as.numeric(sub_set[j,])
  }
  start <- start + ROI_length
}

mat_corr <- matrix(nrow = 432, ncol = 118)
s <- 1
for (j in mod[sd_mod<3]){
  mat_corr[s,1:116] <- apply(abs(cor(j)), MARGIN = 1, function(x) {sum(x>0.3)-1})
  s <- s+1
}

mat_corr[,117] <- train$sex[sd_mod<3]
mat_corr[,118] <- train$age[sd_mod<3]

to_del <- unique(which(is.na(mat_corr), arr.ind = T)[,1])

mat_corr <- mat_corr[-to_del,]

numWorkers <- detectCores()  
cl <- makeCluster(numWorkers)
registerDoParallel(cl)

rf <- foreach(ntree=rep(20000, numWorkers),
              .combine=randomForest::combine,
              .multicombine=TRUE, 
              .packages='randomForest') %dopar% {
                randomForest(mat_corr[,primi_10],
                             factor(train$y[sd_mod<3][-to_del], levels = c(0,1)), 
                             ntree = ntree)
              }

stopCluster(cl)

primi_10 <- order(rf$importance, decreasing = T)[1:10]

mat_corr <- cbind(mat_corr,train$y[-c(to_del)])

data_corr <- as.data.frame(mat_corr)
data_corr$V119
