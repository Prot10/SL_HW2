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

mat_corr <- matrix(nrow = 600, ncol = 116)
#mat_corr[,117] <- train$age
#mat_corr[,118] <- train$sex
for (j in 1:600){
  mat_corr[j,1:116] <- apply(abs(cor(mod[[j]])), MARGIN = 2, function(x) {sum(x<0.5)})
  #mat_corr[j,1:116] <- mat_corr[j,1:116]/sum(mat_corr[j,1:116])*100
}
to_del <- unique(which(is.na(mat_corr), arr.ind = T)[,1])

mat_corr <- mat_corr[-to_del,]
labels <- to_categorical(train$y[-to_del])

mat_nor <- mat_corr[train$y[-to_del]==0,]
mat_aut <- mat_corr[train$y[-to_del]==1,]

hist(mat_aut[,9],freq=F)
hist(mat_nor[,9], add = T,freq = F)

quan_list <- rep(NA,116)

for (i in 1:116){
  quan_nor <- quantile(mat_nor[,i], probs = c(0.2,0.8))
  quan_aut <- quantile(mat_aut[,i], probs = c(0.2,0.8))
  quan_list[i] <- abs(quan_nor[1]-quan_aut[1]) + abs(quan_nor[2]-quan_aut[2])
}

primi_10 <- order(quan_list, decreasing = T)[1:10]
quan_list[primi_10]

### Ricerca migliori 10

numWorkers <- detectCores()  
cl <- makeCluster(numWorkers)
registerDoParallel(cl)

rf <- foreach(ntree=rep(2000, numWorkers),
              .combine=randomForest::combine,
              .multicombine=TRUE, 
              .packages='randomForest') %dopar% {
                randomForest(mat_corr[1:400,primi_10],
                             factor(train$y[-to_del][1:400], levels = c(0,1)), 
                             ntree = ntree)
              }

stopCluster(cl)

sum(abs(as.numeric(rf$y)-as.numeric(rf$predicted)))/400
sum(abs(as.numeric(predict(rf,mat_corr[401:584,primi_10]))-1-train$y[-to_del][401:584]))/184

primi_10 <- order(rf$importance, decreasing = T)[1:10]

### Calcola model meno j in primi_10

mod_for <- list()

for (i in 1:118){
  print(i)
  mod_for[[i]] <- glmnet(as.matrix(data_d1[,-c(i,119)]),
                         as.factor(train$y[-to_del][1:400]),
                         alpha = 1,
                         lambda = lam,
                         family = "binomial")
}

b <- list()
b_median <- rep(NA,118)

for (f in 1:118){
  print(f)
  b[[f]] <- boot(data_d2,
                 statistic = fun_dentro,
                 R = 400,
                 feat = f,
                 model_true = model_true,
                 model_j = mod_for[[f]], 
                 parallel = "multicore")
  b_median[f] <- median(b[[f]]$t)
  print(b_median[f])
}

ind_paper = b_median>1
sum(ind_paper)


model_only_ind <- cv.glmnet(as.matrix(data_d1[,ind_paper]), 
                            as.factor(train$y[-to_del][1:400]),
                            alpha = 1,
                            family = "binomial")

lam <- model_only_ind$lambda.min
#model_true <- glmnet(y ~ ., data_d1, alpha = 1, lambda = 0.1, family = "binomial")
sum(abs((as.numeric(predict(model_only_ind, as.matrix(data_d1[ind_paper]), type = "class"))-train$y[-to_del][1:400])))



