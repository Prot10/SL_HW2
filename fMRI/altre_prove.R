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


#mat_corr[,117] <- train$age
#mat_corr[,118] <- train$sex
mat_corr <- matrix(nrow = 600, ncol = 116)
for (j in 1:600){
  print(j)
  mat_corr[j,1:116] <- apply(abs(cor(mod[[j]])), MARGIN = 2, function(x) {quantile(x,probs=0.1,na.rm=T)})
  #mat_corr[j,1:116] <- mat_corr[j,1:116]/sum(mat_corr[j,1:116])*1000
}
to_del <- unique(which(is.na(mat_corr), arr.ind = T)[,1])
mat_corr <- mat_corr[-to_del,]
#mat_corr <- mat_corr/colMeans(mat_corr)
# labels <- to_categorical(train$y[-to_del])

mat_nor <- mat_corr[train$y[-to_del]==0,]
mat_aut <- mat_corr[train$y[-to_del]==1,]
quantile(mat_aut,probs=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.999,1))
quantile(mat_nor,probs=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.999,1))

ind_nor <- which(mat_nor>80,arr.ind=T)
sort(table(ind_nor[,2]), decreasing = T)

unique_ind <- unique()

ind_aut <- which(mat_aut>80, arr.ind = T)
sort(table(ind_aut[,2]), decreasing = T)

hist(mat_nor[,25], freq=F)
hist(mat_aut[,25], add = T,freq = F)

quan_list <- rep(NA,116)

for (i in 1:116){
  quan_nor <- quantile(mat_nor[,i], probs = c(0.25,0.75))
  quan_aut <- quantile(mat_aut[,i], probs = c(0.25,0.75))
  quan_list[i] <- abs(quan_nor[1]-quan_aut[1]) + abs(quan_nor[2]-quan_aut[2])
}

primi_20 <- order(quan_list, decreasing = T)[1:20]
quan_list[primi_10]

### Ricerca migliori 10

numWorkers <- detectCores()  
cl <- makeCluster(numWorkers)
registerDoParallel(cl)

rf <- foreach(ntree=rep(200, numWorkers),
              .combine=randomForest::combine,
              .multicombine=TRUE, 
              .packages='randomForest') %dopar% {
                randomForest(mat_corr[1:400,],
                             factor(train$y[1:400], levels = c(0,1)), 
                             ntree = ntree)
              }

stopCluster(cl)

sum(abs(as.numeric(rf$y)-as.numeric(rf$predicted)))/400
sum(abs(as.numeric(predict(rf,mat_corr[401:584,primi_10]))-1-train$y[401:584]))/184

primi_10 <- order(rf$importance, decreasing = T)[1:20]

pred <- as.numeric(predict(rf,mat_corr_test[,primi_10]))-1

df <- data.frame(id = test$id, target = ifelse(pred==1,"autism","control"))
write.csv(df, file = "prova.csv",row.names = F)

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



