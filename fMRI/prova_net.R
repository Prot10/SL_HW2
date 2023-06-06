#install.packages("neuralnet")
#library(mlbench)
#library(dplyr)
#library(magrittr)
install.packages("keras")
library(keras)
library(neuralnet)

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

mat_corr <- matrix(nrow = 600, ncol = 116*116 + 2)
mat_corr[,1:(116*116)] <- t(sapply(mod, function(x) (array_reshape(cor(x), dim = 116*116))))
mat_corr[,116*116 + 1] <- train$age
mat_corr[,116*116 + 2] <- train$sex

mat_corr <- matrix(nrow = 600, ncol = 6670)
#mat_corr[,117] <- train$age
#mat_corr[,118] <- train$sex
for (j in 1:600){
  mat_corr[j,] <- cor(mod[[j]])[lower.tri(cor(mod[[j]]))]
  #mat_corr[j,] <- ifelse(is.na(mat_corr[j,]), mean(mat_corr[j,]), mat_corr[j,])
  #mat_corr[j,1:116] <- mat_corr[j,1:116]/sum(mat_corr[j,1:116])*1000
}
to_del <- unique(which(is.na(mat_corr), arr.ind = T)[,1])
mat_corr <- mat_corr[-to_del,]
labels <- to_categorical(train$y[-to_del])

mat_nor <- mat_corr[train$y[-to_del]==0,]
mat_aut <- mat_corr[train$y[-to_del]==1,]

quan_list <- rep(NA,6670)

for (i in 1:6670){
  quan_nor <- quantile(abs(mat_nor[,i]), probs = c(0.25,0.75))
  quan_aut <- quantile(abs(mat_aut[,i]), probs = c(0.25,0.75))
  quan_list[i] <- abs(quan_nor[1]-quan_aut[1]) + abs(quan_nor[2]-quan_aut[2])
}

primi_20 <- order(quan_list, decreasing = T)[1:20]
quan_list[primi_20]


##########################################
start <- 5

for (i in 1:600){
  mod[[i]] <- matrix(nrow = 115, ncol = 116)
}
# ROIs matrices
for (i in 1:(N_ROIs)){
  sub_set <- train[, start:(start+ROI_length-1)]
  for (j in 1:600){ 
    mod[[j]][,i] <- as.numeric(sub_set[j,])
  }
  start <- start + ROI_length
}

# Partial correlations among ROIS -> medians along the correlations for each ROIS
mat_corr <- matrix(nrow = 600, ncol = 116)
for (j in 1:600){
  mat_corr[j,1:116] <- apply((pcor(mod[[j]])$estimate), MARGIN = 2, function(x) {quantile(x,probs=0.7,na.rm=T)})
}
#mat_corr[,117] <- train$age

labels <- to_categorical(train$y)

model <- keras_model_sequential(input_shape = c(116))
model %>%
  layer_dense(units = 10, activation = 'softmax') %>% 
  layer_dense(units = 5, activation = 'relu') %>% 
  #layer_dense(units = 10, activation = 'relu') %>% 
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.001),
  metrics = c('accuracy')
)

labels <- to_categorical(train$y)

history <- model %>% fit(
  mat_corr[1:400,], labels[1:400,], 
  epochs = 50, batch_size = 200,
  validation_split = 0.2
)

model %>% evaluate(mat_corr[401:600,], labels[401:600,])

sum(abs(as.numeric((model %>% predict(mat_corr[401:584,]))[,1] > 0.5 ) - labels[401:584,1]))/184

########### prova su test
#test <- read.csv("test_hw03.csv")

mod_test <- list()

start <- 4

for (i in 1:199){
  mod_test[[i]] <- matrix(nrow = 115, ncol = 116)
}

for (i in 1:(N_ROIs)){
  sub_set <- test[, start:(start+ROI_length-1)]
  for (j in 1:199){ 
    mod_test[[j]][,i] <- as.numeric(sub_set[j,])
  }
  start <- start + ROI_length
}

mat_corr_test <- matrix(nrow = 199, ncol = 6670)
#mat_corr[,117] <- train$age
#mat_corr[,118] <- train$sex
for (j in 1:199){
  gino <- cor(mod_test[[j]],)
  mat_corr_test[j,] <- gino[lower.tri(gino)]
  mat_corr_test[j,]<-ifelse(is.na(mat_corr_test[j,]), 0,mat_corr_test[j,])
  #mat_corr[j,1:116] <- mat_corr[j,1:116]/sum(mat_corr[j,1:116])*1000
}

to_del_test <- which(is.na(mat_corr_test), arr.ind = T)
mat_corr_test <- mat_corr_test[-to_del_test,]

pred <- as.numeric((model %>% predict(mat_corr_test[,primi_20]))[,1] > 0.5 )

df <- data.frame(id = test$id, target = ifelse(pred==1,"autism","control"))
df <- data.frame(id = test$id, target = rep("control",199))
write.csv(df, file = "prova.csv", row.names = FALSE)

