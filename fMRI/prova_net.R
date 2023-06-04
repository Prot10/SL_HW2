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

mat_corr <- matrix(nrow = 600, ncol = 117)
mat_corr[,117] <- train$age
#mat_corr[,118] <- train$sex
for (j in 1:600){
  mat_corr[j,1:116] <- apply(abs(cor(mod[[j]])), MARGIN = 1, function(x) {sum(x>0.2)-1})
  mat_corr[j,1:116] <- mat_corr[j,1:116]/sum(mat_corr[j,1:116])*1000
}
to_del <- unique(which(is.na(mat_corr), arr.ind = T)[,1])

mat_corr <- mat_corr[-to_del,]
labels <- to_categorical(train$y[-to_del])
data_corr <- as.data.frame(mat_corr)
data_corr$y <- train$y[-to_del]

mat_corr <- as.matrix(train)[,-c(1,4)]
labels <- to_categorical(train$y)


n <- neuralnet(y ~ .,
               data = data_corr[1:400,],
               hidden = c(4,2),
               threshold = 0.08,
               linear.output = F,
               lifesign = 'full',
               act.fct = "logistic",
               rep=3)

sum(abs(ifelse(predict(n, data_corr[401:584,])>0.5,1,0)-data_corr[401:584,117]))/(584-400)
sum(abs(ifelse(predict(n, data_corr[1:400,])>0.5,1,0)-data_corr[1:400,117]))/400
aaa

#as.matrix(data_d1[sd_mod[1:500]<2,-119])
#as.factor(train$y[1:500][sd_mod[1:500]<2])

model <- keras_model_sequential(input_shape = c(10))
model %>%
  layer_dense(units = 100, activation = 'relu') %>% 
  layer_dense(units = 50, activation = 'softmax') %>% 
  #layer_dense(units = 10, activation = 'relu') %>% 
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.001),
  metrics = c('accuracy')
)

#labels <- to_categorical(train$y[-to_del])

history <- model %>% fit(
  mat_corr[1:400,primi_10], labels[1:400,], 
  epochs = 100, batch_size = 200,
  validation_split = 0.2
)

model %>% evaluate(mat_corr[401:584,primi_10], labels[401:584,])

model %>% predict(mat_corr[401:584,]) 
