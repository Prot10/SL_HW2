library(glmnet)
library(boot)
library(randomForest)

train <- read.csv("train_hw03.csv")
#test  <- read.csv("test_hw03.csv")

#train$y = ifelse(train$y=="autism", 1, 0)
#train$sex = ifelse(train$sex=="male", 1, 0)

#test$sex = ifelse(test$sex=="male", 1, 0)
N_ROIs <- 116
ROI_length <- 115

train_partial <- data.frame(matrix(ncol=N_ROIs, nrow=nrow(train)))
# col_names <- c(paste0("mean_ROI_", 1:N_ROIs), paste0("sd_ROI_", 1:N_ROIs))
col_names <- c(paste0("mean_ROI_", 1:N_ROIs))
colnames(train_partial) <- col_names

start <- 5

fun_apply <- function(x){
  return(median(scale(x)))
}

for (i in 1:(N_ROIs)){
  sub_set <- train[, start:(start+ROI_length-1)]
  ROIs_mean <- apply(sub_set, MARGIN=1, FUN=fun_apply)
  #ROIs_sd <- apply(sub_set, MARGIN=1, FUN=sd)
  train_partial[, i] <- ROIs_mean
  #train_partial[, i+N_ROIs] <- ROIs_sd
  start <- start + ROI_length
}

####

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

apply(abs(cor(mod[[j]])), MARGIN = 1, function(x) {sum(x>0.5)})

sum(abs(cor(mod[[j]]))[1,]>0.5)

train_partial$sex <- train$sex
train_partial$age <- train$age

to_del <- unique(which(is.na(train_partial), arr.ind = T)[,1])
train_partial <- train_partial[-c(to_del),]

##############################  

# prova glm:
# matrice con i dati relativi a D_1, cioè pazienti nel train
data_d1 <- data.frame(train_partial[1:400,])
data_d2 <- data.frame(train_partial[401:(600-16),])
data_d1$y <- train$y[-to_del][1:400]
data_d2$y <- train$y[-to_del][401:(600-16)]

model_true <- cv.glmnet(as.matrix(data_d1), 
                        train$y[-to_del][1:400],
                        alpha = 1,
                        #lambda = 0.1, 
                        family = "binomial")
lam <- model_true$lambda.min
#model_true <- glmnet(y ~ ., data_d1, alpha = 1, lambda = 0.1, family = "binomial")
sum(abs((as.numeric(predict(model_true, as.matrix(data_d1),s = lam, type = "class"))-train$y[-to_del][1:400])))

# per ogni jth devo calcolare il model su D_1 con e senza j th

fun_dentro <- function(data, ind, feat, model_true, model_j){
  aieie <- data_d2[ind,]
  true <- predict(model_true, 
                  as.matrix(aieie),
                  type = "class")
  meno_j <- predict(model_j, 
                    as.matrix(aieie[-feat]), 
                    type = "class")
  return(sum(abs(as.numeric(as.matrix(aieie[1]))-as.numeric(meno_j)))-
           sum(abs(as.numeric(as.matrix(aieie[1]))-as.numeric(true))))
}  

### Random Forest

forest <- randomForest(data_d1[,-119], factor(data_d1$y, levels = c(0,1)))
primi_10 <- order(forest$importance, decreasing = T)[1:10]

mod <- list()

for (i in 1:118){
  print(i)
  mod[[i]] <- glmnet(as.matrix(data_d2[-i]), 
                     train$y[301:600],
                     alpha = 1,
                     lambda = 0.1,
                     family = "binomial")
}

b <- list()

for (f in 1:118){
  print(f)
  b[[f]] <- boot(data_d2,
                 statistic = fun_dentro,
                 R = 200,
                 feat = f,
                 model_true = model_true,
                 model_j = mod[[f]], 
                 parallel = "multicore")
}

b_median <- rep(NA,10)

for (i in 1:10){
  b_median[i] <- median(b[[i]]$t)
}

for (i in t(mat_strange[,1:116])){
  print(mean(i))
}
