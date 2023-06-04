# Installa il pacchetto neuralnet se non è già installato
# install.packages("neuralnet")

# Carica il pacchetto neuralnet
library(neuralnet)

# Preparazione dei dati (esempio)
# X contiene le caratteristiche di input
# y contiene le etichette di classe
X <- iris[, 1:4]
y <- iris[, 5]

# Dividi i dati in set di addestramento e test
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Definizione dell'architettura della rete neurale
# In questo esempio, utilizziamo una rete con un singolo strato nascosto
# di 5 nodi e la funzione di attivazione sigmoid
net <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                 data = train_data,
                 hidden = c(5), # Numero di nodi nello strato nascosto
                 act.fct = "logistic")

trained_net <- train(net)

predictions <- compute(trained_net, test_data[, 1:4])$net.result
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
actual_classes <- test_data$Species

accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
print(paste("Accuracy:", accuracy))





# NN tensorflow -----------------------------------------------------------

install.packages("tensorflow")

library(reticulate)
path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)

library(tensorflow)
install_tensorflow(envname = "r-reticulate")

install.packages("keras")
library(keras)
install_keras(envname = "r-reticulate")

library(tensorflow)
tf$constant("Hello Tensorflow!")


library(keras)
#install.packages("mlbench")
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

data("BostonHousing")
data <- BostonHousing
str(data)

data %<>% mutate_if(is.factor, as.numeric)

n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
               data = data,
               hidden = c(12,7),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

data <- as.matrix(data)
dimnames(data) <- NULL

set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1,1:13]
test <- data[ind==2, 1:13]
trainingtarget <- data[ind==1, 14]
testtarget <- data[ind==2, 14]
str(trainingtarget)
str(testtarget)

m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 1)