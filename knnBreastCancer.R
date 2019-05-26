#https://www.kaggle.com/uciml/breast-cancer-wisconsin-data
# Carrega pacotes ####
library(class)

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

#Carrega a base de dados ####
setwd("C:\\Users\\Flávia\\Google Drive\\00 PUC BI MASTER\\02 - Data Mining")
database = read.csv("breastCancer.csv", header = TRUE)

# Gera aleatoriamente os indices para base de teste (30% para teste) ####
set.seed(0)
indexes = sample(1:nrow(database), size=0.3*nrow(database))
train = database[-indexes,]
test = database[indexes,]

#Normalização
notInputs = 1:2
train[-notInputs] = scale(train[-notInputs])
test[-notInputs] = scale(test[-notInputs])

#KNN ####
system.time(knn_model <- knn(train[,-notInputs], 
                             test[,-notInputs], 
                             cl=train$diagnosis, k = 5))

#o teste já foi passado no treinamento. 
#a resposta do modelo já são as previsões
table(knn_model, test$diagnosis)
accuracy = 1 - mean(knn_model != test$diagnosis)
accuracy

