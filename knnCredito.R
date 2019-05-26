# Carrega pacotes ####
library(class)

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

#Carrega a base de dados ####
setwd("C:\\Users\\Flávia\\Google Drive\\00 PUC BI MASTER\\02 - Data Mining")
database = read.table("credito.txt", header = TRUE)

# Gera aleatoriamente os indices para base de teste (30% para teste) ####
set.seed(0)
indexes = sample(1:nrow(database), size=0.3*nrow(database))
train = database[-indexes,]
test = database[indexes,]

#Normalização

notInputs = which(colnames(database)=="CLASSE") #posicao da coluna que nao deve ser normalizada 

train[-c(notInputs)] = scale(train[-c(notInputs)])
test[-c(notInputs)] = scale(test[-c(notInputs)])

#KNN ####
knn_model <- knn(train[-c(notInputs)], 
                 test[-c(notInputs)], 
                             cl=train$CLASSE, k = 5)

summary(knn_model)

#o teste já foi passado no treinamento. 
#a resposta do modelo já são as previsões
table(knn_model, test$CLASSE)
accuracy = 1 - mean(knn_model != test$CLASSE)
accuracy

