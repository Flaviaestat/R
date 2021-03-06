#Limpar �rea de trabalho ####
rm(list = ls()) #clean up environment
cat("\014")     #clean up console

#Pacotes necess�rios#
install.packages("e1071")
install.packages("DMwR")
install.packages("caret")
library(e1071)
library(DMwR)
library(caret)

#Semente para reprodu��o de resultados
set.seed(1234)

#Carregamento da Base ####
setwd("C:\\Users\\Fl�via\\Google Drive\\00 PUC BI MASTER")
data = read.csv("Secom.csv", header = F)

#### MISSING ####
#observe a linha 22 da primeira coluna: ela possui um NA
#execute a linha abaixo para saber qual a m�dia para a primeira coluna
summary(data$V1) #m�dia = 3014
#execute a substitui��o (funcao abaixo - e verifique se o valor foi substituido corretamente)

setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 05 - Tratamento de Dados\\R")
source("ReplaceMissingPerClass.R")
data = ReplaceMissingPerClass(data, classIndex = 591)

#trasnforma em fator a classe
data$V591 = as.factor(data$V591)

#procura por atributos (colunas) com vari�ncia 0
nearZeroVarianceIndexes = nearZeroVar(data)
data = data[,-nearZeroVarianceIndexes]

#svm + missing
system.time(svm_model <- svm(V591~., data, probability =T))
predictionsSVM <- predict(svm_model, data, probability =T)
cm = table(predictionsSVM, data$V591); cm
accuracy = 1 - mean(predictionsSVM != data$V591)
accuracy

#"m�trica que avalia o n�vel de concord�ncia de uma tarefa classifica��o"
confusionMatrix(cm)$overall[2]

#Balanceamento ####
table(data[,length(data)])

#undersampling aleat�rio
classe0 = which(data[,length(data)] == -1)
classe1 = which(data[,length(data)] == 1)

classe0 = sample(classe0, size=600) # 600 registros escolhidos aleatoriamente
indexes = c(classe0, classe1)
balancedData = data[indexes,]

table(balancedData[,length(balancedData)])

#svm missing + balanceamento #####
system.time(svm_model <- svm(V591 ~., balancedData, probability =T))
predictionsSVM <- predict(svm_model, balancedData, probability =T)
cm = table(predictionsSVM, balancedData$V591); cm
acuracy = 1 - mean(predictionsSVM != balancedData$V591)
acuracy

confusionMatrix(cm)$overall[2]

#### Aplica PCA para sele��o de atributos ####
classIndex = length(balancedData)
pcaData = balancedData[-classIndex]
pca <- princomp(pcaData) #roda o pca na base

##graficos
plot(pca)

#Pego somente os componentes principais que mant�m uma vari�ncia acumulada de 100% do total
vars = pca$sdev^2
vars = vars/sum(vars)
cumulativeVariance = cumsum(vars)
View(as.data.frame(cumulativeVariance)) #==> atributo 190: redu��o de 400 atributos

cut = 177
pcaData = pca$scores[,1:cut]
pcaData = as.data.frame(pcaData)
pcaData = cbind(pcaData, balancedData[, classIndex]) 

classIndex = cut+1
colnames(pcaData)[classIndex] = c("V591") #renomeia a coluna que representa classe

#SVM
system.time(svm_model <- svm(V591 ~., pcaData, probability =T))
predictionsSVM <- predict(svm_model, pcaData, probability =T)
cm = table(predictionsSVM, pcaData$V591); cm
acuracy = 1 - mean(predictionsSVM != pcaData$V591)
acuracy

confusionMatrix(cm)$overall[2]

probabilidades = attr(predictionsSVM, "probabilities")
concat = cbind(pcaData$V591, predictionsSVM, probabilidades)
View(concat) #probabilidades e previs�es

#ATEN��O!!!!
#Aqui estamos avaliando com a base de treino.
#N�o devemos esquecer que para validar um modelo, devemos avali�-lo
#com uma base nunca vista por ele.
#Assim, avaliamos a capacidade de generaliza��o do modelo.


setwd("c:\\")