#Limpar �rea de trabalho ####
rm(list = ls()) #clean up environment
cat("\014")     #clean up console

#Pacotes necess�rios
#install.packages("e1071")
#install.packages("DMwR")
#install.packages("caret")
#install.packages('corrplot')
library(corrplot)
library(e1071)
library(DMwR)
library(caret)

#Semente para reprodu��o de resultados
set.seed(1234)

#Carregamento da Base ####
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 05 - Tratamento de Dados\\R")
data = read.csv("WA_Fn-UseC_-HR-Employee-Attrition_Missing.csv", na.strings=c("","NA"))

#### MISSING ####
source("ReplaceMissingPerClass.R")
data = ReplaceMissingPerClass(data, classIndex = 2)

#procura por atributos (colunas) com vari�ncia 0
nearZeroVarianceIndexes = nearZeroVar(data)
#Over18, EmployeeCount e StandardHours
str(data)
summary(data)
data = data[,-nearZeroVarianceIndexes]

#svm + missing
system.time(svm_model <- svm(Attrition ~., data, probability =T))
predictionsSVM <- predict(svm_model, data, probability =T)
cm = table(predictionsSVM, data$Attrition); cm
acuracy = 1 - mean(predictionsSVM != data$Attrition)
acuracy

#"m�trica que avalia o n�vel de concord�ncia de uma tarefa classifica��o"
kappa = confusionMatrix(cm)$overall[2]; kappa

#Balanceamento ####

table(data$Attrition) #propor��o
str(data$Attrition)   #estrutura

#undersampling aleat�rio
#classeYes = which(data[,2] == "Yes") #yes
#classeNo = which(data[,"Attrition"] == "No") #no
#
#classeNo = sample(classeNo, size=400) # 600 registros escolhidos aleatoriamente
#indexes = c(classeYes, classeNo)
#balancedData = data[indexes,]

balancedData <- SMOTE(Attrition~., data, perc.over = 100)

table(balancedData$Attrition)

#svm missing + balanceamento #####
system.time(svm_model <- svm(Attrition ~., balancedData, probability =T))
predictionsSVM <- predict(svm_model, balancedData, probability =T)
cm = table(predictionsSVM, balancedData$Attrition); cm
acuracy = 1 - mean(predictionsSVM != balancedData$Attrition)
acuracy

confusionMatrix(cm)$overall[2]

#### Aplica PCA para sele��o de atributos ####
#pca = preProcess(x = balancedData[-2], method = 'pca') #roda o pca na base. Desconsidero segunda coluna (a classe)
#pcaData = predict(pca, balancedData) #aplica transforma��o

pcaData = balancedData[-2]
pcaData = scale(pcaData)
pca <- princomp(pcaData) #roda o pca na base
summary(pca)  #resultados do pca (desvio padr�o, variancia proporcional e propor��o da variancia acumulada)

##graficos
plot(pca)

#Pego somente os componentes principais que mant�m uma vari�ncia acumulada de 100% do total
vars = pca$sdev^2
vars = vars/sum(vars)
cumulativeVariance = cumsum(vars)
View(as.data.frame(cumulativeVariance)) #==> atributo 30: redu��o de 5 atributos

pcaData = pca$scores[,1:29]
pcaData = as.data.frame(pcaData)
pcaData = cbind(pcaData, balancedData$Attrition)
colnames(pcaData)[ncol(pcaData)] <- "Attrition"

#SVM + missing + balanceamento + pca
system.time(svm_model <- svm(Attrition ~., pcaData, probability =T))
predictionsSVM <- predict(svm_model, pcaData, probability =T)
cm = table(predictionsSVM, pcaData$Attrition); cm
acuracy = 1 - mean(predictionsSVM != pcaData$Attrition)
acuracy

confusionMatrix(cm)$overall[2]

probabilidades = attr(predictionsSVM, "probabilities")
concat = cbind(pcaData$Attrition, predictionsSVM, probabilidades)
View(concat) #probabilidades e previs�es

#VOLTANDO A BASE BALANCEADA:  AN�LISE DE CORRELA��O

#MATRIZ DE CORRELA��O #####
correlation = cor(balancedData[-2]) #td a base, excluindo a classe
corrplot(correlation, method = 'number', type = "upper", 
         tl.cex = 0.5, tl.col = 'black') # plotar matriz de correla��o
#observe a matrix e veja que o atributo 'job level'
#est� altamente correlacionado com 'Monthlty Income', o que, 
#inclusive, faz sentido.
#Um desses atributos � candidato a sair: vou excluir Job level

novaBase = balancedData[-14] #14 � o �ndice de Job Level

#SVM + missing + balanceamento + an�lise de correla��o
system.time(svm_model <- svm(Attrition ~., novaBase, probability =T))
predictionsSVM <- predict(svm_model, novaBase, probability =T)
cm = table(predictionsSVM, novaBase$Attrition); cm
acuracy = 1 - mean(predictionsSVM != novaBase$Attrition)
acuracy

confusionMatrix(cm)$overall[2]


