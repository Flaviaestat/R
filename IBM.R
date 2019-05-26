#Limpar área de trabalho ####
rm(list = ls()) #clean up environment
cat("\014")     #clean up console

#Pacotes necessários
#install.packages("e1071")
#install.packages("DMwR")
#install.packages("caret")
#install.packages('corrplot')
library(corrplot)
library(e1071)
library(DMwR)
library(caret)

#Semente para reprodução de resultados
set.seed(1234)

#Carregamento da Base ####
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 05 - Tratamento de Dados\\R")
data = read.csv("WA_Fn-UseC_-HR-Employee-Attrition_Missing.csv", na.strings=c("","NA"))

#### MISSING ####
source("ReplaceMissingPerClass.R")
data = ReplaceMissingPerClass(data, classIndex = 2)

#procura por atributos (colunas) com variância 0
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

#"métrica que avalia o nível de concordância de uma tarefa classificação"
kappa = confusionMatrix(cm)$overall[2]; kappa

#Balanceamento ####

table(data$Attrition) #proporção
str(data$Attrition)   #estrutura

#undersampling aleatório
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

#### Aplica PCA para seleção de atributos ####
#pca = preProcess(x = balancedData[-2], method = 'pca') #roda o pca na base. Desconsidero segunda coluna (a classe)
#pcaData = predict(pca, balancedData) #aplica transformação

pcaData = balancedData[-2]
pcaData = scale(pcaData)
pca <- princomp(pcaData) #roda o pca na base
summary(pca)  #resultados do pca (desvio padrão, variancia proporcional e proporção da variancia acumulada)

##graficos
plot(pca)

#Pego somente os componentes principais que mantêm uma variância acumulada de 100% do total
vars = pca$sdev^2
vars = vars/sum(vars)
cumulativeVariance = cumsum(vars)
View(as.data.frame(cumulativeVariance)) #==> atributo 30: redução de 5 atributos

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
View(concat) #probabilidades e previsões

#VOLTANDO A BASE BALANCEADA:  ANÁLISE DE CORRELAÇÃO

#MATRIZ DE CORRELAÇÃO #####
correlation = cor(balancedData[-2]) #td a base, excluindo a classe
corrplot(correlation, method = 'number', type = "upper", 
         tl.cex = 0.5, tl.col = 'black') # plotar matriz de correlação
#observe a matrix e veja que o atributo 'job level'
#está altamente correlacionado com 'Monthlty Income', o que, 
#inclusive, faz sentido.
#Um desses atributos é candidato a sair: vou excluir Job level

novaBase = balancedData[-14] #14 é o índice de Job Level

#SVM + missing + balanceamento + análise de correlação
system.time(svm_model <- svm(Attrition ~., novaBase, probability =T))
predictionsSVM <- predict(svm_model, novaBase, probability =T)
cm = table(predictionsSVM, novaBase$Attrition); cm
acuracy = 1 - mean(predictionsSVM != novaBase$Attrition)
acuracy

confusionMatrix(cm)$overall[2]


