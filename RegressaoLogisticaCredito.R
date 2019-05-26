rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

#Carrega a base de dados ####
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 07 - Classificação")
database = read.table("credito.txt", header = TRUE)

# Gera aleatoriamente os indices para base de teste (30% para teste) ####
set.seed(0)
indexes = sample(1:nrow(database), size=0.3*nrow(database))
train = database[-indexes,]
test = database[indexes,]

#Normalização
notInputs = length(train )
train[-notInputs] = scale(train[-notInputs])
test[-notInputs] = scale(test[-notInputs])

#Regressao Logistica ####
classifier = glm(formula = CLASSE ~ .,
                 family = binomial,
                 data = train)

# Inferência ####
prob_pred = predict(classifier, type = 'response', newdata = test[-12])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

cm = table(test[, 12], y_pred > 0.5); cm
accuracy = 1 - mean(y_pred != test$CLASSE)
accuracy
