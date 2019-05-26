setwd("C:\\Users\\Flávia\\Google Drive\\00 PUC BI MASTER\\02 - Data Mining")

# Apriori
# install.packages('arules')
library(arules)

# Base original
originalDataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

#Matriz Esparsa
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', 
                            rm.duplicates = TRUE) #remove entradas duplicadas em cada transação
summary(dataset) #Detalhes da matriz
itemFrequencyPlot(dataset, topN = 10) #Top 10 products

#Treinamento
rules = apriori(data = dataset, parameter = list(support = 0.003, confidence = 0.2))

#SUPPORT
#otimizar venda de produtos comprados pelo menos 3 vezes ao dia; Por semana: 3*7;
#Support (mínimo): 3*7/7501 ~ 0.003
#CONFIDENCE
#Baixa: regras que não fazem sentido; Alta: regras óbvias;
#Comece com o valor default (0.8) e vá diminuindo (tentativa e erro).
#Atenção: um valor de 0.8 é muito alto e não gerará nenhuma regra nesse caso. 
#Um valor de 0.8 significa que todas as regras geradas devem estar corretas em 80% das transações.

# Visualizar resultados
inspect(sort(rules, by = 'lift')[1:10])

