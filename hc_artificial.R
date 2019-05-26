# Hierarchical Clustering

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 09 - 10 - Agrupamento\\Exercicios")
# Importing the dataset
dataset = read.csv('artificialData.csv', header=F)

library(ggplot2)
ggplot(data=dataset, aes(x=V1, y=V2)) + geom_point()

# Usando o dendrograma para definir o numero otimo de clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'))
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Rodando a clusterizção hierarquica
hc = hclust(d = dist(dataset, method = 'euclidean')) 
y_hc = cutree(hc, 4) #numero de clusters
y_hc

data = cbind(dataset,y_hc) #associação dos dados aos seus clusters
data$y_hc = as.factor(data$y_hc) #conversão da coluna de clusters em fator (para plotar)

#plotar
ggplot(data = data, aes(x = V1, y = V2, colour = y_hc))+
  geom_point()

#salvar na pasta a figura
jpeg("clustersHC.jpeg")
ggplot(data = data, aes(x = V1, y = V2, colour = y_hc))+
  geom_point()
dev.off()
