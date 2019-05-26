# K-Means Clustering

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 09 - 10 - Agrupamento\\Exercicios")
# Importing the dataset
dataset = read.csv('artificialData.csv', header = F)

library(ggplot2)
ggplot(data=dataset, aes(x=V1, y=V2)) + geom_point()

# Using the elbow method to find the optimal number of clusters
set.seed(1234)
wcss = vector()
for (i in 1:10) 
{
  wcss[i] = kmeans(x = dataset, centers = i)$tot.withinss #sum(kmeans(dataset, i)$withinss)
}
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
kmeans = kmeans(x = dataset, centers = 4)

clusters = kmeans$cluster
data = cbind(dataset,clusters) #associação dos dados aos seus clusters
data$clusters = as.factor(data$clusters) #conversão da coluna de clusters em fator (para plotar)

#plotar
ggplot(data = data, aes(x = V1, y = V2, colour = clusters))+
  geom_point()
#salvar
jpeg("clusters.jpeg")
ggplot(data = data, aes(x = V1, y = V2, colour = clusters))+
  geom_point()
dev.off()
  
  