# K-Means Clustering

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 09\\Exercicios")
# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Using the elbow method to find the optimal number of clusters
set.seed(1234)
wcss = vector()
for (i in 1:10) 
{
  wcss[i] = kmeans(dataset, i)$tot.withinss
}
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
kmeans = kmeans(x = dataset, centers = 5)

library(ggplot2)
clusters = kmeans$cluster
data = cbind(dataset,clusters) 
data$clusters = as.factor(data$clusters)

#plotar
ggplot(data = data, aes(x = AnnualIncome, y = SpendingScore, colour = clusters))+
  geom_point()
#salvar
jpeg("clusters.jpeg")
ggplot(data = data, aes(x = AnnualIncome, y = SpendingScore, colour = clusters))+
  geom_point()
dev.off()
  
  