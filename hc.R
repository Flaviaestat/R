# Hierarchical Clustering

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 09\\Exercicios")
# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'))
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'))
y_hc = cutree(hc, 5)
y_hc

library(ggplot2)
data = cbind(dataset,y_hc) 
data$y_hc = as.factor(data$y_hc)

#plotar
ggplot(data = data, aes(x = AnnualIncome, y = SpendingScore, colour = y_hc))+
  geom_point()

#salvar
jpeg("clustersHC.jpeg")
ggplot(data = data, aes(x = AnnualIncome, y = SpendingScore, colour = y_hc))+
  geom_point()
dev.off()
