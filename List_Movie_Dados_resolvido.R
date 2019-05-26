movieName = "The Shining"
actors=c("Jack Nicholson",
         "Shelley Duvall",
         "Danny Lloyd",
         "Scatman Crothers",
         "Barry Nelson")
sources = c(1,2,3)
scores = c(4.5,4.0,5.0)
comments = c("Best Horror Film I Have Ever Seen",
             "A truly brilliant and scary film from Stanley Kubrick",
             "A masterpiece of psychological horror")

reviews = data.frame(scores,sources, comments)

myList = list(movieName, actors, scores)

#segundo vetor da lista
myList[2]

#segundo elemento do segundo vetor = segundo ator da lista
myList[[2]][2]

#melhor score
indice_score_max = which.max(scores)

#retornando dados para o maior score
reviews[indice_score_max,]

library(ggplot2)