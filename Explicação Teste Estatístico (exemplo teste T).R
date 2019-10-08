
num_campanhas = 100000
media = 100

vectorResults <- c()

for(i in 1:num_campanhas){
  
dist1 = rnorm(30, mean=media, sd=20)
dist2 = rnorm(30, mean=media, sd=20)
teste = t.test(dist1,dist2,paired=TRUE)

vectorResults <- cbind(vectorResults, teste$p.value)

}

vectorResultsString <- vectorResults <= 0.20

falsoNegativo = sum(vectorResultsString) / num_campanhas
falsoNegativo