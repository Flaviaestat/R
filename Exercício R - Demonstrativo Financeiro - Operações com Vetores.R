rm(list = ls()) #limpa ambiente
cat("\14")      #limpa console

meses <- c("JAN", "FEV", "MAR", "ABR" , "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

receita <- c(357, 210, 210, 294, 363, 384, 347, 370, 259, 232, 323, 326)

despesa <- c(111, 180, 182, 200, 285, 195, 136, 175, 215, 190, 250, 230)

Lucro <- receita - despesa

imposto <- Lucro * 0.30

MargemLucro <- Lucro - imposto

mediaLucro <- mean(Lucro)


#Gerando melhores meses de lucro#
tabela <- data.frame(marcacaoMelhores, meses)
tabela_melhores <- subset(tabela, tabela$marcacaoMelhores %in% TRUE)
tabela_melhores

sum(tabela$marcacaoMelhores == 'TRUE')

flagMelhores <- ifelse(tabela$marcacaoMelhores == 'TRUE', 1, 0)

tabela <- cbind(tabela, flagMelhores)
