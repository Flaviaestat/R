#---------------------------- DATA ####

getwd()
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 01 - R\\R\\Exercícios")

movies = read.csv("Movie-Ratings.csv")
head(movies)
#alguns simbolos nao sao lidos e substituidos por ponto
colnames(movies) = c("Filme", "Genero", "AvaliacaoCritica", "AvaliacaoAudiencia", "OrcamentoMM", "Ano")
head(movies)
tail(movies)
str(movies) 
summary(movies) #ano precisa ser numerica? precisamos do ano como fator

#transformar em fator
factor(movies$Ano)
movies$Ano = as.factor(movies$Ano)
summary(movies)
str(movies)

#---------------------------- AESTHETICS ####
library(ggplot2)

ggplot(data=movies, aes(x = AvaliacaoCritica, 
                        y = AvaliacaoAudiencia))

#vai criar um grafico vazio. ggplot ainda nao sabe o que queremos
#linhas? bolinhas? etc..

#---------------------------- GEOMETRY ####

ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia)) +
         geom_point() #bolinhas

#adicionar cor por genero
ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia, 
                        colour = Genero)) +
  geom_point() #bolinhas

#adicionar tamanho por genero: nao faz sentido: warning: explicar
ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia, 
                        colour = Genero, size= Genero)) +
  geom_point() #bolinhas

#adicionar tamanho com outra variavel numerica
ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia, 
                        colour = Genero, size= OrcamentoMM)) +
  geom_point() #bolinhas
#analisando: audiencia da avalia melhor que a critica. valores acima da diagonal

# Plotando em camadas
p = ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia, 
                            colour = Genero, size= OrcamentoMM))

#ponto
p + geom_point()

#linhas
p + geom_line()

#multiplas camadas
p + geom_point() + geom_line()
p + geom_line() + geom_point() #conceito de ggplot : plot em camadas.


#---------------------------- OVERRIDING AESTHETICS ####
q = ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia, 
                            colour = Genero, size= OrcamentoMM))
q + geom_point()

#aes está sendo herdada do objeto q. vamos fazer um override (sobrepor)
q + geom_point(aes(size=AvaliacaoCritica)) # ex1

#ex2
q + geom_point(aes(colour=OrcamentoMM)) #bug

#ex3: insight: orcamento nao afeta muito a critica
q + geom_point(aes(x= OrcamentoMM)) #bug
q + geom_point(aes(x= OrcamentoMM)) + xlab("Orçamento") #bug fix

#ex4 reduzir linha
q + geom_line()+  geom_point()
q + geom_line(size=1)+  geom_point() #mapping vs setting: setting nao precisa de aes

# MAPPING VS SETTING
r = ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia))

#1. mapping
r + geom_point(aes(colour=Genero))
#2. setting
r + geom_point(colour="DarkGreen")
#ERRO
r + geom_point(aes(colour="DarkGreen")) #está mapeando uma cor para a variavel darkgreen

#---------------------------- STATISTICS ####

#histogramas e graficos de densidade

s = ggplot(data=movies, aes(x = OrcamentoMM))
s+ geom_histogram(binwidth = 10) #quao largo eh cada caixa. quantos filmes caem no orcamento de 0 a 10 MM, etc
#estatistica: pq esta agregando os dados nos intervalos desejados
#filmes de comedia e drama tem em geral orcamento menor, e acao maior

s + geom_histogram(binwidth = 10, fill="Green") #setting
s + geom_histogram(binwidth = 10, aes(fill=Genero)) #mapping

s + geom_histogram(binwidth = 10, aes(fill=Genero), colour= "Black") #add border

#grafico de densidade, nao apresentar!!!!
s+geom_density()
s+geom_density(aes(fill=Genero))
s+geom_density(aes(fill=Genero), position = "stack")

# Layer tips

t = ggplot(data=movies)
t + geom_histogram(binwidth = 10, aes(x = AvaliacaoAudiencia),
                   fill="White", colour= "blue") #distribuicao normal    ######grafico 1!!!!!

t + geom_histogram(binwidth = 10, aes(x = AvaliacaoCritica),
                   fill="White", colour= "blue") #distribuicao uniforme   ######grafico 2!!!!!

#criticos tem um conjunto de regras para avaliar os filmes, por isso a distribuicao eh diferente

#SMOOTH
?geom_smooth #ajuda a olhar padroes no grafico

u = ggplot(data= movies, aes(x= AvaliacaoCritica, y = AvaliacaoAudiencia, colour=Genero))
u + geom_point() + geom_smooth(fill=NA)        ######grafico 3!!!!!

#roxa:romance. relacao entre as criticas: critica e audiencia
#estaticamente: se critica avalia baixo ao redor de 25, eh bem proavel q a audiencia avalie alto : +ou-50
#comparando action e horror: se a critica avalia alto, e provavel que a audiencia
#avalie melhor um filme de acao a um filme de horror com mesma nota da critica


#boxplots : muito popular: usamos mt em ciencia de dados. facil de entender

v = ggplot(data=movies, aes(x=Genero, y=AvaliacaoAudiencia, colour=Genero))
v + geom_boxplot()

#ver a distribuicao dos dados
v + geom_boxplot() + geom_jitter()
#outro jeito
v + geom_jitter() + geom_boxplot(alpha = 0.5) ######grafico 4!!!!!

#baseado no genero: thriller tem uma caixa mais estreita, entao eh mais facil estar nesse ramo, mediana mais alta
#ja no ramo de horror, eh mais arriscado. mediana mais baixa

#---------------------------- FACETS ####

w = ggplot(data=movies, aes(x = AvaliacaoCritica, y = AvaliacaoAudiencia, colour=Genero, size=OrcamentoMM))
w + geom_point()

#facets: criar varios graficos
w + geom_point() + facet_grid(Genero~.) #left: rows, right: columns

w + geom_point() + facet_grid(.~Ano) 

w + geom_point() + facet_grid(Genero~Ano) 

w + geom_point() + facet_grid(Genero~Ano) + geom_smooth()

#o intervalo de confianca esta mudando a variacao dos eixos: entao vamos fazer um zoom
# para manter entre 0 e 100
#com coordinates

#---------------------------- COORDINATES ####
w + geom_point()

#limitar o eixo x
w + geom_point() + xlim(50,100)

#e o y
w + geom_point() + xlim(50,100) + ylim(20, 100)


#pro histograma
s + geom_histogram(binwidth = 10, aes(fill=Genero), colour= "Black")
s + geom_histogram(binwidth = 10, aes(fill=Genero), colour= "Black") + 
  ylim(0,50) #eerrrroooo

#lim deleta dados, ja o zoom nao

#ZOOM

s + geom_histogram(binwidth = 10, aes(fill=Genero), colour= "Black") +
  coord_cartesian(ylim = c(0,50)) #os dados ainda estao la, e o zoom foi feito


#voltando pro nosso grafico: y entre 0 e 100
w + geom_point() + facet_grid(Genero~Ano) + geom_smooth() +coord_cartesian(ylim=c(0,100))   ######grafico 5!!!!!
#comedia tem notas de audiencia mais intermediarias e nao tem mt relacao com crirtica
#acao tem relacao direta
#play safe: comedy
#risky: acao


#---------------------------- THEME ####
s + geom_histogram(binwidth = 10, aes(fill=Genero), colour= "Black")
histograma = s + geom_histogram(binwidth = 10, aes(fill=Genero), colour= "Black")

#labels dos eixos
histograma + xlab("Dinheiro") + ylab("Número de Filmes")

#formatacao do label
histograma + xlab("Dinheiro") + ylab("Número de Filmes") +
  theme(axis.title.x = element_text(colour="DarkGreen", size=20),
        axis.title.y = element_text(colour="Red", size=20))

#marcas dos numeros nos eixos
histograma + xlab("Dinheiro") + ylab("Número de Filmes") +
  theme(axis.title.x = element_text(colour="DarkGreen", size=20),
        axis.title.y = element_text(colour="Red", size=20),
        axis.text.x = element_text(size=20))

#lugar da legenda
histograma + xlab("Dinheiro") + ylab("Número de Filmes") +
  theme(axis.title.x = element_text(colour="DarkGreen", size=20),
        axis.title.y = element_text(colour="Red", size=20),
        legend.position = c(1,1),
        legend.justification = c(1,1))

#titulo
histograma + xlab("Dinheiro") + ylab("Número de Filmes") +
  ggtitle("Distribuiçao de Orçamento") +
  theme(axis.title.x = element_text(colour="DarkGreen", size=20),
        axis.title.y = element_text(colour="Red", size=20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        
        plot.title = element_text(size=30, colour = "darkblue"))
##############grafico 6 !!!!!!







