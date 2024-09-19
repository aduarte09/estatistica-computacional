# lendo informações do arquivo
dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")


# resumo dos dados
summary(dados)


# total de sobreviventes
sum(dados[,2])/length(dados[,2])


# resumo dos dados (sem id e embarked)
dados <- dados[,-c(1,12)]
summary(dados)


# transformando variável em factor
dados$Survived <- as.factor(dados$Survived)
summary(dados)

dados$Pclass <- as.factor(dados$Pclass)
summary(dados)

dados$Sex <- as.factor(dados$Sex)
summary(dados)


# quantos homens morreram?
homens <- dados[dados$Sex == "male",]
summary(homens)

table(homens$Survided)                  # gerando tabela
barplot(table(homens$Survived))         # gerando gráfico

sum(homens$Survived == 1)


# quantas mulheres morreram?
mulheres <- dados[dados$Sex == "female",]
summary(mulheres)

table(mulheres$Survived)                # gerando tabela
barplot(table(mulheres$Survived))       # gerando gráfico

sum(mulheres$Survived == 1)


# quantos homens da 3ª classe sobreviveram?
terceira_homens <- homens[homens$Pclass == "3",]
summary(terceira_homens)

table(terceira_homens$Survived)


# assunto da próxima aula...
library(ggplot2)
ggplot(data = homens, aes(x = Pclass, fill = Survived)) + geom_bar()