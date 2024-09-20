# bpm alto? (23% de pureza)
  # sim -> colesterol alto? (46% de pureza)
            # sim -> 
            # não -> 
  # não -> sem doença no coração

# grau de pureza parcial e total
# Gp = Pa(1 - Pa) + Pb(1 - Pb) 
      # proporção de a = Pa, proporção de b = Pb
# Gt = Pa/Ta * Gap + Pb/Tb * Gbp
      # proporção de a = Pa, total de a = Ta, proporção de b = Pb, total de b = Tb,         Gap = grau de pureza de a, gpb = grau de pureza de b

install.packages("rpart")
install.packages("rpart.plot")

library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)

# lendo dados do "rpart"# lendo dados do arquivo
cancer <- read.csv(file = "cancer.csv", header = TRUE)
cancer <- na.omit(cancer)
str(cancer)

# dividindo em treino e teste
n <- round(0.8 * nrow(cancer))
treino <- cancer[1:n,]
teste <- cancer[-(1:n),]

modelo.arvore <- rpart(formula = diagnosis~ ., data = treino, 
                       method = "class")
rpart.plot(modelo.arvore, extra = 101)

previsao <- predict(modelo.arvore, newdata = teste, type = "class")
mean(previsao == teste$diagnosis)

# matriz de confusão
table(previsao, teste$diagnosis)
