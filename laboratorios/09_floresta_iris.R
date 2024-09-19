# a matéria da prova vai até aqui

install.packages("randomForest")

library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)

data(iris)
dados <- iris
set.seed(123)

indices_treino <- sample(1:nrow(dados), 
                         size = 0.8 * nrow(dados), 
                         replace = FALSE)

treino <- dados[indices_treino,]
teste <- dados[ -(indices_treino),]

par(mfrow = c(1,3))

# árvore 01
treino_a01 <- treino[sample(indices_treino, 
                            size = nrow(treino), 
                            replace = TRUE),]

modelo_a01 <- rpart(formula = Species~)