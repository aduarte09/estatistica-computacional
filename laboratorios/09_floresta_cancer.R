install.packages("randomForest")

library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)

dados_cancer <- read.csv(file = "cancer.csv", header = TRUE)
str(dados_cancer)
dados_cancer$diagnosis <- as.factor(dados_cancer$diagnosis)

set.seed(1731)
n <- round(0.8 * nrow(dados_cancer))
indices <- sample(1:nrow(dados_cancer), size = n, replace = FALSE)

treino_cancer <- dados_cancer[indices,]
teste_cancer <- dados_cancer[-indices,]

arvore <- rpart(formula = diagnosis ~ ., 
                data = treino_cancer, 
                method = "class")
previsao.arvore <- predict(arvore, 
                           newdata = teste_cancer, 
                           type = "class")
mean(previsao.arvore == teste_cancer$diagnosis)


floresta <- randomForest(formula = diagnosis ~ .,
                         data = treino_cancer,
                         ntree = 200)
floresta

previsao.floresta <- predict(floresta, 
                            newdata = teste_cancer, 
                            type = "class")
previsao.floresta
mean(previsao.floresta == teste_cancer$diagnosis)
