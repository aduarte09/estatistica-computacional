library(ggplot2)
library(class)

cancer <- read.csv(file = "cancer.csv", header = TRUE)
str(cancer)
cancer <- na.omit(cancer)

n <- round(0.8 * nrow(cancer))
treino <- cancer[1:n,]
teste <- cancer[-(1:n),]

treino_padronizado <- scale(treino[, -1])
teste_padronizado <- scale(teste[, -1])

classe_treino <- treino$diagnosis
classe_teste <- teste$diagnosis

modelo_cancer <- knn(train = treino_padronizado, test = teste_padronizado, k = 1, classe_treino)

mean(modelo_cancer == classe_teste)

# matriz de confusão
table(modelo_cancer, classe_teste)

# fazer análise gráfica desse problema: como que o tamanho influencia se é B ou M