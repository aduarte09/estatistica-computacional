library(ggplot2)
library(palmerpenguins)
library(class)

data("penguins")
pinguins <- penguins
str(penguins)
pinguins <- pinguins[, -c(2, 7, 8)]
pinguins <- na.omit(pinguins)

n <- round(0.8 * nrow(pinguins))
indices_treino <- sample(1:nrow(pinguins), size = n, replace = FALSE)
treino <- pinguins[indices_treino,]
teste <- pinguins[-indices_treino,]

treino_padronizado <- scale(treino[,-1])
teste_padronizado <- scale(teste[,-1])

classe_treino <- treino$species
classe_teste <- teste$species

modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = 1)
modelo
mean(modelo == teste$species)

cor(treino[,-1])

taxa_acerto <- c()
for(k in 1:10) {
  modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k = k)
  taxa_acerto[k] <- mean(modelo == teste$species)
}

df <- data.frame(k = 1:10, taxa_acerto)
ggplot(data = df, aes(x = k, y = taxa_acerto)) + 
  geom_line() + 
  theme_minimal()

# fazer com bico com peso
# fazer com bico com asa

# fazer validaçao cruzada
# faz 10 ou 5 pastas, mesma quantidade de pinguins pra todo mundo, vai testando e aumentando a quantidade