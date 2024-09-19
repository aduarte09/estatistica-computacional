# O objetivo deste laboratório é criar um modelo de classificação que seja capaz de prever a espécie de um pinguim a partir de seu peso, tamanho da asa, tamanho do bico e profundidade do bico. Para isso, construiremos do zero um modelo KNN.

# Lendo o conjunto
library(palmerpenguins)
pinguins <- penguins
str(pinguins)
summary(pinguins)

pinguins <- na.omit(pinguins)
summary(pinguins)

# Dividindo em treino e teste
pinguins <- pinguins[sample(nrow(pinguins)),]
n <- 0.8 * nrow(pinguins)

treino <- pinguins[(1:n),]
teste <- pinguins[-(1:n),]

# Visualização de dados
library(ggplot2)
ggplot(data = treino, mapping = aes(x = species, y = flipper_length_mm)) + 
  geom_boxplot() +
  labs(x = "Espécies",
       y = "Tamanho da asa (mm)") + 
  theme_minimal()

ggplot(data = treino, aes(x = body_mass_g, y = bill_length_mm, color = species)) + 
  geom_point(size = 1.2, alpha = 0.5) +
  labs(x = "Massa corporal (g)",
       y = "Tamanho da asa (mm)",
       col = "Espécies") + 
  theme_minimal()

# Classificando o primeiro pinguim do teste
distancias <- c()
for(k in 1:nrow(treino)) {
  distancias[k] <- sqrt((teste$body_mass_g[1] - treino$body_mass_g[k])**2 + (teste$bill_length_mm[1] - treino$bill_length_mm[k])**2)
}
treino$species[order(distancias)[1]]
treino$species[1]

# Classificando todos os pinguins
classificacao <- c()
for(j in 1:nrow(teste)) {
  distancias <- c()
  for(k in 1:nrow(treino)) {
    distancias[k] <- sqrt((teste$body_mass_g[1] - treino$body_mass_g[k])**2 + (teste$bill_length_mm[1] - treino$bill_length_mm[k])**2)
  }
classificacao[j] <- as.character(treino$species[order(distancias)[1]])
}
mean(classificacao == teste$species)
