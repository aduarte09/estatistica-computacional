install.packages("ggplot2")
library(ggplot2)

# dataset iris
iris
View(iris)

# embaralhamento dos dados
set.seed(1711)
iris <- iris[sample(nrow(iris)),]
n <- round(0.8 * nrow(iris))

# gerando os grupos de treinamento e teste
treinamento <- iris[1:n,]
View(treinamento)

teste <- iris[-(1:n),]
View(teste)

# classificando espécies do teste
resultados <- c()
for(j in 1:nrow(teste)) {
  if(teste$Petal.Length[j] < 2.5) {
    resultados[j] <- "setosa"
  } else {
    if(teste$Petal.Width[j] < 1.75) {
      resultados[j] <- "versicolor"
    } else {
      resultados[j] <- "virginica"
    }
  }
}
teste$Species == resultados
mean(teste$Species == resultados)


# gerando gráficos com ggplot
# point
ggplot(data = treinamento, mapping = aes(x = Petal.Length, 
                                         y = Petal.Width, 
                                         col = Species)) + 
  geom_point(size = 1.5, alpha = 0.5) + 
  theme_minimal()

ggplot(data = treinamento, mapping = aes(x = Sepal.Length, 
                                         y = Sepal.Width, 
                                         col = Species)) + 
  geom_point(size = 1.5, alpha = 0.5) + 
  theme_minimal()

# bar
ggplot(data = treinamento, mapping = aes(x = Species)) + 
  geom_bar() + 
  theme_minimal()

# histogram
ggplot(data = treinamento, mapping = aes(x = Petal.Length)) +
  geom_histogram(bins = 20, fill = "#FF7777") + 
  theme_minimal()

# boxplot
summary(treinamento$Petal.Length)
ggplot(data = treinamento, mapping = aes(y = Petal.Length)) + 
  geom_boxplot() + 
  facet_wrap(~Species)

ggplot(data = treinamento, mapping = aes(y = Petal.Width)) +
  geom_boxplot() +
  facet_wrap(~Species)

ggplot(data = treinamento, mapping = aes(y = Sepal.Length)) +
  geom_boxplot() +
  facet_wrap(~Species)

ggplot(data = treinamento, mapping = aes(y = Sepal.Width)) +
  geom_boxplot() +
  facet_wrap(~Species)
