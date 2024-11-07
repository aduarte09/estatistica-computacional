# teorema de bayes

numerador <- (1/5000) * 0.999
denominador <- ((1/5000) * 0.999) + ((4999/5000) * (1 - 0.999))
numerador/denominador

20 * 0.999
999980 * (1 - 0.999)


# classificação de notícias

install.packages(quanteada)

library(dplyr)
library(tidytext)
library(stopwords)
library(SnowballC)                # lematização
library(quanteada)                # análise quantitativa do texto
library(quanteada.textmodels)     # modelagem de texto

noticias <- read.csv(file = "noticias.csv", header = TRUE)
stopwords_pt <- data.frame(word = stopwords("pt"))

noticias[1,] |>
  unnest_tokens(word, texto) |>
  anti_join(stopwords_pt) |>
  count(word, sort = TRUE) |>
  top_n(20)

noticias_tokens <- tokens(noticias$texto,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_numbers= TRUE,
                          remove_url = TRUE) |>
  tokens_remove(stopwords("pt")) |>
  tokens_wordstem(language = "portuguese")

matriz_frequencia <- dfm(noticias_tokens)

n <- round(0.8 * nrow(noticias))
indices <- sample(1:nrow(noticias), size = n, replace = FALSE)

treino <- matriz_frequencia[indices,]
teste <- matriz_frequencia[-(indices),]

modelo_nb <- textmodel_nb(treino, noticias$categorias[indices])

previsao <- predict(modelo_nb, newdata = teste)

mean(previsao == noticias$categorias[-(indices)])
