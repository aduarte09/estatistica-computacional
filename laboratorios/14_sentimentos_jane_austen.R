library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stopwords)
library(rvest)
library(janeaustenr)
library(stringr)

livro <- prideprejudice
texto <- data.frame(livro)
stopwords_en <- data.frame(word = stopwords("en"))

texto |>
  unnest_tokens(output = word, input = livro) |>
  anti_join(stopwords_en) |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(y = word, x = n)) +
    geom_col(color = "pink", fill = "pink") +
    theme_minimal()

sentimentos <- get_sentiments("bing")

texto |>
  unnest_tokens(output = word, input = livro) |>
  inner_join(sentimentos)

capitulos <- str_detect(texto$livro, "^Chapter \\d+")
capitulos <- cumsum(capitulos)
capitulos

texto |>
  mutate(capitulo = capitulos) |>
  unnest_tokens(word, livro) |>
  inner_join(sentimentos) |>
  count(capitulo, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulo, y = total)) +
    geom_col(color = "pink", fill = "pink") +
    theme_minimal()
