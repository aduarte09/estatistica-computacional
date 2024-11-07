library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stopwords)
library(rvest)

url <- "https://www.bbc.com/portuguese/articles/c303ev01582o"
html <- read_html(url)

texto <- html |>
  html_elements("p.bbc-hhl7in") |>
  html_text2()
texto <- paste0(texto, collapse = " ")

conjunto <- data.frame(texto)
conjunto |>
  unnest_tokens(input = texto, output = word) |>
  count(word, sort = TRUE) |>
  top_n(10)

stopwords_br <- data.frame(word = stopwords("pt"))

conjunto |>
  unnest_tokens(output = word, input = texto) |>
  anti_join(stopwords_br) |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(y = word, x = n)) + 
  geom_col(color = "lightblue", fill = "lightblue") +
  theme_minimal()

