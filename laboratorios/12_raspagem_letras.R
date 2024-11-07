install.packages("rvest")
install.packages("dplyr")
install.packages("tidytext")

library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)

url <- "https://www.letras.mus.br/ray-charles/214417/"

html <- read_html(url)
html |>
  html_elements("h1") |>
  html_text2()

letra <- html |>
  html_elements("div.lyric") |>
  html_elements("p") |>
  html_text2() |>
  paste(collapse = " ")
letra

letra <- data.frame(letra)
View(letra)

letra |>
  unnest_tokens(output = palavra, input = letra) |>
  count(palavra, sort = TRUE) |>
  head(n = 10) |>
  ggplot(aes(x = n, y = palavra)) + 
  geom_col()
