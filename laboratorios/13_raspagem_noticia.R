library(rvest)
library(dplyr)
library(tidytext)

# ler a url e importar a html
url <- "https://www.bbc.com/portuguese/articles/cql3lwgl3ldo"
html <- read_html(url)
html

# identificar o título
titulo <- html |>
  html_elements("h1") |>
  html_text2()
titulo

# lendo o texto
noticia <- html |>
  html_elements("p.bbc-hhl7in") |>
  html_text2()
noticia

# formatando o texto para criar um data frame
noticia <- paste0(noticia, collapse = " ")
noticia

# criando data frame
artigos <- data.frame(titulo, noticia)
View(artigos)

# quebrando em palavras
?unnest_tokens         # token = palavra
artigos |>
  unnest_tokens(output = words, input = noticia) |>
  count(words) |>
  arrange(desc(n))
