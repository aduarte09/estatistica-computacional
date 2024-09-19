library(rvest)  # raspagem de páginas estáticas
library(dplyr)  # manipulação de dados

# raspagem de dados
# passo 1: pegar a url
url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_taxa_de_homicídios"

html <- read_html(url)
html

# html-elements(html, "table")
tabelas <- html |>
  html_elements("table") |>
  html_table()

homicidios <- tabelas[[3]]
