install.packages("dplyr")
install.packages("rvest")
install.packages("stringr")

library("dplyr")       # manipula dados
library("rvest")       # web scraping / raspagem de dados
library("stringr")     # manipula texto

# site com a lista dos filmes de terror
url <- "https://www.timeout.com/film/best-horror-films"
html <- read_html(url)

# pegando tudo que está com tag de h3, na classe _h3_cuogz_1
filmes <- html |>
  html_elements("h3._h3_cuogz_1") |>
  html_text2()

# extraindo as colocações dos filmes
posicao <- str_extract_all(string = filmes, pattern = "^\\d+")
posicao <- unlist(posicao)
posicao

posicao <- as.numeric(posicao)
posicao

# extraindo os anos dos filmes
ano <- unlist(str_extract_all(string = filmes, pattern = "\\(\\d+\\)$"))
ano <- as.numeric(unlist(str_extract_all(string = ano, pattern = "\\d+")))
ano

# extraindo os nomes dos filmes
nome <- str_remove_all(string = filmes, pattern = "^\\d+\\.\\s+")
nome <- str_remove_all(string = nome, pattern = "\\s+\\(\\d+\\)$")
nome

# criando um data frame com todas as informações extraídas
filmes_horror <- data.frame(posicao, nome, ano)

# salvando o data frame criado num arquivo csv
write.csv(filmes_horror, file = "filmes_horror.csv", row.names = FALSE)

