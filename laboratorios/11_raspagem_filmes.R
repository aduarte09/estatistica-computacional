install.packages("dplyr")
install.packages("rvest")
install.packages("stringr")

library("dplyr")       # manipula dados
library("rvest")       # web scraping / raspagem de dados
library("stringr")     # manipula texto

# site com a lista de melhores filmes
url <- "https://www.bosshunting.com.au/entertainment/movies/best-movies-imdb/"
html <- read_html(url)
html

filmes <- html |>
  html_elements("ol.wp-block-list") |>
  html_elements("li") |>
  html_text2()
filmes

posicao <- c(1:100)

#terminar titulo
titulo <- str_remove_all

ano <- unlist(str_extract_all(string = filmes, pattern = "\\(\\d+\\)"))
ano <- as.numeric(unlist(str_extract_all(string = ano, pattern = "\\d+")))
ano

#terminar diretor
diretor <- unlist(str_extract_all(string = filmes, pattern = "dir. "))

nota <- unlist(str_extract_all(string = filmes, pattern = "\\d\\.\\d$"))
nota <- as.numeric(nota)

melhores_filmes <- data.frame(posicao, titulo, ano, diretor, nota)
write.csv(melhores_filmes, file = "melhores_filmes.csv", row.names = FALSE)