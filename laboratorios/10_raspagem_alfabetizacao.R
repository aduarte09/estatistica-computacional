install.packages(geobr)

library(rvest)   # raspagem de páginas estáticas
library(dplyr)   # manipulação de dados
library(stringr) # manipulação de strings
library(geobr)   # informações sobre brasil
library(ggplot2)

# raspagem de dados
url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetização"

html <- read_html(url)
html

# html-elements(html, "table")
tabelas <- html |>
  html_elements("table") |>
  html_table()

alfabetizacao <- tabelas[[3]]
View(alfabetizacao)

# organizando as informações da tabela
alfabetizacao <- alfabetizacao[,c(2,3)]
names(alfabetizacao) <- c("Estado", "Taxa")
View(alfabetizacao)

parte1 <- str_replace_all(string = alfabetizacao$Taxa, 
                          pattern = ",", replacement = ".")

parte2 <- str_replace_all(string = parte1, 
                          pattern = "%", replacement = "")

parte3 <- as.numeric(parte2)
parte3 <- parte3/100

alfabetizacao$Taxa <- parte3

minas <- read_state(code_state = "MG")
ggplot(data = minas) +
  geom_sf() + 
  theme_void()

municipio_mg <- read_municipality(code_muni = "MG")
ggplot(data = municipio_mg) + 
  geom_sf() + 
  theme_void()

estados <- read_state()
estados$name_state
estados <- estados[order(estados$name_state),]
View(estados)

alfabetizacao <- alfabetizacao[order(alfabetizacao$Estado),]
alfabetizacao
View(alfabetizacao)

estados$taxa <- alfabetizacao$Taxa
View(estados)

ggplot(data = estados, aes(fill = taxa)) + 
  geom_sf() + 
  scale_fill_gradient(high = "#132B43", low = "#56B1F7")
