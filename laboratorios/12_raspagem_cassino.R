install.packages("httr")
install.packages("dplyr")
install.packages("jsonlite")

library(httr)
library(dplyr)
library(jsonlite)

# link da api
url_api <- "https://ads.mythad.com/log/common/co/api"

GET(url_api)
content(GET(url_api))

# link do histórico (é o que queremos)
url_history <- "https://blaze1.space/api/roulette_games/history?startDate=2024-08-14T16:45:25.300Z&endDate=2024-09-13T16:45:25.301Z&page=1"

GET(url_history)
dados <- content(GET(url_history), "text")
dados

# organizando os dados e criando uma tabela de frequência das cores
dados <- fromJSON(dados)
dados
table(dados$records$color)

url_base <- "https://blaze1.space/api/roulette_games/history?startDate=2024-08-14T16:45:25.300Z&endDate=2024-09-13T16:45:25.301Z&page="

resultados <- c()
for(i in 1:50) {
  url <- paste0(url_base, i)
  dados <- content(GET(url), "text")
  dados <- fromJSON(dados)
  resultados <- c(resultados, dados$records$color)
}

resultados
prop.table(table(resultados))
