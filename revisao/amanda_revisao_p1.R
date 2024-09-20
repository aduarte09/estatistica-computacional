# exercício 01

moeda <- c("cara", "coroa")      # definindo a moeda
historico_vencedor <- c()        # quem venceu em cada partida do teste

for(i in 1:10000) {
  juju <- 18                     # inicializando o dinheiro da vovó juju
  jorel <- 7                     # inicializando o dinheiro do jorel
  
  while((juju != 0) & (jorel != 0)) {
    jogar_moeda <- sample(moeda, size = 1, replace = TRUE)
    
    if(jogar_moeda == "cara") {
      juju <- juju + 1
      jorel <- jorel - 1
    } else {
      juju <- juju - 1
      jorel <- jorel + 1
    }
    
    if(juju == 0) {
      vencedor <- "jorel"
    } else {
      vencedor <- "juju"
    }
  }
  
  historico_vencedor <- c(historico_vencedor, vencedor)
}

media_jorel <- mean(historico_vencedor == "jorel")
media_jorel                      # média de vezes que jorel ganhou

media_juju <- mean(historico_vencedor == "juju")
media_juju                       # média de vezes que vovó juju ganhou

# como vovó juju começa com uma quantia maior que a de jorel, ela tem maiores chances (cerca de 70%) de vencer jorel.


# exercício 02

bilhetes <- c(1:30)              # inicializando os bilhetes
resultado <- c()                 # quantas tentativas em cada teste

for(i in 1:10000) {
  bilhetes_sorteados <- c()      # guarda os bilhetes sorteados no teste
  n <- 0                         # tentativas necessárias
  
  while(length(unique(bilhetes_sorteados)) < 30) {
    bilhetes_sorteados <- c(bilhetes_sorteados, sample(bilhetes, size = 1, replace = TRUE))
    n <- n + 1
  }
  
  resultado <- c(resultado, n)
}

media_n <- mean(resultado)
media_n                           # quantas tentativas

# são necessárias cerca de 120 tentativas para obter cada um dos trinta bilhetes pelo menos uma vez cada.


# exercício 03

chicago <- read.csv(file = "chicago.csv", header = TRUE)
str(chicago)
chicago <- chicago[,-1]           # removendo os índices


# 03. a) 68398 pessoas morreram por doenças cardiovasculares. O inverno foi a estação com mais mortes causadas por doenças cardiovasculares, enquanto o verão teve o menor número.

# contando o número de mortes por doenças cardiovasculares
mortes_cvd <- sum(chicago$cvd)
mortes_cvd

chicago$season <- as.factor(chicago$season)
str(chicago)

# separando os dados a partir das estações
chicago_primavera <- chicago[chicago$season == "Spring",]
chicago_verao <- chicago[chicago$season == "Summer",]
chicago_outono <- chicago[chicago$season == "Autumn",]
chicago_inverno <- chicago[chicago$season == "Winter",]

# contando o número de mortes em cada estação
mortes_estacao_cvd <- c(
  sum(chicago_primavera$cvd), 
  sum(chicago_verao$cvd), 
  sum(chicago_outono$cvd), 
  sum(chicago_inverno$cvd))

# qual estação teve o maior número de mortes por doenças cardiovasculares
which.max(mortes_estacao_cvd)
# qual estação teve o menor número de mortes por doenças cardiovasculares
which.min(mortes_estacao_cvd)


# 03. b) O ano de 1999 teve o maior número de mortes causadas por doenças respiratórias.

# separando os dados a partir dos anos
chicago_1997 <- chicago[chicago$year == 1997,]
chicago_1998 <- chicago[chicago$year == 1998,]
chicago_1999 <- chicago[chicago$year == 1999,]
chicago_2000 <- chicago[chicago$year == 2000,]

# contando o número de mortes em cada ano
mortes_ano_resp <- c(
  sum(chicago_1997$resp),
  sum(chicago_1998$resp),
  sum(chicago_1999$resp),
  sum(chicago_2000$resp))

# qual ano teve o maior número de mortes causadas por doenças respiratórias
which.max(mortes_ano_resp)


# 03. c) A média de temperatura é 59º na primavera, 70º no verão, 41º no outono e 32º no inverno. A média da umidade relativa é 64% na primavera, 70% no verão, 74% no outono e 77% no inverno.

# calculando a média de temperatura para cada estação
media_temp_primavera <- mean(chicago_primavera$temp)
media_temp_primavera

media_temp_verao <- mean(chicago_verao$temp)
media_temp_verao

media_temp_outono <- mean(chicago_outono$temp)
media_temp_outono

media_temp_inverno <- mean(chicago_inverno$temp)
media_temp_inverno

# calculando a média da umidade relativa para cada estação
media_rhum_primavera <- mean(chicago_primavera$rhum, na.rm = TRUE)
media_rhum_primavera

media_rhum_verao <- mean(chicago_verao$rhum, na.rm = TRUE)
media_rhum_verao

media_rhum_outono <- mean(chicago_outono$rhum, na.rm = TRUE)
media_rhum_outono

media_rhum_inverno <- mean(chicago_inverno$rhum, na.rm = TRUE)
media_rhum_inverno

# gerando o boxplot
library(ggplot2)
ggplot(data = chicago, mapping = aes(x = season, y = temp)) +
  geom_boxplot() +
  theme_minimal()


# 03. d) gerando o gráfico de pontos
ggplot(data = chicago, mapping = aes(x = time, y = temp, col = season)) +
  geom_point() +
  theme_minimal()


# exercício 04

# 04. a) lendo o arquivo
papagaio <- read.table(file = "papagaio.txt", header = TRUE, sep = ",")

head(papagaio)
tail(papagaio)
str(papagaio)
summary(papagaio)


# 04. b) gerando um gráfico com a quantidade de cada espécie
ggplot(data = papagaio, mapping = aes(x = especie)) + 
  geom_bar() +
  theme_minimal()


# 04. c) gerando e analisando gráficos
ggplot(data = papagaio, mapping = aes(x = peso, y = tamanho, col = especie)) +
  geom_point() +
  theme_minimal()

# Papagaios da espécie Arctica são menores e mais leves em relação as outras espécies. Corniculata são mais pesados que os Cirrhata, mesmo com alturas similares.

ggplot(data = papagaio, mapping = aes(x = especie, y = envergadura)) +
  geom_boxplot() +
  theme_minimal()

# Os arctica possuem envergadura menor que os demais, enquanto os Cirrhata, a maior.


# 04. d) construindo uma árvore de decisão

# aleatorizando os dados
papagaio <- papagaio[sample(nrow(papagaio)),]
n <- round(0.8 * nrow(papagaio))

# dividindo em treino e teste
papagaio_treino <- papagaio[(1:n),]
papagaio_teste <- papagaio[-(1:n),]

# utilizando os mesmos gráficos de antes, porém apenas com dados de treino
ggplot(data = papagaio_treino, mapping = aes(x = peso, y = tamanho, col = especie)) +
  geom_point() +
  theme_minimal()

ggplot(data = papagaio_treino, mapping = aes(x = especie, y = envergadura)) +
  geom_boxplot() +
  theme_minimal()

# classificando as espécies
classificacao <- c()

for(i in 1:nrow(papagaio_teste)) {
  if((papagaio_teste$peso[i] <= 400) & (papagaio_teste$tamanho[i] < 35)) {
    classificacao[i] <- "arctica"
  } else {
    if((papagaio_teste$envergadura[i] > 55) & (papagaio_teste$peso[i] > 700)) {
      classificacao[i] <- "cirrhata"
    }
    else {
      classificacao[i] <- "corniculata"
    }
  }
}

# calculando a taxa de acerto da árvore de decisão
taxa_acerto_arvore <- mean(papagaio_teste$especie == classificacao)
taxa_acerto_arvore


# 04. e) construindo um modelo knn

library(class)

# padronizando os dados
papagaio_treino_padrao <- scale(papagaio_treino[,-4])
papagaio_teste_padrao <- scale(papagaio_teste[,-4])

classe_treino <- papagaio_treino$especie
classe_teste <- papagaio_teste$especie

classificacao <- knn(train = papagaio_treino_padrao, test = papagaio_teste_padrao, cl = classe_treino, k = 1)

# calculando a taxa de acerto do modelo knn
taxa_acerto_knn <- mean(classificacao == classe_teste)
taxa_acerto_knn

# O modelo knn teve uma taxa de acerto maior que a árvore de decisão.
