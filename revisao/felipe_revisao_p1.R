#"Questao 1. Vovo Juju e seu neto Jorel estao apostando num jogo de cara ou coroa. Uma moeda e lancada: se sair cara, Vovo Juju recebe 1 real de Jorel; se sair coroa, Vovo Juju paga 1 real para seu neto. No inicio do jogo, Vovo Juju tem 18 reais e Jorel tem 7 reais. O jogo termina quando algum dos jogadores perde todo o seu dinheiro. Simule uma partida deste jogo. A simulacao deve retornar o nome do vencedor do jogo. Agora, simule 10 mil vezes este experimento e calcule a media de vezes em que Jorel ganhou e a media de vezes que Vovo Juju ganhou. Interprete o resultado."


VovoJuju <- 18
Jorel <- 7

#Considera 0 para cara e 1 para coroa
jogocaraoucoroa <- function(){
  
  while(TRUE){
    if(Jorel == 0){
      return("VovoJuju")
    }
    if(VovoJuju == 0){
      return("Jorel")
    }
  
    moeda <- sample(0:1, size = 1)
    
    if(moeda == 0){
      VovoJuju <- VovoJuju + 1
      Jorel <- Jorel - 1
    }else {
      VovoJuju <- VovoJuju - 1
      Jorel <- Jorel + 1
    }
  }
}

#Executando o jogo 1 vez
resultado <- jogocaraoucoroa()

#Executando o jogo 10000 vez
resultados <- replicate(10000, jogocaraoucoroa())

mean(resultados == "VovoJuju")
mean(resultados == "Jorel")

#Questao 2. Uma urna contem bilhetes numerados de 1 ate 30, todos do mesmo tamanho. Considere o seguinte experimento: retirar um bilhete da urna, registrar o resultado e devolver o bilhete para a urna. Voce continuara a sortear bilhetes ate que todos os numeros sejam retirados. Seja N o numero de sorteios que voce precisou realizar ate que todos os numeros fossem registrados. Simule este experimento; a simulacao deve retornar o valor N. Agora, simule 10 mil vezes este experimento. Calcule a media dos resultados obtidos e interprete o valor encontrado.


CompletarAlbum <- function(){
  # Comprar o álbum
  Álbum <- numeric(0)  # Álbum inicialmente vazio
  Álbum_Completo <- 1:30  # Álbum completo para referência
  
  # Quantas vezes você foi bobão tentando completar o álbum
  N <- 0
  
  while(TRUE){
    # Compra de figurinhas
    figurinha <- sample(1:30, 1, TRUE)
    N <- N + 1
    
    # Verificar se a figurinha já está no álbum e adicioná-la caso não
    if(!(figurinha %in% Álbum)){
      Álbum <- c(Álbum, figurinha)
    }
    
    # Verificar se o álbum está completo
    if(setequal(Álbum, Álbum_Completo)){
      return(N)
    }
  }
}

#Execução de 1 Jogo
resultado <- CompletarAlbum()
#Execução de 10000 Jogos
resultados <- replicate(10000, CompletarAlbum())
#Média 10000 Jogos
mean(resultados)  


#Questão 3. O arquivo chicago.csv contem dados de mortalidade diaria (todas as causas exceto acidentes (death), doencas cardiovasculares (cvd), respiratorias(resp)) e clima (temperatura media em °F (temp), umidade relativa media (rhum)) para a cidade de Chicago no periodo 1997-2000. Os dados sao do National Morbidity, Mortality and Air Pollution Study. Importe a partir da funcao read.csv o arquivo chicago.csv para o R. Alem das colunas descritas acima, o conjunto tambem contem as seguintes variaveis: time: sequencia da observacao; year: ano da observacao; month: mes da observacao; day: dia da observacao; season: estacao do ano.

chicago_data <- read.csv("C:/Users/felip/OneDrive/Desktop/Revisão de Estatística/chicago.csv", sep = ",")

str(chicago_data)
chicago_data <- chicago_data[,-1] #Remover linha X = 1,2,3,4,5,6,7,8,etc

#(a) Quantas mortes provocadas por doencas cardiovasculares ocorreram no periodo do estudo? Em qual estacao ocorreram mais mortes provocadas por doencas cardiovasculares? E em qual estacao ocorreram menos mortes provocadas por doencas cardiovasculares? (4 pontos)
  
Mortes_cvd <- sum(chicago_data$cvd) #Mortes por cvd no periodo do estudo

#Separando Todos por estação
Winter <- chicago_data[chicago_data$season == "Winter",]
Spring <- chicago_data[chicago_data$season == "Spring",]
Summer <- chicago_data[chicago_data$season == "Summer",]
Autumn <- chicago_data[chicago_data$season == "Autumn",]

#Somando as mortes por cvd de cada estação
somas <- c(sum(Winter$cvd), sum(Spring$cvd), sum(Summer$cvd), sum(Autumn$cvd))
#Estacoes
estacoes <- c("Winter", "Spring", "Summer", "Autumn")

indice_max <- which.max(somas) #Encontrar indice da estação com maior mortes
estacao_max <- estacoes[indice_max] #Encontrar estação com maior mortes

indice_min <- which.min(somas) #Encontrar indice da estação com menor mortes
estacao_min <- estacoes[indice_min] #Encontrar estação com menor mortes


#(b) Em qual ano ocorreram mais mortes relacionadas a doencas respiratorias? (2 pontos)

Ano1997 <- chicago_data[chicago_data$year == 1997,]
Ano1998 <- chicago_data[chicago_data$year == 1998,]
Ano1999 <- chicago_data[chicago_data$year == 1999,]
Ano2000 <- chicago_data[chicago_data$year == 2000,]

#Somando as mortes por doenças respiratórias de cada ano
somas <- c(sum(Ano1997$resp), sum(Ano1998$resp), sum(Ano1999$resp), sum(Ano2000$resp))

#Ano
Anos <- c("1997", "1998", "1999", "2000")

indice_max <- which.max(somas) #Encontrar indice do ano com maior mortes
Ano_max <- Anos[indice_max] #Encontrar ano com maior mortes

indice_min <- which.min(somas) #Encontrar indice do ano com menor mortes
Ano_min <- Anos[indice_min] #Encontrar ano com menor mortes

#(c) Qual a media de temperatura em cada uma das estacoes? E qual a media da umidade relativa em cada uma das estacoes? Apresente, numa mesma janela e na mesma escala, um boxplot para a variavel temp para cada estacao do ano. (4 pontos)

#Separando por estações
Winter <- chicago_data[chicago_data$season == "Winter",]
Spring <- chicago_data[chicago_data$season == "Spring",]
Summer <- chicago_data[chicago_data$season == "Summer",]
Autumn <- chicago_data[chicago_data$season == "Autumn",]

#Medias de Temperaturas por estação
mean(Winter$temp)
mean(Spring$temp)
mean(Summer$temp)
mean(Autumn$temp)

#Medias umidade relativa por estação
mean(Winter$rhum, na.rm = TRUE)
mean(Spring$rhum, na.rm = TRUE)
mean(Summer$rhum, na.rm = TRUE)
mean(Autumn$rhum, na.rm = TRUE)

#boxplot em relação de temp-season
library(ggplot2)
ggplot(data = chicago_data, mapping = aes(x = season, y = temp))+
  geom_boxplot()+
  theme_minimal()

#(d) Apresente um grafico de pontos para representar as variaveis time (eixo x) e temp (eixo y). Cada ponto do grafico deve ser colorido de acordo com sua estacao. (4 pontos)

ggplot(data = chicago_data, aes(x = time, y = temp, color = season))+
  geom_point()+
  theme_minimal()

#Questao 4. Papagaio-do-mar e o nome comum dado as aves charadriiformes da familia dos alcidos, pertencentes ao genero Fratercula. Existem tres especies de papagaios-do-mar conhecidas: arctica, corniculata e cirrhata. O conjunto papagaio.txt apresenta informacoes sobre o peso (em gramas), o tamanho (em centimetros), a envergadura da asa (em centimetros) e a especie de 500 papagaios-do-mar.

#(a) Leia o arquivo papagaio.txt. Em seguida, comece a analisar os dados a partir das funcoes head, tail, str, summary.

papagaio_data <- read.table("C:/Users/felip/OneDrive/Desktop/Revisão de Estatística/papagaio.txt", sep = ",", header = TRUE)

str(papagaio_data)
summary(papagaio_data)
head(papagaio_data) 
tail(papagaio_data)

#(b) Faca um grafico que apresente a quantidade de aves de cada especie.

ggplot(data = papagaio_data, aes(x = especie))+
  geom_bar()+
  theme_minimal()

#(c) Faca analises graficas para compreender as caracteristicas de cada especie (exemplo: tem alguma especie que se diferencia em relacao a envergadura?). Tente usar diferentes tipos de graficos. Escreva um pequeno texto comentando todos os resultados obtidos.

ggplot(data = papagaio_data, aes(x = envergadura, y = peso, color = especie))+
  geom_point()+
  theme_minimal()

ggplot(data = papagaio_data, aes(x = envergadura, y = tamanho, color = especie))+
  geom_point()+
  theme_minimal()

ggplot(data = papagaio_data, aes(x = tamanho, y = peso, color = especie))+
  geom_point()+
  theme_minimal()

#R: é possível ver que pelo gráfico de especie x peso e pelo gráfico especie x envergadura que os pinguins da espécie cirrhata tem em média um maior peso e envergadura que os demais, que os pinguins da espécie corriculata vem em sequência e por fim os penguins da espécie arctica são os com menor envergadura e peso em média. Em relação a tamanho os pinguins cirrhat e corriculata tem média parecida e os penguins da espécie arctica tem média abaixo de altura.

#(d) Construa um modelo de arvore de decisao usando estruturas condicionais para prever a especie do papagaio a partir das outras variaveis. Em seguida, calcule a sua taxa de acerto para o modelo construido.

Papagaios <- papagaio_data[sample(nrow(papagaio_data)), ]

n <- round(0.8 * nrow(Papagaios))

treinamento <- Papagaios[1:n, ]
teste <- Papagaios[-(1:n), ]

resultados <- c() # vetor para armazenar as previsões do modelo

for (i in 1:nrow(teste)) {
  tamanho <- teste$tamanho[i]
  peso <- teste$peso[i]
  envergadura <- teste$envergadura[i]
  
  if (tamanho < 35) {
    if (peso < 600) {
      resultados[i] <- "arctica"
    } else {
      resultados[i] <- "cirrhata"
    }
  } else if (tamanho < 45) {
    if (peso < 800) {
      resultados[i] <- "corniculata"
    } else {
      resultados[i] <- "cirrhata"
    }
  } else {
    if (envergadura < 60) {
      resultados[i] <- "corniculata"
    } else {
      resultados[i] <- "cirrhata"
    }
  }
}

mean(teste$especie == resultados) # acurácia do modelo

#(e) Construa agora um modelo knn para prever a especie. Voce pode usar a funcao pronta para o knn. Compare a taxa de acerto deste modelo com a arvore de decisao.

install.packages("class")
library(class)

Papagaios <- papagaio_data[sample(nrow(papagaio_data)), ]

n <- round(0.8 * nrow(Papagaios))

treinamento <- Papagaios[1:n, ]
teste <- Papagaios[-(1:n), ]

resultados <- c() # vetor para armazenar as previsões do modelo

# Preparar os dados
treinamento$especie <- as.factor(treinamento$especie)
teste$especie <- as.factor(teste$especie)

# Definindo as variáveis preditoras e a variável resposta
X_treinamento <- treinamento[, c("tamanho", "peso", "envergadura")] # ajuste conforme necessário
y_treinamento <- treinamento$especie

X_teste <- teste[, c("tamanho", "peso", "envergadura")]
y_teste <- teste$especie

# Modelo KNN
k <- 3 # você pode ajustar o valor de k
resultados_knn <- knn(X_treinamento, X_teste, y_treinamento, k)

# Cálculo da acurácia do modelo KNN
acuracia_knn <- mean(y_teste == resultados_knn)

# Cálculo da acurácia do modelo de árvore de decisão (caso você tenha um modelo já treinado)
library(rpart)

# Treinando a árvore de decisão
modelo_arvore <- rpart(especie ~ tamanho + peso + envergadura, data = treinamento)
resultados_arvore <- predict(modelo_arvore, teste, type = "class")

# Cálculo da acurácia do modelo de árvore de decisão
mean(y_teste == resultados_arvore)

