#REVISÃO P1 - Pedro Franklin

#1)

#Considerando 1 <- cara e 2 <- coroa
juju <- 18
jorel <- 7

while(jorel != 0 && juju != 0){
  moeda <- sample(1:2,1,FALSE)
  
  if(moeda == 1){
    juju <- juju + 1
    jorel <- jorel - 1
  } else {
    juju <- juju - 1
    jorel <- jorel + 1
  }
  
  if(jorel == 0){
    print("juju")
  } else {
    if(juju == 0){
      print("jorel")
    }
  }
  
}

resultados_jogo <- c()

for(i in 1:10000){
  
  juju <- 18
  jorel <- 7
  
  while(jorel != 0 && juju != 0){
    moeda <- sample(1:2,1,FALSE)
    
    if(moeda == 1){
      juju <- juju + 1
      jorel <- jorel - 1
    } else {
      juju <- juju - 1
      jorel <- jorel + 1
    }
    
    if(jorel == 0){
      resultados_jogo[i] <- "juju"
    } else {
      if(juju == 0){
        resultados_jogo[i] <- "jorel"
      }
    }
    
  }

}

#Para calcular a média, fazemos:

mean(resultados_jogo == "juju")
mean(resultados_jogo == "jorel")


#2)

bilhetes <- function(){
  
  bilhetes_completos <- c(1:30)
  
  for(i in 1:30){
    bilhetes_completos[i] <- 0
  }
  
  n <- 0
  
  while(sum(bilhetes_completos) < 30){
    numero <- sample(1:30,1,FALSE)
    n <- n+1
    bilhetes_completos[numero] <- 1
  }
  
  return (n)
  
}

resultados <- c()

for(j in 1:10000){
  resultados[j] <- bilhetes()
}

mean(resultados)


#3)

chicago <- read.csv("chicago.csv",header = TRUE)

chicago <- chicago[,-1]

summary(chicago)
str(chicago)

0-#a)

library(ggplot2)

soma_cardio <- sum(chicago$cvd)

inverno <- chicago[chicago$season == "Winter",]
verao <- chicago[chicago$season == "Summer",]
outono <- chicago[chicago$season == "Autumn",]
primavera <- chicago[chicago$season == "Spring",]

soma_inv <- sum(inverno$cvd)
soma_ver <- sum(verao$cvd)
soma_out <- sum(outono$cvd)
soma_pri <- sum(primavera$cvd)

#Com base nisso, podemos notar que no inverno tivemos mais mortes cardiovasculares, e no verão
# o menor número...

#b)

novesete <- chicago[chicago$year == 1997,]
noveoito <- chicago[chicago$year == 1998,]
novenove <- chicago[chicago$year == 1999,] 
doismil <- chicago[chicago$year == 2000,]

soma_novesete <- sum(novesete$resp)
soma_noveoito <- sum(noveoito$resp)
soma_novenove <- sum(novenove$resp)
soma_doismil <-  sum(doismil$resp)

#Por meio disso, podemos notar que o ano que mais teve mortes por doenças respiratórias foi
# 1999

#c)

media_inv <- mean(inverno$temp)
media_ver <- mean(verao$temp)
media_out <- mean(outono$temp)
media_prim <- mean(primavera$temp)

media_umi_inv <- mean(primavera$rhum, na.rm = TRUE)
media_umi_ver <- mean(verao$rhum, na.rm = TRUE)
media_umi_out <- mean(outono$rhum,  na.rm = TRUE)
media_umi_prim <- mean(primavera$rhum,  na.rm = TRUE)

#na.rm utilizado para ignorar os valores NA
#poderíamos ter utilizado na.omit também, mas daria bem mais trabalho.

ggplot(chicago, aes(x = temp))+
  geom_boxplot()+
  theme_minimal()+
  facet_wrap(~season) #Separa pela estação


#d)

ggplot(chicago, aes(x = time, y = temp , color = season))+
  geom_point()+
  theme_minimal()

#4)

papagaio <- read.table("papagaio.txt",header = TRUE, sep = ",")

#a)

str(papagaio)
summary(papagaio)
head(papagaio)
tail(papagaio)

#b)
ggplot(papagaio, aes(x = especie))+
  geom_bar(fill = "darkgreen")+
  theme_minimal()


#c)

ggplot(papagaio, aes(x = tamanho, y = peso, color = especie))+
  geom_point()+
  theme_minimal()

ggplot(papagaio, aes(x = peso, y = envergadura, color = especie))+
  geom_point()+
  theme_minimal()

ggplot(papagaio, aes(x = tamanho, y = envergadura, color = especie))+
  geom_point()+
  theme_minimal()

ggplot(papagaio, aes(x = especie, y = peso))+
  geom_boxplot()+
  theme_minimal()

#Basicamente, podemos por meio desse gráficos notar algumas diferenças significativas entre as 
#espécies de papagaios. A espécie cirrhata é mais pesada em média do que as outras duas espécies
#além de ter algumas aparições com envergaduras bem mais consideráveis. Já a espécie artica se consagra
#como a mais leve e com envergadura parecida com a corniculata. Em sua maioria, as espécies
#se diferenciam mais em peso do que qualquer outra coisa, e alguns possuem características bem parecidas.

#d) ARVORE DE DECISÃO

papagaio <- na.omit(papagaio)

n <- round(0.8*nrow(papagaio))

indices_treino <- sample(1:nrow(papagaio),n,FALSE)

treinamento <- papagaio[indices_treino,]
teste <- papagaio[-indices_treino,]

ggplot(treinamento, aes(x = tamanho, y = peso, color = especie))+
  geom_point()+
  theme_minimal()

resultado <- c()

for(i in 1:nrow(teste)){
  
  if(teste$peso[i]>800){
    resultado[i] <- "cirrhata" 
  } else {
    if(teste$peso[i]<440){
      resultado[i] <- "artica"
    } else{
      if(teste$tamanho[i]>32 && teste$peso[i]<800){
        resultado[i] <- "corniculata"
      } else {
        if(teste$peso[i]>400 & teste$peso[i]<600){
          resultado[i] <- "corniculata"
        } else {
          if(teste$envergadura[i]>70){
            resultado[i] <- "cirrhata"
          }
        }
      }
    }
  }
  
}

mean(teste$especie == resultado)


#e)

library(class)

treinamento_padronizado <- treinamento[,-4]
teste_padronizado <- teste[,-4]

#padronizar é tirar tudo que não é número
#as nossas classificações ficam em classe

classe_treinamento <- treinamento$especie
classe_teste <- treinamento$especie

modelo1 <- knn(train = treinamento_padronizado,test = teste_padronizado,cl = classe_treinamento,k = 10)

mean(teste$especie == modelo1)

#O modelo knn teve eficácia muito melhor do que o da árvore de decisão, atingindo 70% de acerto
#em comparação com praticamente 50% da árvore















