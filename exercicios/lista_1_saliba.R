#Lista 1 - Estatística Computacional
#Matheus Fiod Saliba - 12221BCC024

#Exercício 1)

#a) 
vet1 <- c(10:30)

#b)
vet2 <- c(30:10)

#c)
vet3 <- c(c(10:30),c(29:10))

#Exercício 2)

?rep #Criação de repetições
?seq #Criação de sequências

#a)

rep(c(seq(2,8,by=2)), 10)
#Repetição 10 vezes de um mesmo vetor que vai de 2 até 8 de 2 em 2

#b)

rep(c(seq(2,8,by=2),2), 10)
#Adicionei um 2 no final no vetor

#Exercício 3)

#a)

a <- c(20:30)

resultado <- (a^2) + (4*a)

soma <- sum(resultado)

#b)

b <- c(10:20)

resultado1 <- ((3^b)/b) + ((2^b)/(b^2))

soma1 <- sum(resultado1)

#Exercício 4)

#a)

sorteio <- sample(1:100,40, replace = TRUE)

num_par <- (sorteio%%2 == 0)
#Define com valores lógicos

soma_par <- sum(num_par)
#Usa 0 e 1 de TRUE e FALSE para calcular quantidade

#b)

num_70 <- (sorteio > 70)

soma_70 <- sum(num_70)

#c)

num_impar <- (sorteio%%2 != 0)

posicoes_impar <- c(which(num_impar))
#Função which retorna posições do vetor que são TRUE

#Exercício 5)

lancamento <- function(){
  
  tentativas <- 0
  i <- 0
  
  while(i<2) {
    
    dado <- sample(1:6,1,TRUE)
    #Dado jogado uma vez por iteração
    
    if(dado == 4) {
      i <- i+1
    }
    #Se o dado for 4, aumenta índice i em 1 unidade
    
    tentativas <- tentativas + 1
    #Número de tentativas
  }
  
  return(tentativas)
}

resultado <- lancamento()
print(resultado)

#Exercício 6

quantidades <- c()

for(i in 1:10000) {
  
  resultado <- lancamento()
  quantidades[i] <- resultado
  
}

media_lanc <- mean(quantidades)

#Ao testar várias vezes, a média de lançamentos necessários
#para retirar dois números (4) está entre 11 e 12.

#Exercício 7

fibonacci <- function(n){
  
  if(n<=2){
  print('Número inválido, digite um maior ou igual a três(3)')
    return(0)
  }
  
  fibo <- c(1,1)
  #Criando vetor com os dois primeiros números de sempre
  
  for(i in 3:n){
    
    fibo[i] <- fibo[i-1] + fibo[i-2]
    
  }
  return(fibo)
  
}

resul_fibo <- fibonacci(10)

#Exercício 8
#Sorteio de amigo oculto

#Considerando que Michael seja a posição 1 do vetor,
#Dwight a 2 e assim por diante, temos:

amigo_oculto <- function(){
  
  sorteios <- sample(1:5,5,replace = FALSE)

  for(i in 1:5){
  
    if(sorteios[i] == i){
      return(0)
    }
  }
  
  return(1)

}

sucessofracasso <- amigo_oculto()
#Resultado do amigo oculto

vetor_amigo <- c()
#Criar um vetor para armazenar todos os resultados, 
#e depois saber quantos deram errado

for(i in 1:100000){
  
  vetor_amigo[i] <- amigo_oculto()
  
}

soma_acertos <- sum(vetor_amigo)
soma_fracassos <- 100000 - soma_acertos

proporcao_fracasso <- soma_fracassos/100000
#Nos dá a proporção de fracassos sob o número total de realizações
#do amigo oculto :)

#Exercício 9
#Considerando 0 - derrota, e 1 - vitória

vetcraps <- c()

craps <- function(){
  
  vetcraps <- sample(1:6,2,replace = TRUE)
  
  somainicial <- vetcraps[1] + vetcraps[2]
  
  if(somainicial == 7 || somainicial == 11){
    return(1)
  } else {
    if(somainicial == 2 || somainicial == 3 || somainicial == 12){
      return(0)
    } else {
      novasoma <- 0
      while(novasoma != 7 || novasoma != somainicial){
        vetcraps <- sample(1:6,2,replace = TRUE)
        
        novasoma <- vetcraps[1] + vetcraps[2]
        
        if(novasoma == 7){
          return(0)
        } else {
          if(novasoma == somainicial){
            return(1)
          }
        }
      }
    }
  }
}

vetor_craps <- c()

for(i in 1:100000){
  
  vetor_craps[i] <- craps()
  
}

vetsucesso <- sum(vetor_craps)

proporcao_sucesso <- vetsucesso / 100000

#Exercício 10

#a)

passeio <- function(L){
  
  if(L<0 || L>20){
    print('Digite um número válido.')
    return -1;
  }
  
  while(L != 0 || L != 20){
    
    #Considerando cara = 1 e coroa = 2
    moeda <- sample(1:2,1,FALSE)
    
    if(moeda == 1){
      L <- L+1
    } else {
        L <- L-1
    }
    
    if(L == 20){
      return(1) #Luke chega em casa
    } else {
      if(L == 0){
        return(0) #Luke cai no precipício
      }
    }
    
    
  }
}

resultado_luke <- passeio(5)


#b)

passeio_10k <- function(L){
  
  vet_luke <- c()
  
  for(i in 1:10000){
    
    vet_luke[i] <- passeio(L)
    
  }
  
  acertos_luke <- sum(vet_luke)
  proporcao_luke <- acertos_luke / 10000
  
  return(proporcao_luke)
  
}

teste <- passeio_10k(10)

#c)

library(ggplot2)

proporcoes_luke <- c()

for(i in 1:19){
  proporcoes_luke[i] <- passeio_10k(i)
}

dados_luke <- data.frame(x = 1:19, y = proporcoes_luke)

ggplot(dados_luke, aes(x = as.factor(x), y = y)) +
  geom_bar(stat = "identity") +
  labs(x = "Eixo X", y = "Eixo Y", title = "Proporções de sucesso de Luke")

#Exercício 11

#a)

x <- 0
y <- 0 
# (x,y) começando no ponto (0,0)

link <- function(){
  
  for(i in 1:8){
    
    direcao <- sample(1:4,1,FALSE)
    #Considerando 1 = L, 2 = R, 3 = U e 4 = D
    
    if(direcao == 1){
      x <- x - 1
      y <- y
    } else {
      if(direcao == 2){
        x <- x + 1
        y <- y
      } else {
        if(direcao == 3){
          x <- x
          y <- y + 1
        } else {
          if(direcao == 4){
            x <- x
            y <- y - 1
          }
        }
      }
    }
  }
  
  return(c(x,y))
  
}

# ponto será o vetor que armazenará a posição 1(x) e 2(y)
ponto <- link()
ponto


#b)

vet_link <- c()
#Vetor vai armazenar em cada posição, uma sequência de jogadas 8 vezes
#Se Link voltar para o (0,0), posição será preenchida com valor 1, se não, valor 0

x <- 0
y <- 0

for(i in 1:10000){
  
  ponto <- link()
  
  if(ponto[1] == 0 && ponto[2] == 0){
    vet_link[i] <- 1
  } else {
    vet_link[i] <- 0
  }
  
  x <- 0
  y <- 0
}

origem_link <- sum(vet_link)
prop_link <- origem_link / 10000
prop_link
#Proporção de vezes que ele voltou para a origem em relação ao total de passeios/jornadas feitas
#Essa proporção é extremamente baixa, porque poucos conjuntos de movimentos permitem esse resultado
#Sempre próximo de 0.070 até 0.080



#c)

x <- 0
y <- 0

link_2 <- function(n){
  
  vet_link2 <- c()
  
  if(n%%2 != 0){
    print('Impossível retornar a origem depois de um número ímpar de passos, insira um número par')
    return (-1)
  }
  
  for(i in 1:10000){
    
    x <- 0
    y <- 0
    
    for(j in 1:n){
      
      direcao2 <- sample(1:4,1,FALSE)
      
      if(direcao2 == 1){
        x <- x - 1
        y <- y
      } else {
        if(direcao2 == 2){
          x <- x + 1
          y <- y
        } else {
          if(direcao2 == 3){
            x <- x
            y <- y + 1
          } else {
            if(direcao2 == 4){
              x <- x
              y <- y - 1
            }
          }
        }
      }
    }
    
    if(x == 0 && y == 0) {
      vet_link2[i] <- 1
    } else {
      vet_link2[i] <- 0
    }
    
  }
  
  origem_link2 <- sum(vet_link2)
  prop_link2 <- origem_link2 / 10000
  
  return(prop_link2)
  
}

proporcao_teste <- link_2(16)

#Exercício 12

jogo_seq <- function() {
  
  steven <- c(0,1,0)
  garnit <- c(0,0,1)
  sequencia <- c()
  
  fim <- 0
  
  # Cara = 1, coroa = 0
  # Sorteio inicial
  for(i in 1:3){
    sequencia[i] <- sample(0:1,1,FALSE)
  }
  
  v1 <- sequencia == steven
  v2 <- sequencia == garnit
  
  if(sum(v1) == 3){
    return('steven')
  } else {
    if(sum(v2 == 3)){
      return('garnit')
    }
  }
  
  #Pode ser usado while(TRUE)
  while(fim != 1){
    
    sequencia[1] <- sequencia[2]
    sequencia[2] <- sequencia[3]
    
    sequencia[3] <- sample(0:1,1,FALSE)
    
    v1 <- sequencia == steven
    v2 <- sequencia == garnit
    
    if(sum(v1) == 3){
      fim <- 1
      return('steven')
    } else {
      if(sum(v2) == 3){
        fim <- 1
        return('garnit')
      }
    }
  }
  
  
}

#Replicando 10 mil vezes e vendo média de resultados de garnit
#Fiz uma função para facilitar o teste :)
garnit_10k <- function(){
  vet_sg <- c()
  
  for(i in 1:10000){
    
    vet_sg[i] <- jogo_seq()
    
  }
  
  vitoria_garnit <- (vet_sg == 'garnit')
  num_garnit <- sum(vitoria_garnit)
  
  prop_garnit <- num_garnit / 10000
  
  return(prop_garnit)
  
  #Ao realizar a média do número de vitória de garnit, percebemos que ela na maioria
  #das vezes se encontra entre 0.57 e 0.58, ganhando mais que 50% dos jogos feitos.
}

proporcao_sg <- garnit_10k()

#Exercício 13

library(ggplot2)

dados <- read.table(file = "dados.txt", sep = ";", header = TRUE)

#a) O melhor gráfico para representar a diferença das frequências de gênero
#   é o gráfico de barras, onde conseguimos perceber que Harold matou um número
#   considerável a mais de mulheres dentro da sua série de assassinatos.

dados$Genero <- as.factor(dados$Genero)

ggplot(dados, aes(x = Genero))+
  geom_bar(color="blue", fill = "grey")+
  theme_minimal()+
  labs(x = "Gênero", y = "Quantidade")

#b) Por meio desses dois histogramas, podemos perceber que Harold matava tanto homens
#   quanto mulheres com maior idade. Harold matou muitas mulheres entre 70-85 anos,além
#   de homens da mesma faixa de idade, mesmo que com menor quantidade.

ggplot(dados, aes(x = Idade))+
  geom_histogram(bins = 8,color = "blue",fill = "grey")+
  theme_minimal()+
  facet_wrap(~Genero)+
  labs(x = "Idades", y = "Quantidade")
#Facet_wrap permite visualização por gênero


#c) Com base no boxplot da idade, podemos entender que temos diversos outliers,
#   o que representa que Harold matou algumas pessoas com idade muito menor do que 
#   o comum de suas vítimas. Podemos ainda reforçar o fato que a idade média de suas
#   vítimas foi de 70-80 anos.

ggplot(dados, aes(x = Idade))+
  geom_boxplot(color = "blue", fill = "grey")+
  theme_minimal()+
  labs(x = "Idade")

#d) Um simples gráfico de barras consegue nos mostrar que Harold matou a grande
#   maioria de suas vítimas dentro de suas próprias casas. Poucos casos aconteceram
#   nas outras localizações contidas no conjunto de dados (hospital e casa de enfermagem).

ggplot(dados,aes(x = LocalDaMorte))+
  geom_bar(color = "blue", fill = "grey")+
  theme_minimal()+
  labs(x = "Locais de morte das vítimas", y = "Quantidade")

#e) Pelo gráfico de barras, podemos concluir que Harold matou a maioria de suas vítimas
#   no ano de 1995 para frente, como 1996 e 1997

ggplot(dados, aes(x = AnoDaMorte))+
  geom_bar(color = "blue", fill = "grey")+
  theme_minimal()+
  labs(x = "Ano de morte das vítimas", y = "Quantidade")
  
#f) Basicamente, Harold tinha um padrão bem comum de suas vítimas, sendo elas majoritariamente
#   mulheres, entre seus 70 até 85 anos. Ele costumava realizar seus crimes sempre dentro
#   das próprias casas das pessoas, a maioria deles sendo realizados entre 1995-1996.


#Exercício 14

library(ggplot2)

#a)
primatas <- read.table(file = "primatas.txt", sep = ":", header = TRUE)

summary(primatas)
#Função summary utilizada para mostrar mais detalhes sobre a tabela

#b)

primatas$especie <- as.factor(primatas$especie)
# Com esse comando e com o comando summary, podemos perceber que a quantidade
# de ambas as espécies são iguais...

ggplot(primatas, aes(x = especie))+
  geom_bar(color = "blue", fill = "grey")+
  theme_minimal()+
  labs(x = "Espécies", y = "Quantidade")

primatas$genero <- as.factor(primatas$genero)

ggplot(primatas, aes(x = genero, fill = especie))+
  geom_bar(color = "blue", fill = "grey")+
  theme_minimal()+
  labs(x = "Gênero", y = "Quantidade")+
  facet_wrap(primatas$especie)

#A quantidade de fêmeas e machos também é igual.

#c) 

bonobos <- primatas[primatas$especie == "bonobo",]
chimpanzes <- primatas[primatas$especie == "chimpanze",]

ggplot(bonobos, aes(x = altura, y = peso, color = genero))+
  geom_point()+
  theme_minimal()+
  labs(title = "Comparação entre fêmeas e machos bonobo")

ggplot(chimpanzes, aes(x = altura, y = peso, color = genero))+
  geom_point()+
  theme_minimal()+
  labs(title = "Comparação entre fêmeas e machos chimpanzes")

#d)

femeas_total <- primatas[primatas$genero == "femea",]
machos_total <- primatas[primatas$genero == "macho",]

ggplot(femeas_total, aes(x = altura, y = peso, color = especie))+
  geom_point()+
  theme_minimal()+
  labs(title = "Comparação entre fêmeas bonobo-chimpanze")

ggplot(machos_total, aes(x = altura, y = peso, color = especie))+
  geom_point()+
  theme_minimal()+
  labs(title = "Comparação entre machos bonobo-chimpanze")

#e) Basicamente, pelo estudo dos gráficos, conseguimos entender que os bonobos possuem
#   diferenças consideráveis de peso e altura em relação ao gênero, onde as fêmeas são
#   bem mais leves e menores que os machos.Nos chimpanzes, essa diferença se torna ainda
#   mais considerável, onde os machos são muito mais pesados e altos do que as fêmeas.
#   Ao comparar as fêmeas das duas espécies, as fêmeas bonobo acabam sendo mais altas e
#   leves do que as fêmeas chimpanzes, que costumam ser mais pesadas e menores. Por outro
#   lado, na comparação entre machos, os chimpanzes dominam em questão de peso e altura em
#   relação aos bonobos.

#f)

n <- round(0.8*nrow(primatas))

indices_treino <- sample(1:nrow(primatas),n,FALSE)

treino <- primatas[indices_treino,]
teste <- primatas[-indices_treino,]

ggplot(treino, aes(x = altura, y = peso, color = especie))+
  geom_point()+
  theme_minimal()

#Gráfico para ajudar a determinar os valores da árvore de decisão

resultado <- c()

for(j in 1:nrow(teste)){
  if(teste$peso[j] > 52){
    resultado[j] <- "chimpanze"
  } else {
    if(teste$peso[j] >= 39 & teste$altura[j]>=128){
      resultado[j] <- "bonobo"
    } else {
      if(teste$peso[j] >= 39 & teste$altura[j]<=128){
        resultado[j] <- "chimpanze"
      } else {
        if(teste$peso[j] < 40){
          resultado[j] <- "bonobo"
        }
      }
    }
  }
}

mean(teste$especie == resultado)




































