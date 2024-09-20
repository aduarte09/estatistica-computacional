# exercício 01

vet_a <- c(10:30)                  
vet_b <- c(30:10)                  
vet_c <- c(vet_a, vet_b[-1])    

vet_a    # 10, 11, 12, ..., 30
vet_b    # 30, 29, 28, ..., 10
vet_c    # 10, 11, 12, ..., 30, 29, 28, ..., 10



# exercício 02

?rep     # replicate elements of vectors and lists
?seq     # sequence generation

vet_a <- seq(2, 8, by = 2)
vet_a <- rep(vet_a, times = 10)

vet_b <- seq(2, 8, by = 2)
vet_b <- rep(vet_b, times = 10, length.out = 41)

vet_a    # 2, 4, 6, 8, ..., 2, 4, 6, 8
vet_b    # 2, 4, 6, 8, ..., 2, 4, 6, 8, 2



# exercício 03

soma_a <- c(20:30)
soma_a <- (soma_a ** 2) + (4 * soma_a)
soma_a <- sum(soma_a)

soma_b <- c(10:20)
soma_b <- ((3 ** soma_b) / soma_b) + ((2 ** soma_b) / (soma_b ** 2))
soma_b <- sum(soma_b)

soma_a    # 8085
soma_b    # 268814233.23813



# exercício 04

bolas <- sample(x = 1:100, size = 40, replace = TRUE)
bolas

bolas_pares <- length(bolas[bolas %% 2 == 0])
bolas_maiores_70 <- length(bolas[bolas > 70])
bolas_posicoes_impar <- which(bolas %% 2 != 0)

bolas_pares             # quantas bolas pares foram sorteadas?
bolas_maiores_70        # quantas bolas maiores que 70 foram sorteadas?
bolas_posicoes_impar    # em que posições estão as bolas ímpares?



# exercício 05

lancar_dado <- function() {
  obtem_4 <- 0
  tentativas <- 0
  while(obtem_4 < 2) {
    dado <- sample(x = 1:6, size = 1, replace = TRUE)
    if(dado == 4) {
      obtem_4 <- obtem_4 + 1
    }
    tentativas <- tentativas + 1
  }
  return(tentativas)
}

tentativas <- lancar_dado()
tentativas    # quantas tentativas foram necessárias?



# exercício 06

quantidades <- c()

for(i in 1:10000) {
  tentativas <- lancar_dado()
  quantidades <- c(quantidades, tentativas)
}

media_quantidades <- mean(quantidades)
media_quantidades

# A média indica que são necessários, aproximadamente, 12 lançamentos para obter o número 4 duas vezes.



# exercício 07

fibonacci <- function(n) {
  if(n >= 3) {
    sequencia <- c(1, 1)
    n <- n - 2   # os dois primeiros de n elementos já estão definidos
    num_a <- 1
    num_b <- 1
    for(i in 1:n) {
      soma <- num_a + num_b
      sequencia <- c(sequencia, soma)
      num_a <- num_b 
      num_b <- soma
    }
    return(sequencia)
  } else {
    return("n precisa ser maior ou igual a 3")
  }
}

n <- 10      # defina aqui o valor de n
sequencia <- fibonacci(n)
sequencia    # sequência de fibonacci obtida a partir da função



# exercício 08

sorteios <- c()

for(i in 1:100000) {
  amigo_oculto <- sample(x = 1:5, size = 5, replace = FALSE)
  deu_errado <- any(amigo_oculto == 1:5)
  sorteios[i] <- as.integer(deu_errado)
}

proporcao_erro <- mean(sorteios)
proporcao_erro    # proporção de vezes em que o sorteio deu errado



# exercício 09

jogar_craps <- function() {
  dado_a <- sample(x = 1:6, size = 1, replace = TRUE)
  dado_b <- sample(x = 1:6, size = 1, replace = TRUE)
  soma <- dado_a + dado_b
  
  if((soma == 7) | (soma == 11)) {
    return(1)       # ganhou
  }
  if((soma == 2) | (soma == 3) | (soma == 12)) {
    return(0)       # perdeu
  }
  
  nova_soma <- 0
  
  while((nova_soma != 7) & (nova_soma != soma)) {
    dado_a <- sample(x = 1:6, size = 1, replace = TRUE)
    dado_b <- sample(x = 1:6, size = 1, replace = TRUE)
    nova_soma <- dado_a + dado_b
    
    if(nova_soma == 7) {
      return(0)     # perdeu
    }
    if(nova_soma == soma) {
      return(1)     # ganhou
    }
  }
}

ganhou <- c()

for(i in 1:100000) {
  ganhou[i] <- jogar_craps()
}

proporcao_vitoria <- mean(ganhou)
proporcao_vitoria   # proporção de vitórias no jogo



# exercício 10

# ex.10 - letra A
passeio_aleatorio <- function(L) {
  if((L > 0) & (L < 20)) {
    
    while((L != 0) & (L != 20)) {
      moeda <- sample(x = 1:2, size = 1, replace = TRUE)
      # observação: 1 = coroa, 2 = cara
      
      if(moeda == 1) {
        L <- L - 1    # um passo para a esquerda
      }
      else {
        L <- L + 1    # um passo para a direita
      }
    }
    
    if(L == 20) {
      return(1)       # Luke Skywalker terminou o passeio em casa
    } else {
      return(0)       # Luke Skywalker caiu no precipício
    }

  } else {
    return("L precisa ser maior que zero e menor que 20")
  }
}

L <- 10               # defina aqui o valor de L
fim_passeio <- passeio_aleatorio(L)
fim_passeio           # como o passeio terminou?

# ex.10 - letra B
replicar_passeio_aleatorio <- function(L) {
  resultados <- c()
  for(i in 1:10000) {
    fim_passeio <- passeio_aleatorio(L)
    resultados[i] <- fim_passeio
  }
  proporcao_casa <- mean(resultados)
  return(proporcao_casa)
}

L <- 5                 # defina aqui o valor de L
proporcao_casa <- replicar_passeio_aleatorio(L)
proporcao_casa         # propoção de vezes que Luke chegou em casa

# ex.10 - letra C
proporcao_L <- c()

for(L in 1:19) {
  proporcao_L[L] <- replicar_passeio_aleatorio(L)
}

proporcao_L

?barplot
barplot(height = proporcao_L, names.arg = 1:19,
        main = "Proporção de vezes que Luke chegou em casa",
        xlab = "Posição inicial", ylab = "Proporção")



# exercício 11

# ex.11 - letra A
passeio_exploracao <- function() {
  posicao <- c(0, 0)
  for(i in 1:8) {
    dado <- sample(x = 1:4, size = 1, replace = TRUE)
    # observação: L = 1, R = 2, U = 3, D = 4
    
    if(dado == 1) {
      posicao[1] <- posicao[1] - 1   # move uma casa para a esquerda
    }
    if(dado == 2) {
      posicao[1] <- posicao[1] + 1   # move uma casa para a direita
    }
    if(dado == 3) { 
      posicao[2] <- posicao[2] + 1   # move uma casa para cima
    }
    if(dado == 4) {
      posicao[2] <- posicao[2] - 1   # move uma casa para baixo
    }
  }
  return(posicao)
}

posicao <- passeio_exploracao()
posicao

# ex.11 - letra B
retornou_origem <- c()
origem <- c(0, 0)

for(i in 1:10000) {
  posicao <- passeio_exploracao()
  if(identical(posicao, origem)) {
    retornou_origem <- c(retornou_origem, 1)
  } else {
    retornou_origem <- c(retornou_origem, 0)
  }
}

proporcao_origem <- mean(retornou_origem)
proporcao_origem

# As chances de Link voltar para a origem a cada 8 lançamentos do dado é de, aproximadamente, 7%.

# ex.11 - letra C
?paste             # cobcatenate strings
?all               # are all values true?

passeio_N <- function(N) {
  if(N %% 2 == 0) {
    for(i in 1:N) {
      posicao <- passeio_exploracao()
      if(all(posicao == origem)) {
        retornou_origem <- c(retornou_origem, 1)
      } else {
        retornou_origem <- c(retornou_origem, 0)
      }
    }
    proporcao_origem <- mean(retornou_origem)
    return(paste("Proporção de vezes em que Link retornou à origem:",
                 proporcao_origem))
  } else {
    return("impossível retornar a origem depois de um número ímpar de passos")
  }
}

N <- 50             # defina aqui o valor de N
proporcao_origem <- passeio_N(N)
proporcao_origem    # com base em N lançamentos



# exercíco 12

jogar_moeda <- function() {
  steven <- c(0, 1, 0)
  garnet <- c(0, 0, 1)
  moeda <- sample(x = 0:1, size = 3, replace = TRUE) 
  # observação: coroa = 0, cara = 1
  
  while(TRUE) {
    if(all(moeda == steven)) {
      return("steven")
    }
    else if(all(moeda == garnet)) {
      return("garnet")
    } else {
      novo_lancamento <- sample(x = 0:1, size = 1, replace = TRUE)
      moeda <- c(moeda[-1], novo_lancamento)
    }
  }
}

ganhador <- jogar_moeda()
ganhador            # quem ganhou?

resultados <- c()

for(i in 1:10000) {
  resultados[i] <- jogar_moeda()
}

proporcao_garnet <- mean(resultados == "garnet")
proporcao_garnet    

# A média de vitórias da Garnet é um valor próximo de 67%, ou seja, a sequência escolhida por ela é matematicamente mais provável de acontecer em relação a sequência do Steven.



# exercício 13
library(ggplot2)

dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")
str(dados)

# ex.13 - letra A  
ggplot(data = dados, mapping = aes(x = Genero)) +
  geom_bar()

# É possível notar que a maioria das vítimas eram mulheres, com uma diferença de frequência bem discrepante em relação às vítimas que eram homens.

# ex.13 - letra B
ggplot(data = dados, mapping = aes(x = Idade)) + 
  geom_histogram(bins = 8) +
  facet_wrap(~Genero)

# O gráfico indica que a maioria das vítimas tinha entre 70 e 85 anos, independente de seu gênero.

# ex.13 - letra C
ggplot(data = dados, mapping = aes(y = Idade)) +
  geom_boxplot()

# O boxplot mostra que a maioria das vítimas tinha entre 70 e 85 anos, e, além disso, também é possível perceber que as vítimas mais novas tinham cerca de 55 anos, enquanto as mais velhas, 95. Poucas vítimas tinham menos que 55 anos.

# ex.13 - letra D
ggplot(data = dados, mapping = aes(x = LocalDaMorte)) +
  geom_bar()

# A maioria das vítimas, mais de 200, foram assassinadas em suas casas. Cerca de 10 morreram no hospital, enquanto aproximadamente 5 morreram na casa de repouso.

# ex.13 - letra E
ggplot(data = dados, mapping = aes(x = AnoDaMorte)) +
  geom_bar()

# O número de assassinatos foi crescendo conforme o passar das décadas, sendo a década de 1990 a que contém o maior número de casos. No ano de 1997, houve mais de 40 vítimas.

# ex.13 - letra F
# A maioria das vítimas de Harold Shipman eram idosos com idades entre 70 e 85 anos, sobretudo mulheres. Shipman geralmente cometia os assassinatos nas casas das vítimas. Além disso, entre as décadas de 1970 e 1990, houve um aumento significativo no número de casos, sendo 1997 o ano com o maior número de vítimas registrado.



# exercício 14
library(ggplot2)

# ex.14 - letra A
primatas <- read.table(file = "primatas.txt", header = TRUE, sep = ":")
str(primatas)
summary(primatas)

# ex.14 - letra B
ggplot(data = primatas, mapping = aes(x = especie)) +
  geom_bar()

ggplot(data = primatas, mapping = aes(x = genero)) +
  geom_bar() +
  facet_wrap(~especie)

# ex.14 - letra C
bonobos <- primatas[primatas$especie == "bonobo",]
ggplot(data = bonobos, mapping = aes(x = altura, y = peso, col = genero)) +
  geom_point()

chimpanzes <- primatas[primatas$especie == "chimpanze",]
ggplot(data = chimpanzes, mapping = aes(x = altura, y = peso, col = genero)) + 
  geom_point()

# ex.14 - letra D
femeas <- primatas[primatas$genero == "femea",]
ggplot(data = femeas, mapping = aes(x = altura, y = peso, col = especie)) +
  geom_point()

machos <- primatas[primatas$genero == "macho",]
ggplot(data = machos, mapping = aes(x = altura, y = peso, col = especie)) +
  geom_point()

# ex.14 - letra E
# A partir da análise dos gráficos, foi possível perceber que, independente da espécie, as fêmeas são menores e mais leves que os machos, sendo essa diferença bem mais acentuada entre os chimpanzés. Também é perceptível que os machos da espécie bonobo possuem peso semelhante às fêmeas da espécie chimpanzé, apesar de serem ligeiramente mais altos que essas fêmeas.

# ex.14 - letra F
primatas <- primatas[sample(nrow(primatas)),]
n <- round(0.8 * nrow(primatas))

treino_primatas <- primatas[(1:n),]
teste_primatas <- primatas[-(1:n),]

ggplot(data = treino_primatas, 
       mapping = aes(x = altura, y = peso, col = genero)) +
  geom_point() +
  facet_wrap(~especie)

resultados <- c()

for(i in 1:nrow(teste_primatas)) {
  if(all(teste_primatas$genero[i] == "macho")) {
    if(teste_primatas$peso[i] >= 55) {
      resultados[i] <- "chimpanze"
    } else {
      resultados[i] <- "bonobo"
    }
  } else {
    if((teste_primatas$altura[i] > 122) & (teste_primatas$peso[i] <= 35)) {
      resultados[i] <- "bonobo"
    } else {
      resultados[i] <- "chimpanze"
    }
  }
}

proporcao_acertos <- mean(teste_primatas$especie == resultados)
proporcao_acertos
