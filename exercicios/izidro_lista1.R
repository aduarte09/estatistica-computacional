#Exercicio 1
#(a)
x <- c(10:30)
x

#(b)
y <- c(30:10)
y

#(c)
z <- c(x,y)
z


#Exercicio 2
?rep
#Vê os termos replicados em um vetor
?seq
#Gera sequencias regulares

#(a)
vet1 <- rep(seq(2, 8, by = 2), 10)
vet1

#(b)
vet2 <- c(vet1, 2)
vet2


#Exercicio 3
#(a)
n1 <- c(20:30)
func <- n1^2 + 4*n1
sum(func)

#(b)
n2 <- c(10:20)
func <- ((3^(n2)) / (n2)) + (2^(n2) / (n2^2))
sum(func)


#Exercicio 4
set.seed(1240)
sorteio <- sample (x =1:100 , size = 40, replace = TRUE)
sorteio

#(a) Quantas bolas pares foram sorteadas?
pares <- sorteio[sorteio%%2 == 0]
pares <- sort(pares)
pares
#R: 16 bolas pares foram sorteadas

#(b) Quantas bolas maiores do que 70 foram sorteadas?
maiores_70 <- sorteio[sorteio > 70]
maiores_70
#R: 12 bolas maiores que 70 foram sorteadas

#(c) Em quais retiradas (posicoes) foram sorteadas as bolas impares?
impares <- which(sorteio %% 2 == 1)
impares
#R: Nas posições 4  5  7  9 10 12 13 14 16 18 20 22 24 26 27 28 29 30 31 33 35 36 37 39

#Exercicio 5
sorteio <- c()
cont <- 0
set.seed(1337)
while(cont < 2){
  jogada <- sample (x = 1:6, size = 1, replace = TRUE)
  sorteio <- c(sorteio,jogada)
  if(jogada == 4){
    cont <- cont + 1
  }
}
length(sorteio)


#exercicio 6
quantidades <- c()
set.seed(1337)
for (i in 1:10000) {
  sorteio <- c()
  cont <- 0
  while (cont < 2) {
    jogada <- sample(x = 1:6, size = 1, replace = TRUE)
    sorteio <- c(sorteio, jogada)
    if (jogada == 4) {
      cont <- cont + 1
    }
  }
  quantidades <- c(quantidades, length(sorteio))#numero de lançamentos
}
mean(quantidades)
#R: Cada experimento de aproximadamente 12 lançamentos para mostrar a face 4 duas vezes


#Exercicio 7
fibonacci <- function(n){
  if (n < 3){
    stop("Por favor, insira um valor de n maior ou igual a 3.")
  }
  fib_seq <- numeric(n)
  fib_seq[1] <- 1
  fib_seq[2] <- 1
  for (i in 3:n) {
    fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
  }
  return(fib_seq)
}
fibonacci(10)


#Exercicio 8
resultados <- c()
for(j in 1:10000){
    sorteio <- sample(x = 1:5, size = 5, replace = FALSE)
    if(any(sorteio == 1:5)){
      resultados[j] <- 0
    }else{
      resultados[j] <- 1
    }
}
resultados
proporcao <- sum(resultados == 0)/10000
proporcao


#exercicio 9
resultado <- c()
for(i in 1:100000){
  dado1 <- sample(x = 1:6, size = 1, replace = TRUE)
  dado2 <- sample(x = 1:6, size = 1, replace = TRUE)
  soma <- dado1 + dado2
  if(soma == 7 || soma == 11)
    resultado[i] <- 1
  if(soma == 2 || soma == 3 || soma == 12)
    resultado[i] <- 0
  nova_soma <- 0
  while((nova_soma != 7) & (nova_soma != soma)) {
    dado_a <- sample(x = 1:6, size = 1, replace = TRUE)
    dado_b <- sample(x = 1:6, size = 1, replace = TRUE)
    nova_soma <- dado_a + dado_b
    if(nova_soma == 7) {
      resultado[i] <- 0     # perdeu
    }
    if(nova_soma == soma) {
      resultado[i] <- 1     # ganhou
    }
  }
}
resultado <- sum(resultado == 1)/100000
resultado


#Exercicio 10
#(a)
moeda <- c("cara", "coroa")
n <- 20
precipicio <- 0
passeio <- function(L){
  if(L>precipicio & L<n){
    while((L!=precipicio) & (L!=n)){
      sorteio <- sample(moeda,size = 1)
      if(sorteio == "cara"){
        L <- L+1
      }else{
        L <- L-1
      }
    }
    if(L == n){
      print("Luke chegou em casa, sua jornada acabou! A mimir ZZZ")
      return(1)
    }else{
      print("Luke caiu no precipicio!!!")
      return (0)
    }
  }else{
    return("L precisa ser maior que zero e menor que 20")
  }
}
L <- 10               #defina aqui o valor de L
fim <- passeio(L)
fim

#(b)
passeio2 <- function(L){
  resultados <- c()
  for(j in 1:10000){
    moeda <- c("cara", "coroa")
    n <- 20
    precipicio <- 0
    passeio <- function(L){
      if(L>precipicio & L<n){
        while((L!=precipicio) & (L!=n)){
          sorteio <- sample(moeda,size = 1)
          if(sorteio == "cara"){
            L <- L+1
          }else{
            L <- L-1
          }
        }
        if(L == n)
          return(1)
        else
          return (0)
      }else{
        return("L precisa ser maior que zero e menor que 20")
      }
    }
    fim <- passeio(L)
    resultados[j] <- c(fim)
  }
  resultados <- mean(resultados)
  resultados
}
L <- 10               #defina aqui o valor de L
fim2 <- passeio2(L)
fim2

#(c)
proporcao_L <- c()
for(L in 1:19) {
  proporcao_L[L] <- passeio2(L)
}
proporcao_L[L]
barplot(height = proporcao_L, names.arg = 1:19,
        main = "Proporção de vezes que Luke chegou em casa",
        xlab = "Posição inicial", ylab = "Proporção")


#Exercicio 11
#(a)
explorar <- function(){
  posicao <- c(0,0)
  for(i in 1:8){
    dado <- c("L","R","U","D")
    lance <- sample(dado, 1, replace = TRUE)
    if(lance == "L"){
      posicao[1] <- posicao[1] - 1
    }
    if(lance == "R"){
      posicao[1] <- posicao[1] + 1
    }
    if(lance == "U"){
      posicao[2] <- posicao[2] + 1
    }
    if(lance == "D"){
      posicao[2] <- posicao[2] - 1
    }
  }
  return (posicao)
}
posicao <- explorar()
posicao

#(b)
proporcao3 <- c()
origem <- c(0,0)
for(j in 1:10000){
  posicao <- explorar()
  if(identical(posicao,origem)){
    proporcao3 <- c(proporcao3, 1)
  }else{
    proporcao3 <- c(proporcao3, 0)
  }
}
proporcao3 <- mean(proporcao3)
proporcao3

#R: Link tem chance de voltar ao ponto de partida cerca de 7% das 10000 tentativas

#(c)
func2 <- function(N) {
  if (N %% 2 == 0) {
    proporcao4 <- numeric()
    origem <- c(0, 0)
    for (i in 1:N) {
      posicao <- explorar()
      if (all(posicao == origem)) {
        proporcao4 <- c(proporcao4, 1)
      } else {
        proporcao4 <- c(proporcao4, 0)
      }
    }
    proporcao4 <- mean(proporcao4)
    return(paste("Link voltou à origem em", proporcao4*100, "% das vezes."))
  } else {
    return("Impossível retornar à origem pois o número de passos é ímpar.")
  }
}
N <- 20
proporcao4 <- func2(N)
proporcao4


#Exercicio 12
jogar_moeda <- function() {
  moeda <- c("cara", "coroa")
  steven <- c("cara", "coroa", "cara")
  garnet <- c("cara", "cara", "coroa")
  lance <- sample(moeda, size = 3, replace = TRUE) 
  while (TRUE) {
    if (all(lance == steven)) {
      return("steven")
    }else if (all(lance == garnet)) {
      return("garnet")
    }else{
      novo_lancamento <- sample(moeda, size = 1, replace = TRUE)
      lance <- c(lance[-1], novo_lancamento)
    }
  }
}
ganhador <- jogar_moeda()
ganhador
resultados <- character(10000)
for (i in 1:10000) {
  resultados[i] <- jogar_moeda()
}
proporcao_garnet <- mean(resultados == "garnet")
proporcao_garnet
#R: A média de vitórias da Garnet é aproximadamente 67%, ou seja, a sequência escolhida por ela é matematicamente mais provável de acontecer em relação a sequência do Steven.




#Exercicio 13
library(ggplot2)
dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")
str(dados)
#(a)
ggplot(data = dados, mapping = aes(x = Genero)) +
  geom_bar()
#Observa-se que a maioria das vítimas eram mulheres, com uma diferença de frequência consideravelmente maior em comparação com as vítimas do sexo masculino.

#(b)
ggplot(data = dados, mapping = aes(x = Idade)) + 
  geom_histogram(bins = 8) +
  facet_wrap(~Genero)
#O gráfico mostra que a maioria das vítimas estava na faixa etária de 70 a 85 anos, independentemente do gênero.

#(c)
ggplot(data = dados, mapping = aes(y = Idade)) +
  geom_boxplot()
#O boxplot revela que a maior parte das vítimas tinha entre 70 e 85 anos. Nota-se ainda que as vítimas mais jovens tinham aproximadamente 55 anos, enquanto as mais idosas tinham em torno de 95 anos. Poucas vítimas possuíam menos de 55 anos.

#(d)
ggplot(data = dados, mapping = aes(x = LocalDaMorte)) +
  geom_bar()
#A maior parte das vítimas, mais de 200 pessoas, foram assassinadas em suas residências. Cerca de 10 morreram no hospital e aproximadamente 5 faleceram na casa de repouso.

#(e)
ggplot(data = dados, mapping = aes(x = AnoDaMorte)) +
  geom_bar()
#O número de homicídios aumentou ao longo das décadas, sendo a década de 1990 a que registrou a maior quantidade de casos. Em 1997, foram contabilizadas mais de 40 vítimas.

#(f)
# A maioria das vítimas de Harold Shipman era composta por idosos entre 70 e 85 anos, principalmente mulheres. Shipman costumava realizar os assassinatos nas residências das vítimas. Além disso, houve um crescimento expressivo no número de casos entre as décadas de 1970 e 1990, com 1997 registrando o maior número de vítimas.


#Exercicio 14
library(ggplot2)

#(a)
primatas <- read.table(file = "primatas (1).txt", header = TRUE, sep = ":")
str(primatas)
summary(primatas)

#(b)
ggplot(data = primatas, mapping = aes(x = especie)) +
  geom_bar()
ggplot(data = primatas, mapping = aes(x = genero)) +
  geom_bar() +
  facet_wrap(~especie)

#(c)
bonobos <- primatas[primatas$especie == "bonobo",]
ggplot(data = bonobos, mapping = aes(x = altura, y = peso, col = genero)) +
  geom_point()
chimpanzes <- primatas[primatas$especie == "chimpanze",]
ggplot(data = chimpanzes, mapping = aes(x = altura, y = peso, col = genero)) + 
  geom_point()

# (d)
femeas <- primatas[primatas$genero == "femea",]
ggplot(data = femeas, mapping = aes(x = altura, y = peso, col = especie)) +
  geom_point()
machos <- primatas[primatas$genero == "macho",]
ggplot(data = machos, mapping = aes(x = altura, y = peso, col = especie)) +
  geom_point()

#(e)
#A partir da análise dos gráficos, foi possível perceber que, nos chimpanzés, as fêmeas são menores e mais leves que os machos, ja nos bonobos, podemos definir que o peso é bem definido entre machos e fêmeas, ja a altura os machos tendem a ter uma altura maior que as fêmeas. Também é perceptível que os machos da espécie bonobo possuem peso semelhante às fêmeas da espécie chimpanzé, apesar de serem ligeiramente mais altos que essas fêmeas.

#(f)
primatas <- primatas[sample(nrow(primatas)),]
n <- round(0.8*nrow(primatas))
treino_primatas<- primatas[(1:n),]
teste_primatas <- primatas[-(1:n),]
ggplot(data = treino_primatas, mapping = aes(x = altura, y = peso, col = genero))+
  geom_point()+
  facet_wrap(~especie)

resultado <- c()

for(i in 1:nrow(teste_primatas)){
  if(all(teste_primatas$genero[i] == "macho")){
    if(teste_primatas$peso[i] >= 55){
      resultado[i] <- "chimpanze"
    }else{
      resultado[i] <- "bonobo"
    }
  }else{
    if((teste_primatas$altura[i] > 122) & (teste_primatas$peso[i] <= 35)){
      resultado[i] <- "bonobo"
    }else{
      resultado[i] <- "chimpanze"
    }
  }
}
proporcao_acertos <- mean(teste_primatas$especie == resultado)
proporcao_acertos <- round(proporcao_acertos,2)
paste("A acurácia é de", proporcao_acertos, "%")
