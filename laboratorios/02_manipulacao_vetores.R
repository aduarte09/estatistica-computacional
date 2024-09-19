# operações com vetores

a <- c(1, 2, 5)
b <- c(10, 12)
d <- c(a, 6)

3 * a
3 + a
a + b
d + b


# for em R

x <- 0
for(i in 1:20) {
  x <- x + i
}
x
print(x)


# problema dos aniversários

aniversarios <- sample(x = 1:366, size = 10, replace = TRUE)
aniversarios
duplicated(aniversarios)


resultados <- c()
for (j in 1:10000) {
  aniversarios <- sample(x = 1:366, size = 40, replace = TRUE)
  resultados[j] <- any(duplicated(aniversarios))
}
mean(resultados)


# criando uma função

calcular_probabilidade <- function(n) {
  for(k in 1:10000) {
    aniversarios <- sample(x = 1:366, size = n, replace = TRUE)
    resultados[k] <- any(duplicated(aniversarios))
  }
  return(mean(resultados))
}

calcular_probabilidade(n = 23)


# problema da mega sena

bilhete <- c(4, 5, 12, 43, 21, 34)
semanas <- 0
acertos <- 0

# fazer a estimativa correta utilizando um for x10000
while(acertos < 6) {
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
  acertos <- sum(bilhete %in% sorteio)
  semanas <- semanas + 1
}

acertos
semanas