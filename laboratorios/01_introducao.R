# operações básicas

2 + 5           # soma
2 - 4           # subtração
2 * 3           # multiplicação
2 / 2           # divisão
3 ** 2          # potência
3 ^ 2           # potência
10 %% 3         # resto da divisão


# atribuindo valores (<-, ->, =)

a <- 3 * 2
4 * 3 -> b
resultado = a + b
d <- TRUE


# função class()

class(a)
class(b)
class(d)


# valores lógicos
TRUE + TRUE
TRUE + FALSE

TRUE | TRUE     # or
TRUE | FALSE
TRUE & FALSE    # and
FALSE & FALSE
!TRUE           # not


# criando vetores

vet1 <- c(3, 7, 10)
lenght(vet1)
sum(vet1)
mean(vet1)

vet2 <- c(1, 2, "texto")
class(vet2)
vet2[c(2, 3)]

vet3 <- c(vet1, 60, 78)
vet3 < 50                     # verifica valores menores que 50
sum(vet3 < 50)                # soma os valores lógicos
vet3[vet3 < 50]               # busca valores menores que 50
sum(vet3[vet3 < 50])          # soma valores menores que 50



# criando um dado

sample(10)
dado <- sample(x = 1:6, size = 1000000, replace = TRUE)
dado
sum(dado == 3)/1000000
mean(dado == 3)
barplot(table(dado))
