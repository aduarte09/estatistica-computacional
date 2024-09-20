#Exercício 1
#Crie uma função chamada EstimativaIntegral que possua como entradas dois números reais a e b, a < b, e que retorne o valor da integral
#Por fim, utilize a função anterior para estimar a integral com a = −1 e b = 2.

EstimativaIntegral <- function(a, b) {
  integrand <- function(x) {
    exp(2 * x + x^2)
  }
  resultado <- integrate(integrand, a, b)
  return(resultado$value)
}

# Estimando a integral de a = -1 até b = 2
a <- -1
b <- 2
valor_integral <- EstimativaIntegral(a, b)
valor_integral


#Exercicio 2
#Um álbum tem espa¸co para N figurinhas. Cada pacote que você compra, para preencher o álbum, vem com uma figurinha. Todas as figurinhas
#são igualmente prováveis de serem obtidas. Seja X a variável aleatória que conta o número de figurinhas que você tem que comprar para completar o álbum.
#Assim, como exemplo, considere que N = 5 e que as figurinhas sejam 1, 2, 3, 4, 5. Suponha que as figurinhas que você obteve, na ordem das compras, 
#foram: 4, 2, 3, 3, 4, 5, 1. Portanto, neste caso, X = 7.

#(a) Para N = 20, estime via Monte Carlo a esperança de X, isto é, a quantidade média de figurinhas que você tem que comprar para preencher o álbum.
#Estime também a probabilidade de X ser menor ou igual a 30.

# Definir o número de figurinhas no álbum
N <- 20
# Definir o número de simulações para a estimativa
simulacoes <- 10000
# Vetor para armazenar o número de figurinhas compradas em cada simulação
resultados <- numeric(simulacoes)
# Simulação de Monte Carlo
set.seed(123)  # Para reprodutibilidade
for (i in 1:simulacoes) {
  # Figurinhas compradas
  figurinhas <- integer(N)  # Vetor para armazenar as figurinhas já obtidas
  compras <- 0              # Contador de compras realizadas
  while (sum(figurinhas > 0) < N) {   # Enquanto não completar o álbum
    nova_figurinha <- sample(1:N, 1)  # Compra uma figurinha aleatória
    figurinhas[nova_figurinha] <- 1   # Marca a figurinha como obtida
    compras <- compras + 1            # Incrementa o contador de compras
  }
  resultados[i] <- compras  # Armazena o número de compras feitas
}
# Estimar E[X]: média do número de figurinhas compradas
esperanca_X <- mean(resultados)
esperanca_X
# Estimar P(X <= 30)
prob_X_menor_igual_30 <- mean(resultados <= 30)
prob_X_menor_igual_30

#(b) Crie uma função cuja entrada seja N e cuja saída seja a estimativa de E[X].

# Função para estimar E[X] dado N figurinhas no álbum
estima_E_X <- function(N, simulacoes = 10000) {
  # Vetor para armazenar o número de figurinhas compradas em cada simulação
  resultados <- numeric(simulacoes)
  # Simulação de Monte Carlo
  for (i in 1:simulacoes) {
    figurinhas <- integer(N)  # Vetor para armazenar as figurinhas já obtidas
    compras <- 0              # Contador de compras realizadas
    while (sum(figurinhas > 0) < N) {  # Enquanto não completar o álbum
      nova_figurinha <- sample(1:N, 1)  # Compra uma figurinha aleatória
      figurinhas[nova_figurinha] <- 1   # Marca a figurinha como obtida
      compras <- compras + 1            # Incrementa o contador de compras
    }
    resultados[i] <- compras  # Armazena o número de compras feitas
  }
  # Estimação de E[X]: média do número de figurinhas compradas
  esperanca_X <- mean(resultados)
  return(esperanca_X)
}
# Exemplo de uso da função para N = 20
N <- 20
estima_E_X(N)


#Exercício 3
#É O DO LUKE DO TRABALHO
