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
