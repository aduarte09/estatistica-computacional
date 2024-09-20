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
