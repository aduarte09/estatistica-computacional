library(dplyr)
library(ggplot2)

femur <-ggplot2femur <- read.csv("femur.csv", header = TRUE)
str(femur)
summary(femur)

homens <- femur |>
  filter(genero == "Male")
str(homens)

cor(homens$altura, homens$femur)

ggplot(data = homens, aes(x = femur, y = altura)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(homens$altura, homens$femur)

modelo <- lm(data = homens, formula = altura ~ femur)
modelo
summary(modelo)

modelo$residuals
hist(modelo$residuals)

shapiro.test(modelo$residuals)
