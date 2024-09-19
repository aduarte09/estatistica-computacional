# instalando o pacote ggplot2
install.packages("ggplot2")
library(ggplot2)


# importando os dados
titanic <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
titanic <- titanic[, -c(1,9:12)]
str(titanic)


# definindo os fatores
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
str(titanic)
summary(titanic)


# criando gráficos com ggplot
ggplot(data = titanic, aes(x = Survived)) + 
  geom_bar(fill = "#8856a7") + 
  theme_minimal()

ggplot(data = titanic, aes(x = Survived, fill = Sex)) +
  geom_bar() + 
  scale_fill_manual(values = c("female" = "#fa9fb5", "male" = "#9ebcda")) +
  theme_minimal()

ggplot(data = titanic, aes(x = Sex, fill = Survived)) + 
  geom_bar() + 
  scale_fill_manual(values = c("1" = "#845EC2", "0" = "#B39CD0")) + 
  theme_minimal()

ggplot(data = titanic, aes(x = Sex, fill = Survived)) + 
  geom_bar() + 
  labs(title = "Análise de sobrevivência de mulheres e homens por classe",
       x = "Sexo",
       y = "Frequência",
       fill = "Sobreviveu") +
  scale_fill_manual(values = c("1" = "#B39CD0", "0" = "#845EC2"), 
                    labels = c("1" = "Sim", "0" = "Não")) +
  facet_wrap(~Pclass) + 
  scale_x_discrete(labels = c("female" = "mulheres", "male" = "homens")) +
  theme_minimal()

