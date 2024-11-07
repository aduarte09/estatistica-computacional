# Lista ESTCOMP 2 - Matheus Fiod Saliba
# 12221BCC024

# Exercício 1)

diabetes <- read.table("diabetes.txt", header = TRUE, sep = ";")

diabetes <- diabetes[,-1]

diabetes$Diabetic <- as.factor(diabetes$Diabetic)

diabetes <- diabetes[sample(nrow(diabetes)),]

n <- round(0.8*nrow(diabetes))

treinamento <- diabetes[1:n,] #
teste <- diabetes[-(1:n),] 

# a)
library(ggplot2)

ggplot(treinamento, aes(x = BMI))+
  geom_boxplot()+
  facet_wrap(~Diabetic)
  
ggplot(treinamento, aes(x = Age, fill = factor(Diabetic))) +
  geom_histogram(bins = 50, alpha = 0.6) +
  labs(x = "Age", y = "Count", fill = "Diabetes")

ggplot(treinamento, aes(x = Pregnancies, y = BMI, color = Diabetic))+
  geom_point()

ggplot(treinamento, aes(x = Age, y = BMI, color = factor(Diabetic)))+
  geom_point(alpha = 0.6)

# Com base nesses gráficos, conseguimos ter uma base das principais diferenças de dados entre pessoas
# que são diabéticas ou não. Podemos notar que em relação ao BMI, pessoas que tem diabetes costumam ter
# um índice de massa corporal consideravelmente maior em relação às pessoas que não possuem, com uma média
# bem maior quando analisamos o boxplot criado.
# Além disso,notamos no histograma criado para analisar a idade, que normalmente temos diabéticos entre 20
# e 25 anos com maior frequência do que o resto, voltando a ser comum entre 40-50 anos.
# Vale ressaltar que diabéticos são muito comuns no grupo que tiveram mais de 2 filhos / ficaram
# grávidas mais de duas vezes.

# b)

# Realizando a árvore de decisões com rpart :

library(rpart)
library(rpart.plot)

modelo.arvore <- rpart(formula = Diabetic~ ., data = treinamento, method = "class")
rpart.plot(modelo.arvore, extra = 101)

# Criando uma função, e considerando que 0 = não diabético e 1 = diabético.

diagnostico <- function(preg,glucose,insulin,bmi,idade){
  
  if(preg < 2){
    return(0)
  } else {
    if(bmi < 22){
      return(0)
    } else {
      if(insulin < 52 && idade < 36) {
        return(0)
      } else {
        if(insulin > 52 && idade < 36 && glucose < 96 && bmi >= 35){
          return(0)
        } else {
           return(1)
        }
      }
    }
  }

}

# Para classificar teste, usaremos um for:

resultados <- c()

for(i in 1:nrow(teste)){
  
  resultados[i] <- diagnostico(teste$Pregnancies[i],teste$PlasmaGlucose[i],teste$SerumInsulin[i],teste$BMI[i],teste$Age[i])
  
}

mean(teste$Diabetic == resultados)

# Com o modelo de diagnóstico com condicionais utilizado,
# temos uma acurácia de praticamente 90% nos diagnósticos. (0.89)

# c)

# Criando o modelo de árvore aleatória com o randomForest:

library(randomForest)

floresta <- randomForest(formula = Diabetic~ ., data = treinamento, ntree = 200)

floresta

# Encontrando a acurácia com o predict:

floresta.previsao <- predict(floresta,newdata = teste,type = "class")

mean(floresta.previsao == teste$Diabetic)  

#A acurácia do modelo é de 93%%.

# d) Para o primeiro modelo, a probabilidade da classificação ser correto quanto ao diagnóstico de diabetes
#    de um paciente é de aproximadamente 90%. Por outro lado, no segundo modelo, a probabilidade é de 93%.

# e) O modelo que se saiu melhor foi a floresta de decisão, que conseguiu com a função criar uma acurácia de
#    acerto de 93%, o que é um resultado muito bom. Por outro lado, o modelo da floresta se saiu bem, mas não
#    tanto quanto a floresta, conseguindo uma acurácia de aproximadamente 90%.

#--------------------------------------------------------------------------------------------------------------

# Exercício 2)

cerebelo <- read.csv("cerebelo.csv")


# a)

ggplot(cerebelo, aes(x = Body_g, y = Cerebellum_g))+
  geom_point()

ggplot(cerebelo, aes(x = Log_body, y = Log_cerebellum))+
  geom_point()

# Basicamente, podemos notar que o gráfico de dispersão com os valores na forma logarítmica teve um resultado
# muito melhor, indo de acordo com uma reta imaginária (nesse caso) do modelo linear e indicando uma possível 
# forte correlação. Por outro lado, o que utilizamos os valores sem a transformação teve um resultado um pouco
# inconsistente, tendo um intervalo de confiança muito amplo, o que não é uma boa estimativa. Apesar da diferença
# dos resultados dos gráficos, conseguimos afirmar que com certeza há uma grande possibilidade de existir correlação
# entre o peso do corpo e o peso do  na forma logarítmica, pelos pontos estarem criando uma reta na dispersão,
# seguindo o modelo. No modelo não logarítmico, a correlação não parece ser forte.

# b)

correlacao_g <- cor(cerebelo$Body_g,cerebelo$Cerebellum_g)

# c)

correlacao_log <- cor(cerebelo$Log_body,cerebelo$Log_cerebellum)

# d) Como esperávamos pelos gráficos de dispersão, a correlação do modelo não logarítmico é extremamente baixa,
#    já o modelo logarítmico é alta, indicando grande relação entre eles.

# e) 

# Determinando a reta de regressão:

modelo <- lm(data = cerebelo, formula = Log_body ~ Log_cerebellum)

summary(modelo)

# Com base em summary(modelo), podemos afirmar que o valor A da reta é intercept, ou seja, aproximadamente 2.8.
# Também conseguimos afirmar que o valor de B será Log_cerebellum, ou seja, aproximadamente 1.15.
# Portanto, a equação da reta será 2.8x + 1.15y = 0.

cor.test(cerebelo$Log_body,cerebelo$Log_cerebellum)

# Sobre o teste de hipóteses, analisaremos principalmente o p-valor indicado pela função cor.test,
# com valor igual a 6.912e-08, ou seja, um valor muito menor do que o valor que usamos como base (5%),
# o que nos faz rejeitar H0, indicando que o coeficiente de correlação não é zero.

# Gráficos com reta inserida :

ggplot(cerebelo, aes(x = Body_g, y = Cerebellum_g))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(cerebelo, aes(x = Log_body, y = Log_cerebellum))+
  geom_point()+
  geom_smooth(method = "lm")

# f) 

#Um teste de hipóteses para verificar se os resíduos seguem uma distribuição normal.

#H0:resíduos seguem uma distribuição normal
#Ha:resíduos não seguem distribuição normal

#Vamos torcer para p valor ser maior que 5%, para não rejeitarmos.

hist(modelo$residuals)

# Usaremos o modelo de shapiro: 

shapiro.test(modelo$residuals)

# O p-valor deu 2% (0.2335), um valor menor que 10%, o que garante a rejeição de H0, indicando que os
# resíduos não seguem uma distribuição normal, o que torna o modelo um bom modelo.

# g) 

peso <- 100000

log_peso <- log10(peso)
log_cerebelo <- 1.15*log_peso - 2.8
cerebelo <- 10^log_cerebelo


#--------------------------------------------------------------------------------------------------------------

# Exercício 3) CLUSTERIZAÇÃO K-MEANS

azeite <- read.table("olive.txt",header = TRUE, sep = ",")

# a)

dados_padronizados <- scale(azeite[,2:9])

# b)

clusterizacao <- kmeans(x = azeite[,2:9], centers = 3, nstart = 20)

cluster <- as.factor(clusterizacao$cluster)

azeite$cluster_k3 <- cluster

ggplot(azeite, aes(x = cluster_k3, fill = region))+
  geom_bar()

# Conseguimos um gráfico que nos mostra de acordo com cada cluster criado, a classificação dos azeites
# em cada um deles. Podemos notar que no cluster 1, temos maior número de azeites do sul da Itália, e um
# pequeno setor da Sardinia. No cluster 2, maioria composta pelo sul da Itália, uma parte menor pela Sardinia
# e uma parte menor ainda para o norte da Itália. E por último, conseguimos analisar que o cluster 3 concentra
# azeites do norte,e uma minoria do sul.

# c)

# Para k = 4

clusterizacao <- kmeans(x = azeite[,2:9], centers = 4, nstart = 20)

cluster <- as.factor(clusterizacao$cluster)

azeite$cluster_k4 <- cluster

ggplot(azeite, aes(x = cluster_k4, fill = region))+
  geom_bar()

# No primeiro cluster, maioria sul e pequena parte sardinia. No segundo cluster, maioria norte, minoria sul.
# No terceiro cluster, uma quantidade menor, mas compensada pela presença integral de sul. No quarto, uma grande quantidade
# assim como o primeiro e segundo, mas com pluralidade entre regiões.

# Para k = 5

clusterizacao <- kmeans(x = azeite[,2:9], centers = 5, nstart = 20)

cluster <- as.factor(clusterizacao$cluster)

azeite$cluster_k5 <- cluster

ggplot(azeite, aes(x = cluster_k5, fill = region))+
  geom_bar()

# No primeiro cluster, uma quantidade grande, com maioria norte e minoria sul. No segundo cluster, quantidade
# também grande, com maioria sul e minoria sardinia. No terceiro, quantidade pequena com unicidade sul. No quarto
# quantidade mediana com maioria sul e minoria norte. Por último, mas não menos importante, o quinto cluster, contendo
# maioria sardinia e minoria norte e sul.
       
       
