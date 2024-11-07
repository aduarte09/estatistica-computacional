library(ggplot2)
library(rpart)
library(rpart.plot)

#QUESTÃO 1 

#A------------------------------------------------------------------

dados_diabetes <- read.table("diabetes.txt", header = TRUE, sep = ";")


head(dados_diabetes)

indice_treino <- sample(1:nrow(dados_diabetes), 0.7 * nrow(dados_diabetes))
treinamento <- dados_diabetes[indice_treino, ]
teste <- dados_diabetes[-indice_treino, ]


ggplot(treinamento, aes(x = PlasmaGlucose, fill = factor(Diabetic))) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Distribuição da Glicose por Status de Diabetes",
       x = "Concentração de Glicose",
       fill = "Diabetes")
#QUEM TEM DIABETES TEM CONCENTRAÇÃO MENOR DE GLICOSE


ggplot(treinamento, aes(x = factor(Diabetic), y = BMI, fill = factor(Diabetic))) +
  geom_boxplot() +
  labs(title = "Boxplot do Índice de Massa Corporal (IMC) por Status de Diabetes",
       x = "Diabetes",
       y = "IMC")
#DIABÉTICOS TEM IMC ELEVADO EM RELAÇÃO AOS NÃO DIABÉTICOS

ggplot(treinamento, aes(x = factor(Diabetic), y = Age, fill = factor(Diabetic))) +
  geom_boxplot() +
  labs(title = "Boxplot da Idade por Status de Diabetes",
       x = "Diabetes",
       y = "Idade")
#APESAR DE A MÉDIA DA IDADE DE DIABÉTICOS SER MAIS ELEVADA, HÁ MUITOS OUTLIERS NOS NÃO DIABÉTICOS, DANDO A ENTENDER QUE IDADE NÃO PODE SER ANALISADA DE FORMA ISOLADA.

ggplot(treinamento, aes(x = factor(Diabetic), y = DiastolicBloodPressure, fill = factor(Diabetic))) +
  geom_boxplot() +
  labs(title = "Boxplot da Pressão Sanguínea por Status de Diabetes",
       x = "Diabetes",
       y = "Pressão Sanguínea")
#DIABÉTICOS TEM PRESSÃO SANGUÍNEA MAIS ELEVADA

#B------------------------------------------------------------------

modelo_arvore <- rpart(Diabetic ~ Pregnancies + PlasmaGlucose + DiastolicBloodPressure + TricepsThickness + 
                         SerumInsulin + BMI + DiabetesPedigree + Age,
                        data = treinamento, method = "class")

rpart.plot(modelo_arvore, type = 2, extra = 104, under = TRUE, fallen.leaves = TRUE, main = "Árvore de Decisão para Diagnóstico de Diabetes")



diagnostico <- function(preg, glucose, insulin, bmi, idade) {
  if (preg < 2) {
    return(0)  
  } else {
    if (bmi < 22) {
      if (idade < 36) {
        return(0)  
      } else {
        return(1) 
      }
    } else {
      if (insulin < 52) {
        return(0)  
      } else {
        if (glucose < 96) {
          return(1) 
        } else {
          if (idade < 36) {
            return(1)  
          } else {
            if (idade >= 24 && idade < 27) {
              return(0)  
            } else {
              return(1)  
            }
          }
        }
      }
    }
  }
}
#0 NÃO DIABÉTICO  ---- 1 DIABÉTICO

previsoes <- sapply(1:nrow(teste), function(i) {
  diagnostico(
    teste$Pregnancies[i], teste$PlasmaGlucose[i], teste$SerumInsulin[i],
    teste$BMI[i], teste$Age[i]
  )
})

teste$Diabetic_label <- ifelse(teste$Diabetic == 1, 1, 0)

acuracia <- mean(previsoes == teste$Diabetic_label)
acuracia

#C------------------------------------------------------------------


library(randomForest)
modelo_floresta <- randomForest(factor(Diabetic) ~ Pregnancies + PlasmaGlucose + DiastolicBloodPressure +
                                  TricepsThickness + SerumInsulin + BMI + DiabetesPedigree + Age,
                                data = treinamento, ntree = 100, mtry = 3, importance = TRUE)

previsoes_floresta <- predict(modelo_floresta, teste)

acuracia_floresta <- mean(previsoes_floresta == factor(teste$Diabetic))
acuracia_floresta

#D------------------------------------------------------------------

#Floresta é melhor, tendo 93% aproximadamente de acerto, já a árvore 87% aproximadamente


#E------------------------------------------------------------------

#A árvore de decisão, embora mais simples e fácil de entender, não conseguiu pegar tão bem as complexidades dos dados como a floresta. Isso mostra que, para esse tipo de problema, o randomForest costuma dar melhores resultados. Mas a árvore de decisão é boa quando a gente precisa entender de onde veio a resposta, partindo da interpretação do usuário em relação à arvore gerada.


#2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#A------------------------------------------------------------------

library(ggplot2)


data <- read.csv("cerebelo.csv")

ggplot(data, aes(x = Body_g, y = Cerebellum_g)) +
  geom_point(color = "blue") +
  labs(title = "Dispersão do Peso do Cerebelo vs Peso do Corpo", 
       x = "Peso do Corpo (g)", y = "Peso do Cerebelo (g)") +
  theme_minimal()

ggplot(data, aes(x = Log_body, y = Log_cerebellum)) +
  geom_point(color = "red") +
  labs(title = "Dispersão do Log Peso do Cerebelo vs Log Peso do Corpo", 
       x = "Log Peso do Corpo", y = "Log Peso do Cerebelo") +
  theme_minimal()



#B------------------------------------------------------------------


correlacao <- cor(data$Body_g, data$Cerebellum_g)
correlacao

#C------------------------------------------------------------------

correlacao_log <- cor(data$Log_body, data$Log_cerebellum)
correlacao_log

#D------------------------------------------------------------------
#A correlação entre peso do cerebelo e o peso do corpo é bem fraca, o que não indica um crescimento proporcional, ou seja, nem todas espécies segue essa tendência com relação as 2 variáveis. Já a segunda relação, entre logaritmos do peso do cerebelo e do corpo, conseguimos perceber uma forte correlação, o que não necessáriamente indica que uma coisa leva a outra, porém, nesse caso sugere que o peso do cerebelo cresce de forma proporcional ao aumento do peso do corpo, especialmente quando observamos as espécies de maior porte.

#E------------------------------------------------------------------
modelo <- lm(data = data, formula = Log_body ~ Log_cerebellum)
summary(modelo)


ggplot(data, aes(x = Log_body, y = Log_cerebellum)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Dispersão com a Reta de Regressão",
       x = "Log Peso do Corpo", y = "Log Peso do Cerebelo") +
  theme_minimal()

#EQUAÇÃO DA RETA: 2.8x + 1.15y = 0.

cor.test(data$Log_body, data$Log_cerebellum)
#6.912e-08, rejeitamos h0, o que indica que a correlação NÃO é 0

#F------------------------------------------------------------------

teste_normalidade <- shapiro.test(modelo$residuals)
teste_normalidade

#p = 0.2335 rejeitamos h0

#G------------------------------------------------------------------
peso <- 100000

calc_cerebelo <- function(peso){
  log_peso <- log10(peso)
  log_cerebelo <- 0.7828*log_peso - 2.1574
  cerebelo <- 10^log_cerebelo
  return(cerebelo)
}

calc_cerebelo(peso)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#A------------------------------------------------------------------

olive <- read.csv("olive.txt")
dados_numericos <- olive[, -1]

dados_padronizados <- scale(dados_numericos)
head(dados_padronizados)


#B------------------------------------------------------------------
modelo_k3 <- kmeans(dados_padronizados, centers = 3)

olive$cluster_k3 <- as.factor(modelo_k3$cluster)

ggplot(olive, aes(x = cluster_k3, fill = region)) +
  geom_bar() +
  labs(title = "Distribuição dos Clusters por Região", 
       x = "Cluster K3", 
       y = "Contagem de Azeites") +
  theme_minimal()

#NO PRIMEIRO CLUSTER, VEMOS QUE A ITALIA DOMINA, COM O NORTE LEVEMENTE NA FRENTE DO QUE O SUL
#NO SEGUNDO CLUSTER, O SUL DA ITÁLIA ESTÁ BEM FRACO, COM A SARDINA COM A MESMA QUANTIDADE DO QUE O SUL DA ITALIA NO CLUSTER 1, E O NORTE DA ITALIA COM MUITO MAIS AZEITES
#NO TERCEIRO CLUSTER, O SUL DA ITÁLIA E A SARDINIA ESTÃO COM QUANTIDADES ELEVADAS DE AZEITES, MENOS DO QUE O 2° CLUSTER, COM A SARDINIA LEVEMENTE À FRENTE

#C------------------------------------------------------------------

#K = 4
modelo_k4 <- kmeans(dados_padronizados, centers = 4)

olive$cluster_k4 <- as.factor(modelo_k4$cluster)

ggplot(olive, aes(x = cluster_k4, fill = region)) +
  geom_bar() +
  labs(title = "Distribuição dos Clusters por Região (k = 4)", 
       x = "Cluster K4", 
       y = "Contagem de Azeites") +
  theme_minimal()


#K = 5

modelo_k5 <- kmeans(dados_padronizados, centers = 5)

olive$cluster_k5 <- as.factor(modelo_k5$cluster)

ggplot(olive, aes(x = cluster_k5, fill = region)) +
  geom_bar() +
  labs(title = "Distribuição dos Clusters por Região (k = 5)", 
       x = "Cluster K5", 
       y = "Contagem de Azeites") +
  theme_minimal()








