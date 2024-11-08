library(ggplot2)             # gráficos
library(rpart)               # árvore de decisão
library(rpart.plot)          # árvore de decisão
library(randomForest)        # floresta aleatória


# exercício 01

# importando os dados
diabetes <- read.table(file = "diabetes.txt", header = TRUE, sep = ";")
str(diabetes)
summary(diabetes)

# organizando os dados
diabetes <- diabetes[,-1]                         # removendo id
diabetes$Diabetic <- as.factor(diabetes$Diabetic) # diagnóstico vira factor
str(diabetes)

# criando os conjuntos de treino e teste
diabetes <- diabetes[sample(nrow(diabetes)),]     # aleatorizando a ordem
n <- round(0.8 * nrow(diabetes))

treino_diabetes <- diabetes[(1:n),]               # treino: 80% dos dados
teste_diabetes <- diabetes[-(1:n),]               # teste: 20% dos dados


# 1-a: análise gráfica

# Quanto à concentração de glicose no plasma, a maioria dos diabéticos estão entre 95-120.
ggplot(data = treino_diabetes, 
       mapping = aes(x = PlasmaGlucose, fill = Diabetic)) + 
  geom_bar() +
  theme_classic()

# Quanto à pressão sanguínea, a maioria dos diabéticos estão entre 55-90.
ggplot(data = treino_diabetes, 
       mapping = aes(x = DiastolicBloodPressure, fill = Diabetic)) + 
  geom_bar() +
  theme_classic()

# Quanto à concentração de insulina sérica no plasma, os não diabéticos apresentam a mediana de 50, enquanto diabéticos apresentam a mediana de 150.
ggplot(data = treino_diabetes, 
       mapping = aes(y = SerumInsulin, fill = Diabetic)) +
  geom_boxplot() +
  theme_classic()

# Quanto à idade, entre os 40-60 anos, há mais diabéticos que não diabéticos.
ggplot(data = treino_diabetes,
       mapping = aes(x = Age, fill = Diabetic)) +
  geom_histogram(binwidth = 1, color = "white") +
  theme_classic()


# 1-b: árvore de decisão

# criando um modelo de árvore de decisão
arvore_diabetes <- rpart(formula = Diabetic ~ .,
                         data = treino_diabetes,
                         method = "class")

# plotando a árvore de decisão
rpart.plot(arvore_diabetes, extra = 101)

# criando função
diagnosticar <- function(Pregnancies, PlasmaGlucose, DiastolicBloodPressure, TricepsThickness, SerumInsulin, BMI, DiabetesPedigree, Age) {
  
  if(Pregnancies < 2) return(0)
  else {
    if(BMI < 22) return(0)
    else {
      if(SerumInsulin < 62) {
        if(Age < 38) return(0)
        else return(1)
      }
      else {
        if(Age < 38) {
          if(Age >= 24) {
            if(Age < 27) return(0)
            else return(1)
          }
          else return(1)
        }
        else return(1)
      }
    }
  }
  
}

# classificando os pacientes do teste
resultado_teste <- c()
for(i in 1:nrow(teste_diabetes)) {

  resultado_teste[i] <- diagnosticar(
    teste_diabetes$Pregnancies[i],
    teste_diabetes$PlasmaGlucose[i],
    teste_diabetes$DiastolicBloodPressure[i],
    teste_diabetes$TricepsThickness[i],
    teste_diabetes$SerumInsulin[i],
    teste_diabetes$BMI[i],
    teste_diabetes$DiabetesPedigree[i],
    teste_diabetes$Age[i])

}

# calculando acurácia da árvore de decisão
acuracia_diabetes <- mean(resultado_teste == teste_diabetes$Diabetic)
acuracia_diabetes    # aproximadamente 89%


# 1-c: floresta aleatória

# criando um modelo de floresta aleatória
floresta_diabetes <- randomForest(formula = Diabetic ~ .,
                                  data = treino_diabetes,
                                  ntree = 200)

resultado_teste <- predict(floresta_diabetes,
                           newdata = teste_diabetes,
                           type = "class")

# calculando acurária da floresta aleatória
acuracia_diabetes <- mean(resultado_teste == teste_diabetes$Diabetic)
acuracia_diabetes    # aproximadamente 93%


# 1-d: probabilidade de diagnóstico correto

# Para o modelo da árvore de decisão, a probabilidade de diagnóstico correto é de 89%. Para o modelo da floresta aleatória, a probabilidade é de 93%.


# 1-e: comparação dos dois modelos

# Dentre os dois modelos utilizados, o da floresta aleatória se saiu melhor, com precisão variando entre 93-94%. O modelo da árvore aleatória, entretanto, também teve um bom desempenho, alcançando precisão de 89-90% durante as execuções deste arquivo. Apesar da proximidade dos resultados para este caso, a floresta aleatória ainda apresenta uma maior confiabilidade, tendo em vista que realiza vários testes antes de classificar um paciente.



# exercício 02

# importando os dados
cerebelo <- read.csv(file = "cerebelo.csv", header = TRUE)
str(cerebelo)
summary(cerebelo)


# 2-a: gráficos de dispersão

# 1º gráfico de dispersão (cerebelo x corpo): A maioria das espécies ficaram bem próximas no gráfico, ou seja, houve pouca dispersão, o que dificulta no entendimento adequado dos dados.
ggplot(data = cerebelo,
       mapping = aes(x = Body_g, y = Cerebellum_g)) +
  geom_point() +
  theme_classic()

# 2º gráfico de dispersão (valores transformados): Com os valores transformados, houve maior dispersão e é possível ter uma melhor visualização dos dados. Dessa forma, nota-se um indício de correlação positiva. 
ggplot(data = cerebelo,
        mapping = aes(x = Log_body, y = Log_cerebellum)) +
  geom_point() +
  theme_classic()
          
# Os dois grafos apresentaram dispersões bem diferentes. O segundo, gerado a partir dos valores transformados, permite uma melhor análise, principalmente pelo indício de correlação positiva dos dados. Observando esse gráfico, tem-se a impressão de que o peso de cerebelo é proporcional ao peso do corpo de cada animal.


# 2-b: coeficiente de correlação
correlacao_g <- cor(cerebelo$Body_g, cerebelo$Cerebellum_g)
correlacao_g    # 0.34


# 2-c: coeficiente de correlação (valores transformados)
correlacao_log <- cor(cerebelo$Log_body, cerebelo$Log_cerebellum)
correlacao_log  # 0.94


# 2-d: comparação de coeficientes
# Os resultados obtidos por meio da função cor() confirmaram as suspeitas que surgiram com a plotagem dos gráficos. No primeiro caso, utilizando os valores não transformados, os dados apresentam baixa correlação, de aproximadamente 34%. Por outro lado, no segundo caso, com os valores transformados, a correlação foi bem significativa, com aproximadamente 94%.


# 2-e: reta de regressão

# encontrando a equação da reta de regressão
modelo_cerebelo <- lm(data = cerebelo,
                      formula = Log_body ~ Log_cerebellum)
summary(modelo_cerebelo)  

# Intercept: A = 2.80 // Log_cerebellum: B = 1.15
# equação da reta de regressão: 2.80x + 1.15y = 0

# analisando testes de hipótese
cor.test(cerebelo$Log_body, cerebelo$Log_cerebellum)

# p-value = 6.912e-08
# Como p-value é menor que 5%, devemos rejeitar H0. Dessa forma, temos que a correlação não é igual a 0.

# acrescentando a reta de regressão ao gráfico
ggplot(data = cerebelo,
       mapping = aes(x = Log_body, y = Log_cerebellum)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()


# 2-f: testes de hipótese para resíduos

# H0: os resíduos seguem uma distribuição normal
# Ha: os resíduos não seguem uma distribuição normal

hist(modelo_cerebelo$residuals)             # gerando histograma
shapiro.test(modelo_cerebelo$residuals)     # p-value = 0.2335

# Como o valor de p-value é menor que 5%, H0 é rejeitada. Ou seja, é possível concluir que os resíduos não seguem uma distribuição normal.


# 2-g: prevendo peso do cerebelo

peso_corpo <- 100000                      # peso do corpo em gramas
log_corpo <- log10(peso_corpo)            # transformando para log
log_cerebelo <- 1.15 * log_corpo - 2.80   # 2.80x + 1.15y = 0
peso_cerebelo <- 10 ^ log_cerebelo        # peso do cerebelo em gramas
peso_cerebelo                             # 891 gramas



# exercício 03

# importando os dados
olive <- read.table(file = "olive.txt", header = TRUE, sep = ",")
str(olive)
summary(olive)


# 3-1: padronização de dados

dados_padronizados <- scale(olive[, (2:9)])   # ignorando índice e região


# 3-b: modelo k-means (k = 3)

# gerando os clusters
clusterizacao <- kmeans(x = dados_padronizados, 
                        centers = 3, 
                        nstart = 20)

# adicionando os clusters no conjunto de dados
cluster_olive <- as.factor(clusterizacao$cluster)
olive$cluster_k3 <- cluster_olive

# gerando o gráfico de barras
ggplot(data = olive,
       mapping = aes(x = cluster_k3, fill = region)) +
  geom_bar() +
  theme_classic()


# No cluster 1, a maior parte do azeite tem origem na região Norte, com menos de 1% na região Sul, enquanto a Sardinia não aparece. No cluster 2, há apenas azeite da região Sul. No cluster 3, as regiões Sul e Sardinia possuem cerca de 100 dos azeites produzidos cada, enquanto a região Norte tem cerca de 50.


# 3-c: modelo k-means (k = 4, k = 5)

# gerando os clusters, com k = 4
clusterizacao <- kmeans(x = dados_padronizados, 
                        centers = 4, 
                        nstart = 20)

# adicionando os clusters no conjunto de dados
cluster_olive <- as.factor(clusterizacao$cluster)
olive$cluster_k4 <- cluster_olive

# gerando o gráfico de barras
ggplot(data = olive,
       mapping = aes(x = cluster_k4, fill = region)) +
  geom_bar() +
  theme_classic()


# Cluster 1: Apenas a região Sul aparece, com cerca de 250 azeites.
# Cluster 2: Região Sul tem cerca de 100 azeites, Norte tem menos de 50, Sardinia não aparece.
# Cluster 3: Sardinia tem cerca de 100 azeites, Norte tem menos de 50, Sul não aparece.
# Cluster 4: Apenas a região Norte aparece, com quase 150 azeites.


# gerando os clusters, com k = 5
clusterizacao <- kmeans(x = dados_padronizados, 
                        centers = 5, 
                        nstart = 20)

# adicionando os clusters no conjunto de dados
cluster_olive <- as.factor(clusterizacao$cluster)
olive$cluster_k5 <- cluster_olive

# gerando o gráfico de barras
ggplot(data = olive,
       mapping = aes(x = cluster_k5, fill = region)) +
  geom_bar() +
  theme_classic()


# Cluster 1: Norte tem quase 100 azeites, Sul tem menos que 10, Sardinia não aparece.
# Cluster 2: Sardinia tem cerca de 100 azeites, Norte tem menos que 10, SUl não aparece.
# Cluster 3: Apenas a região Sul aparece, com cerca de 250 azeites.
# Cluster 4: Apenas a região Norte aparece, com um pouco mais de 50 azeites.
# Cluster 5: Sul tem cerca de 100 azeites, Norte tem menos que 10, Sardinia não aparece.

