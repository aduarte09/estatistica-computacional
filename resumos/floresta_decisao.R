install.packages("randomForest")
library(randomForest)
library(rpart)
library(rpart.plot)

#Na árvore padrão, a escolha de uma influencia na escolha da outra. Queremos tornar
#a escolha aleatória na decisão, sem influência uma das outras.

#Construindo vários treinamentos e tirando influência de algumas variáveis podem
#ser maneiras válidas de separar essas classificações...

#Se temos n variáveis,vamos sortear sqrt(n) variáveis.

#Várias árvores separadas = floresta de decisão.

library(ggplot2)

cancer <- read.table("cancer.txt",sep = ",",TRUE)
str(cancer)
cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer <- na.omit(cancer)

n <- round(0.8*nrow(cancer))

set.seed(14092004)

indices_treino <-sample(1:nrow(cancer),n,FALSE)

treino <- cancer[indices_treino,]
teste <- cancer[-indices_treino,]

arvore <- rpart(formula = diagnosis ~.,data = treino,method = "class")
previsao <- predict(arvore , newdata=teste ,type="class")


mean(previsao == teste$diagnosis)

# ~ . (diagnóstico se referindo a todas as variáveis)
#todas as variáveis serão utilizadas para determinar o diagnóstico

#número de árvores (ntree)

floresta <- randomForest(formula = diagnosis ~ .,data = treino, ntree = 200)

floresta

#00B -> out of bag, o que sobrou do sorteio, coisas repetidas.
#Jogamos lá dentro da árvore para construí-la, reutilizando o que,
#anteriormente, seria inútil.

#Não precisamos nem colocar o teste para o modelo já tentar nos dar uma resposta!

#randomForest::randomForest() ... Usar func específica do pacote sem ler ele por inteiro.

previsao.floresta <- predict(floresta, newdata = teste, type = "class")

mean(previsao.floresta == teste$diagnosis)
