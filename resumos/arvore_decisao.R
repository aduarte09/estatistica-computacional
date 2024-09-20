#ARVORE DE DECISÕES - UTILIZADA PARA CLASSIFICAR CONJUNTO DE DADOS
#Na maioria das vezes, precisaremos criar um conjunto de treino e um conjunto teste,
#e esse processo é um padrão infinito.

#Após isso, podemos fazer a árvore por if e else, ou pela própria função da biblioteca rpart... 

iris
iris <- iris[sample(nrow(iris)),] #embaralhar dados de iris, especificamente 
#as linhas

# 80% de iris
n <- round(0.8*nrow(iris)) #arredondar para 80%
n #(120)

treinamento <- iris[1:n,] #pegar dados de iris de 1 até 120(estão embaralhados)
teste <- iris[-(1:n),] #pegar o que não foi pego(restante)

ggplot(treinamento, aes(x = Petal.Length,y = Petal.Width, color = Species))+
  geom_point()

#Na maior parte das vezes, faremos um ggplot com x e y com dados utilizados para identificação
#e com a cor dividida nos tipos do conjunto de dados. Esse gráfico nos permitirá ver os dados
#que serão utilizados dentro da nossa árvore de decisões.

#--------------------------------------------------------------------
cancer <- read.csv("cancer.csv", TRUE)
cancer <- na.omit(cancer)
#TIRAR NA do conjunto de dados

n <- round(0.8*nrow(cancer))

indices_treino <-sample(1:nrow(cancer),n,FALSE)
#Vamos sortear os pinguins que serão colocados no treino
#ao invés de só embaralhar

#MANEIRA MUITO UTILIZADA TAMBEM, POR INDICES

treino <- cancer[indices_treino,]

teste <- cancer[-indices_treino,]

#-------------------------------------------------------------------
#ARVORE DE DECISÕES
for(j in 1:nrow(teste)){
  
  if(teste$Petal.Length[j] < 2.5){
    resultado[j] <- "setosa"
  } else {
    if(teste$Petal.Width[j] < 1.75){
      resultado[j] <- "versicolor"
    } else {
      resultado[j] <- "virginica"
    }
  }
  
}

#2.5, 1.75 foram dados que pegamos do ggplot de pontos.
#Nessa parte, o teste será utilizado, porque é nele que testaremos se nossas medidas estarão corretas.

#Para ver consistência,usar:
mean(teste$Species == resultado)

#Ver a média de acerto entre as espécies e os resultados da nossa árvore de decisão.

#----------------------------------------------
# USANDO RPART

#. = todos
#uma árvore de decisão pode ser dividida em: 
#arvore classificação algo que já existe, regressão prever algum valor

modelo.arvore <- rpart(formula = diagnosis~ ., data = treino, method = "class")
rpart.plot(modelo.arvore, extra = 101)

previsao <- predict(modelo.arvore, newdata = cancer, type = "class")

mean(previsao == cancer$diagnosis)

#fazer matriz confusão

table(previsao, cancer$diagnosis)

#linha = modelo
#coluna = erro
