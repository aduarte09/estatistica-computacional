#REVISÃO - COLETA DE DADOS EM R
#COMANDOS BÁSICOS E UTILIZADOS NA MAIORIA DAS VEZES

dados <- read.table("nomedoarquivo.txt",header = TRUE, sep = ", ou ;")

sep é o que separa os dados do arquivo

dados <- read.csv("nomedoarquivo.csv")

csv não precisa de separador, já que a vírgula é padrão do arquivo.

Para excluir uma coluna que não seja necessária :
  
  dados[,c(-1,-10)] #Exclui colunas 1 e 10
  Ordem sempre será linha e coluna, respectivamente
  
  
Se precisarmos separar um conjunto de dados específico de um maior, podemos fazer isso:
  
  homens <- dados[dados$sex == "homens"]
  Tira de dados apenas os homens com base na categoria sex
  Você pode ir filtrando de acordo com essas separações, por exemplo:
    
  homensTerceiraClasse <- homens[homens$class == "3"]
  
Resumo de um conjunto de dados = summary(conjunto)
Estrutura de todos os dados de um conjunto = str(conjunto)


Podemos transformar um conjunto em fator, para facilitar o uso no resumo, por ex:
  
  titanic$Survived = as.factor(titanic$Survived)
  Ao puxar str/summary novamente, a contagem estará bem mais fácil de ser visualizada
