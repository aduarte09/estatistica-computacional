ESTATÍSTICA COMPUTACIONAL

Laboratórios
1. Funções
sample(): sorteia um elemento;
duplicate(): verifica se há valores duplicados em um vetor ou data frame;
mean(): calcula a média de um conjunto de valores;
any(): retorna TRUE se ao menos um valor em uma expressão lógica for TRUE;
read.table(): lê dados de um arquivo e os retorna em forma de data frame;
read.csv(): lê dados de um arquivo CSV e os retorna em forma de data frame;
summary(): gera um resumo estatístico das variáveis de um data frame;
sum(): soma os elementos de um vetor ou coluna;
length(): retorna o número de elementos em um vetor ou o comprimento de uma lista;
as.factor(): converte uma variável em um fator (usado para variáveis categóricas);
table(): cria uma tabela de contagem de frequências de uma variável;
barplot(): cria gráficos de barras;
facet_wrap(): divide um gráfico ggplot em subgráficos com base em uma variável categórica;
nrow(): retorna o número de linhas de um data frame;
na.omit(): remove linhas com valores NA (ausentes) de um data frame;
data.frame(): cria um data frame a partir de vetores ou listas;
cor(): matriz de correlação, mede a força e direção da relação entre variáveis numéricas.


2. Estruturas de repetição
for(i in intervalo) {}
while(condição lógica) {}

3. Estruturas condicionais
if(condição lógica) {}
else {}
else if(condição lógica) {}

4. Declarando funções
nome_funcao <- function() {código}

5. Gráficos ggplot
Barra: geom_bar()
Ponto: geom_point()
Boxplot: geom_boxplot()
Histograma


6. Treino e teste
Aleatorizar a ordem dos dados com sample;
Definir n como 80% do total de dados;
80% para treino;
20% para teste;
Para KNN: padronizar os dados com scale;
Analisar gráficos do treino;
Classificar dados do teste.

7. Bibliotecas
ggplot2: criação de gráficos, ggplot()
class
rpart
rpart.plot
randomForest

Aqui está um breve comentário sobre as bibliotecas que você mencionou e suas principais funções:


     - ``, ``, ``, `geom_histogram()`: Criam diferentes tipos de gráficos (barra, ponto, boxplot, histograma).
     - `facet_wrap()`: Divide o gráfico em facetas com base em uma variável categórica.
   
2. **class**: Usada para realizar classificações, especialmente em algoritmos como K-Nearest Neighbors (KNN).
   - Principal função:
     - `knn()`: Classifica dados usando o algoritmo KNN.

3. **rpart**: Biblioteca para criação de árvores de decisão, usada em problemas de classificação e regressão.
   - Principal função:
     - `rpart()`: Cria uma árvore de decisão baseada em um conjunto de dados.

4. **rpart.plot**: Usada para visualizar árvores de decisão geradas pelo `rpart`.
   - Principal função:
     - `rpart.plot()`: Plota a árvore de decisão de forma visual.

5. **randomForest**: Implementa o algoritmo Random Forest, que cria múltiplas árvores de decisão e usa votação para classificação ou média para regressão.
   - Principal função:
     - `randomForest()`: Gera o modelo de floresta aleatória para classificação ou regressão.

Essas bibliotecas são muito úteis para visualização de dados, classificação, e modelagem preditiva.


Lista de exercícios
---


Revisão
---


