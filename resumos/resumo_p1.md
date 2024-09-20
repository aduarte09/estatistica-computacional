# ESTATÍSTICA COMPUTACIONAL

### Funções
- ```sample()``` sorteia um elemento;
- ```duplicate()``` verifica se há valores duplicados em um vetor ou data frame;
- ```mean()``` calcula a média de um conjunto de valores;
- ```any()``` retorna TRUE se ao menos um valor em uma expressão lógica for TRUE;
- ```read.table()``` lê dados de um arquivo e os retorna em forma de data frame;
- ```read.csv()``` lê dados de um arquivo CSV e os retorna em forma de data frame;
- ```summary()``` gera um resumo estatístico das variáveis de um data frame;
- ```sum()``` soma os elementos de um vetor ou coluna;
- ```length()``` retorna o número de elementos em um vetor ou o comprimento de uma lista;
- ```as.factor()``` converte uma variável em um fator (usado para variáveis categóricas);
- ```table()``` cria uma tabela de contagem de frequências de uma variável;
- ```barplot()``` cria gráficos de barras;
- ```facet_wrap()``` divide um gráfico ggplot em subgráficos com base em uma variável categórica;
- ```nrow()``` retorna o número de linhas de um data frame;
- ```na.omit()``` remove linhas com valores NA (ausentes) de um data frame;
- ```data.frame()``` cria um data frame a partir de vetores ou listas;
- ```cor()``` matriz de correlação, mede a força e direção da relação entre variáveis numéricas;
- ```which()``` retorna os índices dos elementos que atendem a uma condição lógica especificada.


### Estruturas de repetição
```
for(i in intervalo) {
     # código
}
```
```
while(condição lógica) {
     # código
}
```

### Estruturas condicionais
```
if(condição lógica) {
     # código
} else if (condição lógica) {
     # código
} else {
     # código
}
```

### Declarando funções
```nome_funcao <- function() {código}```

### Gráficos ggplot
- Barra: ```geom_bar()```
- Ponto: ```geom_point()```
- Boxplot: ```geom_boxplot()```
- Histograma: ```geom_histogram()```


### Treino e teste (passo a passo)
1) Aleatorizar a ordem dos dados com sample;
2) Definir n como 80% do total de dados;
3) 80% para treino;
4) 20% para teste;
5) Para KNN: padronizar os dados com scale;
6) Analisar gráficos do treino;
7) Classificar dados do teste.


### Bibliotecas
- ggplot2: criação de gráficos, ```ggplot()```;
- class: realiza classificações, ```knn()```;
- rpart: criação de árvores de decisão, ```rpart()```;
- rpart.plot: visualização de árvores de decisão, ```rpart.plot()```;
- randomForest: criação de múltiplas árvores de decisão, ```randomForest()```;

