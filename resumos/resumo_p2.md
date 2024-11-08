## WEB SCRAPING
**Bibliotecas**
* rvest: Usada para raspar dados.
* dplyr: Usada para manipulação de dados em tabelas.
* tidytext: Usada para manipulação de texto.

**Funções**
* ```read_html()```: Lê o conteúdo HTML de uma página.
* ```html_elements()```: Extrai elementos HTML específicos da página.
* ```html_text2()```: Extrai o texto limpo dos elementos HTML.
* ```unnest_tokens()```: Separa o texto em tokens (palavras).

## CLASSIFICAÇÃO
**Bibliotecas**
* ggplot2: Usada para visualização de dados.
* tidyr: Usada para organizar conjuntos de dados.
* stopwords: Contém listas de palavras comuns a serem ignoradas.
* stringr: Fornece funções úteis para manipulação de strings.

**Funções**
* ```data.frame()```: Cria um data frame.
* ```unnest_tokens()```: Separa o texto em tokens (palavras).
* ```count()```: Conta a ocorrência de cada token.
* ```sort()```: Ordena os tokens por contagem.
* ```top_n()```: Retorna os n tokens mais frequentes.
* ```mutate()```: Cria novas colunas a partir de colunas existentes.
* ```anti_join()```: Remove linhas que não têm correspondência entre duas tabelas.
* ```inner_join()```: Une duas tabelas com base em colunas comuns.
* ```unique()```: Retorna valores únicos em uma coluna.
* ```filter()```: Filtra linhas do data frame.
* ```group_by()```: Agrupa linhas do data frame por uma ou mais colunas.
* ```get_sentiments()```: Obtém a polaridade (positiva/negativa) de cada token.
* ```str_detect()```: Identifica padrões em strings.
* ```cumsum()```: Calcula a soma acumulada de uma coluna.
* ```spread()```: Transforma uma coluna de valores em múltiplas colunas.
* ```regex()```: Usa expressões regulares para manipulação de strings.

## TEOREMA DE BAYES
O Teorema de Bayes é usado para classificar notícias com base em probabilidades. O modelo Naive Bayes (NB) é um exemplo de classificador baseado no Teorema de Bayes.

**Bibliotecas**
* SnowballC: Usada para lematização e radicalização de palavras.
* quanteda: Usada para análise quantitativa de texto.
* quanteda.textmodels: Contém modelos de texto.

**Funções**
* ```tokens()```: Tokeniza o texto em palavras.
* ```tokens_remove()```: Remove tokens específicos (como stopwords).
* ```tokens_wordstem()```: Faz a lematização/radicalização das palavras.
* ```dfm()```: Cria uma matriz de documentos-termos (document-feature matrix).
* ```textmodel_nb()```: Treina um modelo de classificação Naive Bayes.
* ```predict()```: Faz a previsão usando o modelo treinado.

## MODELO LINEAR
A correlação é uma relação entre duas variáveis, representada por pares ordenados.
* Correlação linear negativa: quando X cresce, Y diminui.
* Correlação linear positiva: quando X cresce, Y também cresce.
* Correlação não linear: sem padrão.

**Funções**
* ```cor()```: Calcula o coeficiente de correlação.
* ```lm()```: Gera um modelo de regressão linear.
* ```summary()```: Fornece informações sobre o modelo de regressão linear.
* ```geom_smooth()```: Cria a reta de regressão linear no gráfico.

## INTERVALOS
**Funções**
* ```geom_density()```: Gera uma curva de densidade para aproximar a distribuição normal.
* ```geom_segment()```: Desenha segmentos de reta no gráfico.
* ```geom_vline()```: Desenha linhas verticais no gráfico.

## TESTE DE HIPÓTESE
O Teste de hipótese determina a probabilidade de erros do tipo I (falso positivo) e tipo II (falso negativo).

**Bibliotecas**
* Stat2Data: Contém conjuntos de dados para exemplos.

**Funções**
* ```prop.test()```: Realiza teste de hipótese para proporções.
* ```shapiro.test()```: Realiza o teste de normalidade de Shapiro-Wilk.

**APRENDIZADO NÃO-SUPERVISIONADO**
**Bibliotecas**
* jpeg: Usada para trabalhar com imagens JPEG.

**Funções**
* ```readJPEG()```: Lê uma imagem JPEG.
* ```kmeans()```: Realiza a clusterização de dados usando o algoritmo k-means.
