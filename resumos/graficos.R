#CRIAÇÃO DE GRÁFICOS COM BIBLIOTECA GGPLOT2

library(ggplot2)

ggplot(data = conjuntodedados, aes ( x = ..., y = ...)) + 
  geom_bar(color = "red", fill = "blue")+
  theme_minimal()

aes indica o que vai ficar no eixo x e no eixo y do gráfico, podemos tirar algum deles e deixar
só com x ou y...



ggplot(data = titanic, aes(x = Survived, fill = Sex))+
  geom_bar()+
  scale_fill_manual(values = c("male" = "#5B99C2", "female" = "#F9DBBA"))+
  theme_minimal()

#fill em aes quer fazer divisão entre sexo entre os que sobreviveram
#scale fill manual permite escolher cores da escala de acordo com valores, ou seja, machos e fêmeas
#serão representados de forma diferente


ggplot(data = titanic, aes(x = Sex, fill = Survived))+
  geom_bar()+
  scale_fill_manual(values = c("0" = "#5B99C2", "1" = "#F9DBBA"),
                    labels = c("0" = "não", "1" = "sim"))+
  facet_wrap(~Pclass)+
  labs(titles = "Análise de sobrevivência de homens e mulheres por classe",
       fill = "Sobreviveu")+
  scale_x_discrete(labels = c("female" = "mulher", "male" = "homem"))+
  theme_classic()

#~ é em função de.
# facet_wrap divide em faces, utilizando outra variável
# labs permite adicionar título
#labels permite mudar nome dos elem. da legenda dentro de scale fill
#fill dentro de labs muda nome da legenda
