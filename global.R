# global

#setwd("C:/Users/Erick/Desktop/ProcessoSeletivoOper")
library(rsconnect)
library(e1071)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)

dados <- as.tibble(read.csv2("consumo_cerveja.csv", sep = ";", dec = ","))

# mudando a categoria de algumas variaveis:

dados <- dados %>% mutate(Data = dmy(Data),
                          fds = as_factor(fds))

summary(dados) # Estatatisticas basicas para uma primeira visualizacao dos dados

# Como possuimos um conjunto de dados anual, vamos primeiro analisar a distribuicao do consumo de cerveja
# ao longo do ano. Para facilitar a visualizacao e a interpretacao desses dados, iremos esbocar uma media
# movel simples na variavel "consumo de cerveja em litros" durante todo o ano de 2019.

# media movel
mm <- stats::filter(dados$Consumo_de_cerveja_lt, filter = rep(1/15, 15), method = "convolution", sides = 1, circular = T)

# Tabela que servira de base para a construcao do grafico da media movel
gmm <- stack(list(
  consumo = dados$Consumo_de_cerveja_lt,
  MM = as.numeric(mm)
))

# Adicionando o vetor Data a tabela
gmm$data <- ymd(dados$Data)

# Plotando o grafico

  grafico1 <- ggplot(gmm, aes(x=data, y=values, colour = ind)) +
                geom_line(size = 0.90) + 
                ggtitle("Consumo de Cerveja durante o Ano de 2019") +
                xlab("Data") + ylab("Consumo de Cerveja (l)") +
                scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
                theme_light()

# Criando funcao para plotar o grafico no dash posteriormente
  
plot.grafico1 <- function(df, x, y) {
  
  out3 <- ggplot(df, aes(x=x, y=y, colour = ind)) +
    geom_line(size = 0.5L) + 
    ggtitle("Media Movel do Consumo de Cerveja em 2019") +
    xlab("Data") + ylab("Consumo de Cerveja (l)") + labs(colour = " ") +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
    theme_classic()
  
  return(out3)
  
}  
  
plot.grafico1(gmm, gmm$data, gmm$values)

# Como podemos observar no grafico, o intervalo de maior consumo acontece entre janeiro e fevereiro
# duminui entre maio e setembro, e volta a crescer em outubro. 
# A grande vantagem das medias moveis eh nos permitir identificar tendencias. Nesse grafico por exemplo,
# podemos perceber que os meses de maior consumo, sao tambem os meses das estacoes mais quentes
# por exemplo, entre janeiro e fevereiro de 2019 era verao, e entre junho e setembro, era inverno, periodo
# em que a media de consumo diminuiu.

# Como associamos o consumo de cerveja as estacoes do ano, podemos levantar a seguinte questao:
# Sera que a temperatura influencia no consumo de cerveja?
# Para isso, testaremos estatisticamente duas hipoteses
# H0: correlacao entre consumo e temperatura eh  = 0
# H1: correlacao entre consumo e temperatura eh != 0

# Teste de correlacao de Pearson

cor.test(
  x = dados$Temperatura_Media,
  y = dados$Consumo_de_cerveja_lt,
  method = "pearson"
)

# Criando objeto que realiza o teste para saida no dash


cor_pearson <- cor.test(
  x = dados$Temperatura_Media,
  y = dados$Consumo_de_cerveja_lt,
  method = "pearson"
)

cor_pearson



# com um p-valor siginificativo, podemos rejeitar H0
# as variaveis apresentam uma correlacao positiva moderada

# Vamos observar o comportamento grafico das duas variaveis


library(ggplot2)

grafico2 <- ggplot(dados) +
              aes(x = Temperatura_Media, y = Consumo_de_cerveja_lt) +
              geom_point(size = 1L, colour = "darkblue") +
              geom_smooth(method = lm) +
              ggtitle("Consumo x Temperatura") +
              xlab("Temperatura Media (C)") + ylab("Consumo de Cerveja (l)") +
              theme_light()

# criando funcao para plotar o grafico no dash

plot.grafico2 <- function(df, x, y) {
  
  out_rg <- ggplot(df) +
    aes(x = x, y = y) +
    geom_point(size = 0.5L, colour = "darkblue") +
    geom_smooth(method = lm) +
    ggtitle("Consumo x Temperatura") +
    xlab("Temperatura Media (C)") + ylab("Consumo de Cerveja (l)") +
    theme_classic()
  
  return(out_rg)
}

plot.grafico2(dados, dados$Temperatura_Media, dados$Consumo_de_cerveja_lt)

# Segundo grafico de regressão que sera exibido no dash

plot.grafico5 <- function(df, x, y) {
  
  out_rg2 <- ggplot(df) +
    aes(x = x, y = y, colour = fds) +
    geom_point(size = 0.5L) +
    geom_smooth(method = lm, colour = "black") +
    ggtitle("Consumo x Temperatura") +
    xlab("Temperatura Media (C)") + ylab("Consumo de Cerveja (l)") +
    theme_classic()
  
  return(out_rg2)
}

plot.grafico5(dados, dados$Temperatura_Media, dados$Consumo_de_cerveja_lt)


# Graficamente conseguimos visualizar que de fato ha uma tendencia linear positiva entre as variaveis, ou seja
# a medida que a temperatura aumenta, o consumo de cerveja tambem aumenta.

# Podemos tambem aplicar um filtro ao grafico de dispersao Consumo x Temperatura para diferenciar os dias uteis
# dos finais de semana

# Grafico Consumo x Temperatura

library(ggplot2)

grafico3 <- ggplot(dados) +
              aes(x = Temperatura_Media, y = Consumo_de_cerveja_lt, colour = fds) +
              geom_point(size = 1L) +
              scale_color_hue() +
              ggtitle("Consumo x Temperatura") +
              xlab("Temperatura Media (C)") + ylab("Consumo de Cerveja (l)") +
              theme_light()

# Criando uma funcao que reproduz o grafico no dash

plot.grafico3 <- function(df, x, y) {
  
  out1 = ggplot(df) +
    aes(x = x, y = y) +
    geom_point(size = 1L, colour = "#FF9999") +
    ggtitle("Consumo x Temperatura") +
    xlab("Temperatura Media (C)") + ylab("Consumo de Cerveja (l)") + 
    theme_classic()
  
  return(out1)
  
}

plot.grafico3(dados, dados$Temperatura_Media, dados$Consumo_de_cerveja_lt)


# Podemos notar no grafico que o consumo de cerveja eh maior aos finais de semana.
# Para uma visualizacao mais direta, podemos recorrer a visualizacao pelo boxplot

library(ggplot2)

grafico4 <- ggplot(dados) +
              aes(x = fds, y = Consumo_de_cerveja_lt) +
              geom_boxplot(fill = "orange") +
              ggtitle("Consumo de Cerveja", subtitle = "Comparacao: Dias Uteis = 0; Final de Semana = 1") +
              xlab("Dia da semana") + ylab("Consumo de Cerveja (l)") +
              theme_minimal()

# Criando funcao geradora do grafico 4 para o dash

plot.grafico4 <- function(df, x, y) {
  
  out2 <- ggplot(df) +
    aes(x = x, y = y) +
    geom_boxplot(fill = "orange") +
    ggtitle("Consumo de Cerveja", subtitle = "Comparacao: Dias Uteis = 0; Final de Semana = 1") +
    xlab("Dia da semana") + ylab("Consumo de Cerveja (l)") +
    theme_classic()

  return(out2)  
}

plot.grafico4(dados, dados$fds, dados$Consumo_de_cerveja_lt)

# Pelo boxsplot conseguimos ver com clareza que o consumo medio de cerveja aos final de semana e mais alto
# em comparacao aos dias uteis.

# Como os graficos indicam uma forte associacao entre o consumo de cerveja e os finais de semana,
# vou propor aqui uma dinamica.
# Dividiremos o nosso banco de dados em duas partes, usaremos a primeira para criar um modelo de Regressao.
# depois usaremos o modelo criado para adivinhar, na segunda parte do conjunto de dados
# se aquele determinado dia da semana foi fim de semana, ou dia util, com base no consumo de cerveja.


# dividindo o nosso banco em duas partes

jannov <- dados[1:334,] # janeiro a novembro

dezembro <- dados[335:365,] # Dezembro

#install.packages(c("readr","mfx","caret","pRoc","ResourceSelection","modEvA","foreign","stargazer"))

m1 <- glm(fds~Consumo_de_cerveja_lt, family = binomial(link = "logit"), data = jannov)
summary(m1)

# A nossa variavel de interesse a o consumo de cerveja que obteve coeficiente 3.202e-04. Pelo fato de ser 
# positivo, nos informa que quando o consumo de cerveja se eleva, elevam-se as chances de o dia em questao
# ser um final de semana.
# De igual forma, nota-se que ha significancia estatistica a p = 0,001 na utilizacao da variavel
# Consumo de Cerveja (l) para o modelo, mostrando que possui importancia ao modelo de regressao proposto.

# Porem, o modelo da regressao logistica tras os resultados dos estimadores na forma logaritma, ou seja,
# o log das chances da variavel Consumo de Cerveja(l) no modelo e 3.202e-04.
# No entanto, para uma interpretacao mais enriquecida da relacao do Consumo, com o dia da semana em questao
# e necessaria a transformacao deste coeficiente, ou seja, que seja efetuada a exponenciacao da variavel
# de regressao. Assim obteremos a razao das chances.

require(mfx)
logitor(fds~Consumo_de_cerveja_lt, data = jannov)

# Criando objeto que sera usado no dash
razoes <- logitor(fds~Consumo_de_cerveja_lt, data = jannov)

# A razao de chances observada  da variavel consumo foi 1.00032025, que pode assim ser interpretado:
# para cada variacao unitaria no Consumo de cerveja, as chances de o dia em questao ser um final de semana
# aumentam em 1.00032025 vezes. 

# Parece pouco, porem estamos lidando com uma grandeza na casa dos milhares, logo, o aumento no consumo, 
# sempre ocorre substancialmente.

# Intervalo de confianca 
# A determinacao do intervalo de confianca do modelo proposto e relevante para que seja analizada a 
# estimativa do intervalo de predicao do coeficiente da variavel independente, a um nivel de confianca 
# de 95%. Desta forma, em 95% dos casos, o parametro dos coeficientes estara dentro deste intervalo.

exp(cbind(OR=coef(m1), confint(m1)))

# Criando o objeto que sera usado no dash

intervalo <- exp(cbind(OR=coef(m1), confint(m1)))

# O coeficiente do nosso consumo, assume o valor 1.000320, podendo variar de 1.000247 ate 1.000401

# Predicao de probabilidades

dezembro$predicao <- round(predict(m1, newdata = dezembro, type = "response"), 3) 

# Matriz de confusao e calculo da acuracia

require(caret)

dezembro$resultado <- as.factor(
  ifelse(
    predict(m1,
            newdata = dezembro,
            type = "response")
    >0.7,"1","0"))

confusionMatrix(dezembro$resultado, dezembro$fds, positive = "1")

# cRiando objeto da matriz que será usado no dash

conf_matriz <- confusionMatrix(dezembro$resultado, dezembro$fds, positive = "1")

# criando a tabela que saira no dash

library(rhandsontable)
tabela1 <- rhandsontable(dezembro)

# CONCLUSAO: A matriz de confusao retorna uma excelente acuracia total do modelo em 87%.
# entre 8 dias listados como final de semana nosso modelo acertou 4. E entre os 23 dias uteis, nao acusou
# nenhum falsamente.

# criando func para valuebox que servira de base para o dash
# func vl_media

funcao_vl_media <- function(unica) {
  
  df2 <- dados[dados$fds == unica, ]
  media <- mean(df2$Consumo_de_cerveja_lt)
  media <- round(media, 1)
  return(media)
}

funcao_vl_media(1)
funcao_vl_media(0)

# funcao vl_var

funcao_vl_desv <- function(unica) {
  
  df3 <- dados[dados$fds == unica, ]
  desvio <- sd(df3$Consumo_de_cerveja_lt)
  desvio <- round(desvio, 1)
  return(desvio)
}

funcao_vl_desv(1)
funcao_vl_desv(0)


# shinyapp
library(shiny)
library(shinydashboard)

shinyApp(ui, server)



