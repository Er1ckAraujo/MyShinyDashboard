# Server

library(shiny)
library(shinydashboard)
library(ggplot2)
source("global.R")

server <- function(input, output) {
  
  output$grafico_dispersao <- renderPlotly({
    df <- dados[dados$fds == input$select_fds, ]
    plot <- plot.grafico3(df, df$Temperatura_Media , df$Consumo_de_cerveja_lt)
    ggplotly(plot)
  
    })
  
  output$grafico_regressao <- renderPlotly({
    plot2 <- plot.grafico2(dados, dados$Temperatura_Media, dados$Consumo_de_cerveja_lt)
    ggplotly(plot2)
  })
  
  output$grafico_regressao2 <- renderPlotly({
    plot2.1 <- plot.grafico5(dados, dados$Temperatura_Media, dados$Consumo_de_cerveja_lt)
    ggplotly(plot2.1)
  })
  
  output$grafico_mm <- renderPlotly({
    plot3 <- plot.grafico1(gmm, gmm$data, gmm$value)
    ggplotly(plot3)
  })
  
  output$temperatura_media <- renderValueBox({
    valueBox(
      paste0(funcao_vl_media(input$select_fds), " litros" ), "Consumidos em média", icon = icon("beer"),
      color = "blue"
    )
  })
  
  output$desv_temperatura <- renderValueBox({
    valueBox(
      paste0(funcao_vl_desv(input$select_fds), " litros" ), " Desvio Padrão Amostral", icon = icon("wine-bottle"),
      color = "red"
    )
  })
  
  output$exp_pearson <- renderText({
    "Como associamos o consumo de cerveja à temperatura, 
  podemos levantar a seguinte questão:
    
  Será que a temperatura influencia no consumo de cerveja?
    
  Para isso, testaremos estatisticamente duas hipoteses
    H0: correlacão entre consumo e temperatura é  = 0
    H1: correlacão entre consumo e temperatura é != 0
    
  Com um p-valor siginificativo, podemos rejeitar H0.
  As variaveis apresentam uma correlacao positiva moderada.
  O resultado pode ser visualizado ao lado: "
  })
  
  output$cor_pearson <- renderPrint({
    cor_pearson
  })
  
  output$exp_modelo <- renderText({
    "Na análise gráfica pudemos observar uma relação entre o 
    consumo de cerveja e os finais de semana, baseado nisso,
    propus uma dinâmica para o projeto dividindo o banco de 
    dados em duas partes: A primeira, contém dados no intervalo
    de Janeiro a Novembro. A segunda possui dados de Dezembro.
    Usaremos a primeira parte para criar um modelo de Regressao
    que possa calcular a probabilidade de um dia ser um final de 
    semana ou dia útil baseado no consumo de cerveja do dia em 
    questão. Depois usaremos o modelo criado para 
    adivinhar, na segunda parte do conjunto de dados, se aquele
    determinado dia da semana foi fim de semana ou dia util.
    
    A nossa variável de interesse é o Consumo de Cerveja, e obteve 
    coeficiente 3.202e-04. Pelo fato de ser positivo, nos informa 
    que quando o consumo de cerveja se eleva, elevam-se as chances de 
    o dia em questão ser um final de semana. De igual forma, nota-se 
    que há significância estatística a p = 0,001 na utilização da 
    variável Consumo_de_cerveja_lt para o modelo, mostrando que a 
    variável possui importância ao modelo de regressão proposto."
  })
  
  output$mod_predicao <- renderPrint({
    summary(m1)
  })
  
  output$exp_razoes <- renderText({
    "O Modelo de Regressão logística tras os resultados dos estimadores
    na forma logarítmica. No entanto, para uma interpretação mais 
    enriquecida da relação do consumo com o dia da semana em questão
    é necessário que seja efetuada a exponenciação da variável de 
    regressão. Assim obteremos a razão das chances.
    
    A razão de chances observada da variável consumo foi 1.00032025,
    que pode assim ser interpretada:
    
    Para cada variação unitária no Consumo de cerveja, as chances de
    o dia em questão ser um final de semana aumentam em 1.00032025 vezes."
  })
  
  output$razoes_chn <- renderPrint({
    razoes
  })
  
  output$exp_matriz <- renderText({
    "Adicionei uma coluna no banco de dados Dezembro que contém as 
    probabilidades de cada um dos 31 dias em questão ser um final 
    de semana. Os valores foram calculados usando o modelo 
    esboçado anteriormente.
    
    Para gerar uma variável binária resultante, adicionei um critério
    que considera as probabilidades acima de 70% finais de semana.
    Sendo: 1 = Final de Semana e 0 = Dia útil.
    
    A matriz de confusão esboçada retorna uma excelente acurácia total
    do modelo em 87%. Entre os 8 dias listados como final de semana no
    mes de dezembro, nosso modelo acertou 4. E entre os 23 dias úteis,
    não acusou nenhum falsamente.
    
    A matriz pode ser consultada ao lado:"
  })
  
  output$matriz <- renderPrint({
    conf_matriz
  })
  
  output$tabela_mtz <- renderRHandsontable({
    tabela1
  })
  
  
  
}
