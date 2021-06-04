# Ui 

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
source("global.R")

ui <- dashboardPage(
  
  dashboardHeader(title = "Consumo de Cerveja da cidade de São Paulo no ano 2019",
                  titleWidth = 650),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-line")),
      menuItem("Estatísticas", tabName = "estatisticas", icon = icon("calculator"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "graficos",
              fluidRow(
                box(width = 3,
                    selectInput(inputId = "select_fds", label = "Final de Semana: 1 = sim; 0 = nao",
                                choices = unique(dados$fds),
                                selected = 1)),
                
                valueBoxOutput(width = 3, outputId = "temperatura_media"),
                valueBoxOutput(width = 3, outputId = "desv_temperatura"),
                box(
                  title = "Sobre:", width = 3, background = "light-blue", collapsible = TRUE, collapsed = TRUE,
                  "Podemos observar que existe uma relação linear entre o Consumo de Cerveja e a Temperatura.
                  Podemos observar também que o Consumo de Cerveja é maior aos finais de semana."
                )),
              
              fluidPage(
                box(
                  plotlyOutput(outputId = "grafico_dispersao")),
                
                tabBox(id = "tabbox", side = "right", height = "250px",
                       tabPanel("Consumo x Temperatura",
                                plotlyOutput(outputId = "grafico_regressao", height = "357px")),
                       tabPanel("Finais de Semana", height = "357px",
                                plotlyOutput(outputId = "grafico_regressao2", height = "357px"))
                  ),
                ),
                
              fluidPage(
                box(width = 12,
                    plotlyOutput(outputId = "grafico_mm")))),
      
      tabItem(tabName = "estatisticas",
              fluidRow(
                box(
                  title = "Correlação de Pearson - Análise e discussão", width = 6, background = "maroon", collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "exp_pearson")),
                box(
                  title = "Teste de Correlação de Pearson", background = "maroon", collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "cor_pearson"))
              ),
              
              fluidRow(
                box(
                  title = "Modelo de Predição - Análise e discussão", width = 6, background = "purple", collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "exp_modelo")),
                box(
                  title = "Modelo de predição", background = "purple", collapsible = TRUE, collapsed = TRUE, 
                  verbatimTextOutput(outputId = "mod_predicao"))
              ),
              
              fluidRow(
                box(
                  title = "Razões de Chance - Análise e discussão", width = 6, background = "olive", collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "exp_razoes")),
                box(
                  title = "Razões de Chance",  background = "olive", collapsible = TRUE, collapsed = TRUE, 
                  verbatimTextOutput(outputId = "razoes_chn"))
                
              ),
              
              fluidRow(
                box(
                  title = "Matriz de Confusão - Análise e discussão", width = 6, background = "yellow", collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "exp_matriz")),
                box(
                  title = "Matriz de Confusão", background = "yellow", collapsible = TRUE, collapsed = TRUE,
                  verbatimTextOutput(outputId = "matriz"))
              ),
              
              fluidRow(
                box(
                  title = "Matriz e Resultado", width = 12,
                  "Abaixo podemos conferir a matriz e a coluna 'resultado' com os valores binários resultantes:",
                  rHandsontableOutput("tabela_mtz")
                )
              )
              )
            )
      )
    )

