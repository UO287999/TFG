library(shiny)
library(ggplot2)
library(lme4)
library(dplyr)

# Suponemos que el dataset ya está cargado como df
df <- df_sin_democracia_sin_na

ui <- fluidPage(
  titlePanel("Modelos Mixtos para Felicidad"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Selecciona una región:", 
                  choices = unique(df$regional_indicator)),
      checkboxInput("ajustar", "Ajustar modelo mixto", value = FALSE)
    ),
    mainPanel(
      plotOutput("plot_felicidad"),
      verbatimTextOutput("modelo_output")
    )
  )
)

server <- function(input, output) {
  datos_filtrados <- reactive({
    df %>% filter(regional_indicator == input$region)
  })
  
  output$plot_felicidad <- renderPlot({
    ggplot(datos_filtrados(), aes(x = year, y = happiness_score, group = country, color = country)) +
      geom_line() +
      theme_minimal() +
      labs(title = paste("Evolución de la felicidad en", input$region),
           x = "Año", y = "Happiness Score")
  })
  
  output$modelo_output <- renderPrint({
    req(input$ajustar)
    df_modelo <- datos_filtrados()
    modelo <- lmer(happiness_score ~ year + (1 | country), data = df_modelo)
    summary(modelo)
  })
}

shinyApp(ui, server)

