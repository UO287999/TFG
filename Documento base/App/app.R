library(shiny)
library(ggplot2)
library(lme4)
library(dplyr)
library(shinydashboard)

# Suponemos que el dataset ya está cargado como df
df <- df_sin_democracia_sin_na

ui <- dashboardPage(
  dashboardHeader(title = "Felicidad y Modelos Mixtos"),
  dashboardSidebar(
    selectInput("region", "Selecciona una o varias regiones:", 
                choices = unique(df$regional_indicator), selected = "Western Europe", multiple = TRUE),
    checkboxGroupInput("variables", "Variables independientes:",
                       choices = c("year", "gdp_per_capita", "freedom_to_make_life_choices", "social_support")),
    selectInput("modelo", "Modelo mixto a utilizar:",
                choices = c("Solo intercepto" = "intercept", 
                            "Intercepto + pendiente" = "intercept_slope",
                            "Completo (aleatorios anidados)" = "completo")),
    actionButton("ajustar", "Ajustar modelo")
  ),
  dashboardBody(
    fluidRow(
      box(plotly::plotlyOutput("plot_felicidad"), width = 12),
      box(verbatimTextOutput("modelo_output"), width = 12)
    )
  )
)

server <- function(input, output) {
  
  datos_filtrados <- reactive({
    df %>% 
      filter(regional_indicator %in% input$region) %>%
      mutate(year = as.integer(year))  # Arreglar años decimales
  })
  
  formula_reactiva <- reactive({
    vars <- paste(input$variables, collapse = " + ")
    switch(input$modelo,
           "intercept" = as.formula(paste0("happiness_score ~ ", vars, " + (1 | country)")),
           "intercept_slope" = as.formula(paste0("happiness_score ~ ", vars, " + (1 + year | country)")),
           "completo" = as.formula(paste0("happiness_score ~ ", vars, " + (1 + ", vars, " | country)"))
    )
  })
  
  modelo_reactivo <- eventReactive(input$ajustar, {
    lmer(formula_reactiva(), data = datos_filtrados())
  })
  
  output$modelo_output <- renderPrint({
    req(input$ajustar)
    summary(modelo_reactivo())
  })
  
  output$plot_felicidad <- plotly::renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = year, y = happiness_score, color = country, group = country)) +
      geom_line() +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      labs(title = paste("Felicidad en", paste(input$region, collapse = ", ")),
           x = "Año", y = "Happiness Score") +
      theme_minimal() +
      scale_x_continuous(breaks = pretty(datos_filtrados()$year))
    
    plotly::ggplotly(p)
  })
}

shinyApp(ui, server)

