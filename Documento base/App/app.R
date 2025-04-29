library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(lme4)
library(MuMIn)
library(dplyr)

# Dataset
# Asumimos que df_sin_democracia_sin_na ya está cargado como "df"
df <- df_sin_democracia_sin_na

# Variables (según Capítulo 4)
variables_longitudinales <- c("year", "gdp", "support", "freedom")
variables_fijas <- c("life_exp", "corruption", "generosity")

ui <- dashboardPage(
  dashboardHeader(title = "Modelos de Felicidad",
                  dropdownMenu(type = "notifications",
                               notificationItem(text = "Dashboard actualizado", icon = icon("refresh"))
                  )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualización", tabName = "visualizacion", icon = icon("chart-line")),
      menuItem("Modelo", tabName = "modelo", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "visualizacion",
              fluidRow(
                box(selectInput("region", "Selecciona región:", choices = unique(df$regional_indicator), selected = "Western Europe", multiple = TRUE), width = 6),
                box(selectInput("pais", "Selecciona país (opcional):", choices = NULL, multiple = TRUE), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("plot_felicidad"), width = 12)
              )
      ),
      tabItem(tabName = "modelo",
              fluidRow(
                box(checkboxGroupInput("var_long", "Variables longitudinales:", choices = variables_longitudinales, selected = "year"), width = 6),
                box(checkboxGroupInput("var_fijas", "Variables fijas:", choices = variables_fijas), width = 6)
              ),
              fluidRow(
                box(selectInput("modelo_tipo", "Modelo mixto a utilizar:",
                                choices = c("Intercepto aleatorio" = "intercept", 
                                            "Intercepto y pendiente aleatoria (year)" = "intercept_slope",
                                            "Intercepto y efectos aleatorios para todas las variables" = "completo")), width = 6),
                box(actionButton("ajustar", "Ajustar modelo", icon = icon("play")), width = 6)
              ),
              fluidRow(
                infoBoxOutput("aicBox"),
                infoBoxOutput("r2Box"),
                box(verbatimTextOutput("modelo_output"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    paises <- df %>% filter(regional_indicator %in% input$region) %>% distinct(country) %>% pull(country)
    updateSelectInput(session, "pais", choices = paises)
  })
  
  datos_filtrados <- reactive({
    df %>% filter(regional_indicator %in% input$region)
  })
  
  formula_reactiva <- reactive({
    vars <- c(input$var_long, input$var_fijas)
    vars_formula <- paste(vars, collapse = " + ")
    if (input$modelo_tipo == "intercept") {
      as.formula(paste0("happiness_score ~ ", vars_formula, " + (1 | country)"))
    } else if (input$modelo_tipo == "intercept_slope") {
      as.formula(paste0("happiness_score ~ ", vars_formula, " + (1 + year | country)"))
    } else {
      as.formula(paste0("happiness_score ~ ", vars_formula, " + (1 + ", vars_formula, " | country)"))
    }
  })
  
  modelo_reactivo <- eventReactive(input$ajustar, {
    lmer(formula_reactiva(), data = datos_filtrados())
  })
  
  output$modelo_output <- renderPrint({
    req(modelo_reactivo())
    summary(modelo_reactivo())
  })
  
  output$aicBox <- renderInfoBox({
    req(modelo_reactivo())
    infoBox("AIC", round(AIC(modelo_reactivo()), 2), icon = icon("balance-scale"), color = "purple", fill = TRUE)
  })
  
  output$r2Box <- renderInfoBox({
    req(modelo_reactivo())
    r2 <- r.squaredGLMM(modelo_reactivo())
    infoBox("R2 marginal/condicional", paste0(round(r2[1], 2), "/", round(r2[2], 2)), icon = icon("chart-area"), color = "green", fill = TRUE)
  })
  
  output$plot_felicidad <- renderPlotly({
    datos <- datos_filtrados()
    if (!is.null(input$pais) && length(input$pais) > 0) {
      datos <- datos %>% filter(country %in% input$pais)
    }
    
    p <- ggplot(datos, aes(x = year, y = happiness_score, color = country, group = country)) +
      geom_line(size = 1) +
      labs(title = paste("Felicidad en", paste(input$region, collapse = ", ")),
           x = "Año", y = "Happiness Score") +
      theme_minimal() +
      scale_x_continuous(breaks = pretty(datos$year))
    
    if (input$ajustar > 0 && !is.null(input$pais) && length(input$pais) >= 1) {
      modelo <- modelo_reactivo()
      nueva_data <- datos %>% select(country, year, all_of(c(input$var_long, input$var_fijas)))
      nueva_data$pred <- predict(modelo, newdata = nueva_data)
      p <- p + geom_line(data = nueva_data, aes(x = year, y = pred, color = country, group = country),
                         linetype = "dashed", size = 0.8)
    }
    
    
    ggplotly(p)
  })
}

shinyApp(ui, server)