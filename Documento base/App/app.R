library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(lme4)
library(MuMIn)
library(dplyr)
library(tidyr)
df <- df_sin_democracia_sin_na
vars_politicas <- c(
  "fair_election", "regime_category", "democracy",
  "electoral_category", "presidential", "alternation"
)

# Variables permitidas como efectos fijos
efectos_fijos_posibles <- c(
  "regional_indicator", "gdp", "support", "life_exp", "freedom", 
  "generosity", "corruption", "status", "political_rights", "civil_liberties",
  "fair_election", "regime_category", "democracy",
  "electoral_category", "presidential", "alternation"
)

# Función auxiliar para verificar variación en variables políticas
pais_con_variacion <- function(data, vars) {
  data %>%
    group_by(country) %>%
    filter(if_all(all_of(vars), ~ length(unique(.)) > 1)) %>%
    pull(country) %>%
    unique()
}

ui <- dashboardPage(
  dashboardHeader(title = "Modelos de Felicidad"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptiva", tabName = "descriptiva", icon = icon("globe")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-line")),
      menuItem("Información", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "descriptiva",
              fluidRow(
                box(selectizeInput("vars_desc", "Selecciona hasta 2 variables:", 
                                   choices = names(df)[sapply(df, is.numeric) & names(df) != "year"], 
                                   multiple = TRUE, selected = "happiness_score", 
                                   options = list(maxItems = 2)), width = 6),
                box(selectInput("año_mapa", "Selecciona el año para el mapa:", 
                                choices = sort(unique(df$year)), selected = max(df$year)), width = 3),
                box(actionButton("actualizar_graficos", "Actualizar gráficos", icon = icon("refresh")), width = 3)
              ),
              fluidRow(
                box(selectInput("region_desc", "Selecciona región:", choices = unique(df$regional_indicator), 
                                selected = "Western Europe", multiple = TRUE), width = 6),
                box(uiOutput("pais_desc_ui"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("grafico_evolucion", height = 300), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("grafico_mapa", height = 400), width = 12)
              )
      ),
      
      # Placeholder para las otras pestañas
      tabItem(tabName = "analisis",
              fluidRow(
                box(checkboxGroupInput("efectos_fijos", "Efectos fijos:",
                                       choices = efectos_fijos_posibles),
                    width = 6),
                
                box(checkboxGroupInput("efectos_aleatorios", "Efectos aleatorios:",
                                       choices = c("country", "year", "regional_indicator")),
                    width = 6)
              ),
              
              fluidRow(
                box(selectInput("region_analisis", "Selecciona región:", 
                                choices = unique(df$regional_indicator), selected = "Western Europe", multiple = TRUE), width = 6),
                box(uiOutput("pais_analisis_ui"), width = 6)
              ),
              fluidRow(
                box(
                  actionButton("ajustar_modelo", "Ajustar LMM", icon = icon("play")),
                  actionButton("ajustar_glmm", "Ajustar GLMM", icon = icon("cogs")),
                  width = 6
                ),
                box(infoBoxOutput("aic_analisis"), width = 4),
                box(infoBoxOutput("r2_analisis"), width = 4)
              ),
              fluidRow(
                box(verbatimTextOutput("modelo_formula"), width = 12)
              ),
              fluidRow(
                box(verbatimTextOutput("resumen_modelo"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("grafico_predicciones", height = 400), width = 12)
              )
              
      )
      ,
      tabItem(tabName = "info",
              fluidRow(
                box(title = "¿Qué hace esta aplicación?", width = 12, status = "primary", solidHeader = TRUE,
                    p("Esta aplicación interactiva permite explorar y modelizar los determinantes de la felicidad a nivel mundial,
                utilizando datos longitudinales de múltiples países y años. Ha sido desarrollada como parte de un trabajo de fin de grado
                sobre el análisis de datos longitudinales en el ámbito biosanitario.")
                )
              ),
              fluidRow(
                box(title = "Pestaña: Descriptiva", width = 12, status = "info", solidHeader = TRUE,
                    p("En esta pestaña se pueden visualizar la evolución temporal y espacial de distintas variables (como 'Happiness Score', 'GDP', 'Freedom'...).
                Puedes seleccionar hasta 2 variables para comparar, un año específico para el mapa y una o varias regiones o países."),
                    p("Una vez selecciones las opciones deseadas, pulsa 'Actualizar gráficos' para que se genere la visualización.")
                )
              ),
              fluidRow(
                box(title = "Pestaña: Análisis", width = 12, status = "info", solidHeader = TRUE,
                    p("Aquí puedes ajustar modelos mixtos personalizados. Escoge las variables que quieras incluir como efectos fijos o aleatorios,
                y filtra los datos por región o país."),
                    p("Una vez configurado, pulsa 'Ajustar modelo'. Se mostrará la fórmula del modelo, sus métricas (AIC, R²) y un gráfico con predicciones,
                incluyendo una estimación para el año 2025.")
                )
              ),
              fluidRow(
                box(title = "Repositorio del proyecto", width = 12, status = "success", solidHeader = TRUE,
                    p("El código fuente completo de la aplicación y el análisis se encuentra disponible en el siguiente repositorio de GitHub:"),
                    tags$a(href = "https://github.com/UO287999/TFG", "Ver repositorio", target = "_blank")
                )
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  modelo_tipo <- reactiveVal("lmm")
  observeEvent(input$ajustar_modelo, {
    modelo_tipo("lmm")
  })
  
  observeEvent(input$ajustar_glmm, {
    modelo_tipo("glmm")
  })
  
  
  observe({
    paises <- df %>% filter(regional_indicator %in% input$region_desc) %>% distinct(country) %>% pull(country)
    updateSelectInput(session, "pais_desc", choices = paises)
  })
  
  output$pais_desc_ui <- renderUI({
    selectInput("pais_desc", "Selecciona país (opcional):", choices = NULL, multiple = TRUE)
  })
  
  output$pais_analisis_ui <- renderUI({
    selectInput("pais_analisis", "Selecciona país (opcional):", choices = NULL, multiple = TRUE)
  })
  
  observe({
    paises <- df %>% filter(regional_indicator %in% input$region_analisis) %>% distinct(country) %>% pull(country)
    updateSelectInput(session, "pais_analisis", choices = paises)
  })
  
  
  datos_filtrados_desc <- eventReactive(input$actualizar_graficos, {
    df %>% filter(regional_indicator %in% input$region_desc,
                  if (!is.null(input$pais_desc) && length(input$pais_desc) > 0) country %in% input$pais_desc else TRUE)
  })
  
  datos_filtrados_analisis <- eventReactive(c(input$ajustar_modelo, input$ajustar_glmm), {
    usa_politicas <- any(input$efectos_fijos %in% vars_politicas)
    
    data_base <- if (usa_politicas) {
      showNotification("Estás usando variables políticas: el análisis se limita a datos hasta 2020", type = "warning")
      df_completo_sin_na
    } else {
      df_sin_democracia_sin_na
    }
    
    # Filtrar por región y país
    data_filtrada <- data_base %>%
      filter(regional_indicator %in% input$region_analisis,
             if (!is.null(input$pais_analisis) && length(input$pais_analisis) > 0) country %in% input$pais_analisis else TRUE)
    
    # Si se usan variables políticas, filtrar países con variación en esas variables
    if (usa_politicas) {
      vars_usadas <- intersect(input$efectos_fijos, vars_politicas)
      paises_validos <- pais_con_variacion(data_filtrada, vars_usadas)
      
      if (length(paises_validos) == 0) {
        showNotification("Ningún país seleccionado tiene variación en las variables políticas elegidas. Ajuste cancelado.", type = "error")
        return(NULL)
      }
      
      data_filtrada <- data_filtrada %>% filter(country %in% paises_validos)
    }
    
    data_filtrada
  })
  
  
  
  output$grafico_evolucion <- renderPlotly({
    req(input$vars_desc)
    datos <- datos_filtrados_desc()
    
    datos_long <- datos %>% 
      pivot_longer(cols = all_of(input$vars_desc), names_to = "variable", values_to = "valor")
    
    p <- ggplot(datos_long, aes(x = year, y = valor, group = country, color = country)) +
      geom_line() +
      facet_wrap(~ variable, scales = "free_y") +
      theme_minimal() +
      scale_x_continuous(breaks = sort(unique(datos$year))) +
      labs(x = "Año", y = "Valor", title = "Evolución temporal") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  
  output$grafico_mapa <- renderPlotly({
    req(input$vars_desc[1])
    datos_año <- df %>% filter(year == input$año_mapa)
    
    plot_ly(data = datos_año,
            type = "choropleth",
            locations = ~country,
            locationmode = "country names",
            z = ~get(input$vars_desc[1]),
            text = ~country,
            colorscale = "Viridis",
            colorbar = list(title = input$vars_desc[1])) %>%
      layout(
        title = list(text = paste("Mapa mundial de", input$vars_desc[1], "en", input$año_mapa),
                     x = 0.5),
        geo = list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = 'natural earth'))
      )
    
  })
  
  modelo_formula <- reactive({
    efectos_fijos <- unique(c("year", input$efectos_fijos))  # year siempre fijo
    efectos_fijos_txt <- paste(efectos_fijos, collapse = " + ")
    
    if (length(input$efectos_aleatorios) == 0) {
      formula_txt <- paste0("happiness_score ~ ", efectos_fijos_txt, " + (1 | country)")
    } else {
      efectos_aleatorios_txt <- paste(input$efectos_aleatorios, collapse = " + ")
      formula_txt <- paste0("happiness_score ~ ", efectos_fijos_txt, " + (1 + ", efectos_aleatorios_txt, " | country)")
    }
    
    as.formula(formula_txt)
  })
  
  modelo_ajustado <- eventReactive(
    list(input$ajustar_modelo, input$ajustar_glmm), {
      req(input$efectos_fijos)
      
      datos <- datos_filtrados_analisis()
      formula <- modelo_formula()
      
      tryCatch({
        if (modelo_tipo() == "lmm") {
          lmer(formula, data = datos)
        } else {
          glmer(formula, data = datos, family = gaussian(link = "identity"))
        }
      }, error = function(e) {
        showNotification("No se pudo ajustar el modelo. Revisa las variables seleccionadas.", type = "error")
        NULL
      })
    }
  )
  
  
  output$modelo_formula <- renderUI({
    req(modelo_formula())
    formula_txt <- paste0("$$", deparse(modelo_formula()), "$$")
    withMathJax(HTML(formula_txt))
  })
  
  output$resumen_modelo <- renderPrint({
    req(modelo_ajustado())
    summary(modelo_ajustado())
  })
  
  
  output$aic_analisis <- renderInfoBox({
    req(modelo_ajustado())
    valoraic <- paste0("AIC: ", round(AIC(modelo_ajustado()), 2))
    infoBox(
      title = "AIC",
      value = valoraic,
      icon = icon("calculator"),
      color = "purple",
      fill = FALSE
    )
  })
  
  output$r2_analisis <- renderInfoBox({
    req(modelo_ajustado())
    r2 <- r.squaredGLMM(modelo_ajustado())
    valor <- paste0("Marginal: ", round(r2[1], 2), " | Condicional: ", round(r2[2], 2))
    infoBox(
      title = "R² marginal / condicional",
      value = valor,
      icon = icon("chart-area"),
      color = "green",
      fill = FALSE
    )
  })
  
  output$grafico_predicciones <- renderPlotly({
    req(modelo_ajustado())
    datos <- datos_filtrados_analisis()
    
    # Predecir para años observados
    datos_pred <- datos %>%
      select(country, year, all_of(input$efectos_fijos), all_of(input$efectos_aleatorios)) %>%
      mutate(pred = predict(modelo_ajustado(), newdata = ., allow.new.levels = TRUE))
    
    # Generar predicción para 2025 (replicando 2024)
    datos_2025 <- datos %>%
      filter(year == max(year)) %>%
      mutate(year = 2025)
    datos_2025$pred <- predict(modelo_ajustado(), newdata = datos_2025, allow.new.levels = TRUE)
    
    # Unificar datos
    datos_pred_completo <- bind_rows(
      datos %>% select(country, year, happiness_score) %>% mutate(tipo = "real"),
      datos_pred %>% select(country, year, pred) %>% rename(happiness_score = pred) %>% mutate(tipo = "ajustado"),
      datos_2025 %>% select(country, year, pred) %>% rename(happiness_score = pred) %>% mutate(tipo = "pred_2025")
    )
    
    # Crear gráfico
    p <- ggplot(datos_pred_completo, aes(x = year, y = happiness_score, color = country, group = country)) +
      geom_line(data = subset(datos_pred_completo, tipo == "real"), size = 1) +
      geom_line(data = subset(datos_pred_completo, tipo == "ajustado"), linetype = "dashed", size = 1) +
      geom_point(data = subset(datos_pred_completo, tipo == "pred_2025"), shape = 17, size = 3) +  # mismo color que país
      theme_minimal() +
      labs(x = "Año", y = "Happiness Score",
           title = "Predicción vs Realidad (incluye 2025)") +
      scale_x_continuous(breaks = seq(min(datos$year), 2025, by = 1)) +
      theme(legend.position = "right")
    
    ggplotly(p)
  })
  
  
}

shinyApp(ui, server)