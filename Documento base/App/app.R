library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(lme4)
library(MuMIn)
library(dplyr)
library(tidyr)
library(readr)
library(performance)
library(DHARMa)
library(glmmTMB)

# Leer los datos base
df <- read.csv("df_unificado.csv")

# Variables permitidas como efectos fijos
efectos_fijos_posibles <- c(
  "year", "regional_indicator", "gdp", "support", "life_exp", "freedom", 
  "generosity", "corruption", "status", "political_rights", "civil_liberties",
  "fair_election", "regime_category", "democracy",
  "electoral_category", "presidential", "alternation"
)

ui <- dashboardPage(
  dashboardHeader(title = "Modelos de Felicidad"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Información", tabName = "info", icon = icon("info-circle"), selected=TRUE),
      menuItem("Descriptiva", tabName = "descriptiva", icon = icon("globe")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-line"))
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
                                       choices = c("year", "regional_indicator")),
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
                )
              ),
              
              fluidRow(
                box(infoBoxOutput("aic_analisis"), width = 6),
                box(infoBoxOutput("r2_analisis"), width = 6)
              ),
              fluidRow(
                box(verbatimTextOutput("modelo_formula"), width = 12)
              ),
              fluidRow(
                box(verbatimTextOutput("resumen_modelo"), width = 12)
              ),
              fluidRow(
                box(title = "Validación del modelo (gráficas)", plotOutput("grafico_validacion", height = 300), width = 12)
              ),
              fluidRow(
                box(title = "Validación del modelo (tests)", verbatimTextOutput("test_validacion"), width = 12)
              ),
              fluidRow(
                box(title = "¿Es el modelo válido para predecir?", verbatimTextOutput("modelo_valido"), width = 12, status = "warning")
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
         utilizando datos longitudinales de múltiples países y años (2015–2024). Ha sido desarrollada como parte de un Trabajo de Fin de Grado
         sobre el análisis de datos longitudinales en el ámbito biosanitario."),
                    p("El análisis combina datos del ", strong("World Happiness Report"), " con variables políticas obtenidas de bases como ",
                      em("Freedom in the World"), " y ", em("Democracy Data"), " (fijadas en 2020 y replicadas)."),
                    br(),
                    tags$h4("Fuentes de datos:"),
                    tags$ul(
                      tags$li(a(href = "https://www.worldhappiness.report/data-sharing/", "World Happiness Report", target = "_blank")),
                      tags$li(a(href = "https://freedomhouse.org/report/freedom-world#Data", "Freedom in the World (Freedom House)", target = "_blank")),
                      tags$li(a(href = "https://xmarquez.github.io/democracyData/index.html", "Democracy Data", target = "_blank"))
                    )
                )
              )
              ,
              
              fluidRow(
                box(title = "Pestaña: Descriptiva", width = 12, status = "info", solidHeader = TRUE,
                    p("Permite visualizar la evolución temporal y espacial de las variables más relevantes. Puedes:"),
                    tags$ul(
                      tags$li("Seleccionar hasta 2 variables numéricas para comparar (ej: Happiness Score, GDP, Freedom)."),
                      tags$li("Elegir un año específico para visualizar un mapa mundial interactivo."),
                      tags$li("Filtrar por región y país según el interés del análisis."),
                      tags$li("Pulsar ", strong("Actualizar gráficos"), " para generar las visualizaciones.")
                    )
                )
              ),
              
              fluidRow(
                box(title = "Pestaña: Análisis", width = 12, status = "info", solidHeader = TRUE,
                    p("Aquí puedes construir modelos estadísticos (LMM o GLMM) para analizar cómo influyen diferentes variables en el nivel de felicidad."),
                    p("Funcionalidades disponibles:"),
                    tags$ul(
                      tags$li("Elegir variables como efectos fijos (ej:", code("GDP"), ",", code("support"), ",", code("freedom"),"...)."),
                      tags$li("Añadir efectos aleatorios como ", code("year"), " o ", code("regional_indicator"), "."),
                      tags$li("Filtrar datos por región y país."),
                      tags$li("Visualizar la fórmula del modelo ajustado y sus métricas (ver abajo)."),
                      tags$li("Ver el gráfico con la comparación entre valores reales y estimados, y una predicción para 2025.")
                    ),
                    p("La predicción para 2025 ", strong("solo se muestra si el modelo es válido"), " según los tests de DHARMa (uniformidad, dispersión, outliers).")
                )
              ),
              
              fluidRow(
                box(title = "¿Cómo interpretar AIC y R²?", width = 12, status = "warning", solidHeader = TRUE,
                    tags$ul(
                      tags$li(strong("AIC (Akaike Information Criterion):"), " penaliza modelos complejos y permite comparar el ajuste entre modelos. Valores más bajos indican mejor ajuste relativo."),
                      tags$li(strong("R² marginal:"), " proporción de la varianza explicada por los efectos fijos."),
                      tags$li(strong("R² condicional:"), " proporción de la varianza explicada por los efectos fijos y aleatorios.")
                    )
                )
              ),
              
              fluidRow(
                box(title = "Repositorio del proyecto", width = 12, status = "success", solidHeader = TRUE,
                    p("El código fuente completo de la aplicación, el análisis de datos y la memoria escrita se encuentra disponible en el siguiente repositorio de GitHub:"),
                    tags$a(href = "https://github.com/UO287999/TFG", "Ver repositorio", target = "_blank")
                )
              ),
              
              fluidRow(
                box(title = "Información adicional", width = 12, status = "info", solidHeader = TRUE,
                    tags$ul(
                      tags$li("Autor: ", strong("Pablo Álvarez Arnedo (UO287999)")),
                      tags$li("Universidad de Oviedo – Grado en Ciencia e Ingeniería de Datos"),
                      tags$li("Fecha de la última actualización: ", Sys.Date()),
                      tags$li("Versión de la app: 1.0")
                    )
                )
              )
      )
      
      
    )
  )
)

server <- function(input, output, session) {
  modelo_tipo <- reactiveVal("lmm")
  modelo_valido_flag <- reactiveVal(FALSE)
  validacion_hecha <- reactiveVal(FALSE)
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
    
    # Filtrar por región y país
    data_filtrada <- df %>%
      filter(regional_indicator %in% input$region_analisis,
             if (!is.null(input$pais_analisis) && length(input$pais_analisis) > 0) country %in% input$pais_analisis else TRUE)
    
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
    req(length(input$efectos_fijos) > 0)  # <- obliga a seleccionar al menos uno
    efectos_fijos <- input$efectos_fijos
    efectos_fijos_txt <- paste(efectos_fijos, collapse = " + ")
    
    if (length(input$efectos_aleatorios) == 0) {
      formula_txt <- paste0("happiness_score ~ ", efectos_fijos_txt)
    } else {
      efectos_aleatorios_txt <- paste0("(1 + ", paste(input$efectos_aleatorios, collapse = " + "), " | country)")
      formula_txt <- paste0("happiness_score ~ ", efectos_fijos_txt, " + ", efectos_aleatorios_txt)
    }
    
    as.formula(formula_txt)
  })
  
  
  modelo_ajustado <- eventReactive(
    list(input$ajustar_modelo, input$ajustar_glmm), {
      req(input$efectos_fijos)
      
      datos <- na.omit(datos_filtrados_analisis())
      
      formula <- modelo_formula()
      
      tryCatch({
        if (modelo_tipo() == "lmm") {
          lmer(formula, data = datos)
        } else {
          glmmTMB(formula, data = datos, family = Gamma(link = "log"))
        }
      }, error = function(e) {
        showNotification(paste("Error al ajustar el modelo:", conditionMessage(e)), type = "error")
        NULL
      })
    }
  )
  
  
  output$modelo_formula <- renderPrint({
    req(modelo_formula())
    print(modelo_formula())
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
  
  output$grafico_validacion <- renderPlot({
    req(modelo_ajustado())
    par(mfrow = c(1, 2))
    plot(fitted(modelo_ajustado()), resid(modelo_ajustado()),
         main = "Residuos vs Ajustados", xlab = "Ajustados", ylab = "Residuos")
    abline(h = 0, col = "red")
    qqnorm(resid(modelo_ajustado()), main = "QQ-Plot de residuos")
    qqline(resid(modelo_ajustado()))
  })
  
  output$test_validacion <- renderPrint({
    req(modelo_ajustado())
    
    sim_res <- simulateResiduals(fittedModel = modelo_ajustado(), plot = FALSE)
    
    cat("Test de Uniformidad:\n")
    print(testUniformity(sim_res))
    
    cat("\nTest de Dispersión:\n")
    print(testDispersion(sim_res))
    
    cat("\nTest de Outliers:\n")
    print(testOutliers(sim_res))
  })
  
  output$modelo_valido <- renderPrint({
    req(modelo_ajustado())
    
    sim_res <- simulateResiduals(fittedModel = modelo_ajustado(), plot = FALSE)
    
    res_uniformity <- testUniformity(sim_res)
    res_dispersion <- testDispersion(sim_res)
    res_outliers <- testOutliers(sim_res)
    
    fallos <- c()
    if (res_uniformity$p.value < 0.05) fallos <- c(fallos, "uniformidad")
    if (res_dispersion$p.value < 0.05) fallos <- c(fallos, "dispersión")
    if (res_outliers$p.value < 0.05) fallos <- c(fallos, "outliers")
    
    if (length(fallos) == 0) {
      modelo_valido_flag(TRUE)
      cat("✅ Este modelo es válido para hacer predicciones porque pasa los tests de uniformidad, dispersión y outliers.")
    } else {
      modelo_valido_flag(FALSE)
      cat("❌ Este modelo NO es válido para hacer predicciones.\n")
      cat("Falla en los siguientes aspectos:", paste(fallos, collapse = ", "), "\n")
    }
    
    # Marcamos que la validación se ha completado
    validacion_hecha(TRUE)
  })
  
  
  
  output$grafico_predicciones <- renderPlotly({
    req(modelo_ajustado())
    req(validacion_hecha())
    datos <- datos_filtrados_analisis()
    tipo_pred <- ifelse(modelo_tipo() == "glmm", "response", "link")
    
    # Predicciones para datos observados
    datos_pred <- datos %>%
      mutate(pred = predict(modelo_ajustado(), newdata = ., allow.new.levels = TRUE, type = tipo_pred))
    
    # Datos con realidad y modelo ajustado
    datos_pred_completo <- bind_rows(
      datos %>% select(country, year, happiness_score) %>% mutate(tipo = "real"),
      datos_pred %>% select(country, year, pred) %>% rename(happiness_score = pred) %>% mutate(tipo = "ajustado")
    )
    
    # Si el modelo es válido, añadir predicción para 2025
    if (modelo_valido_flag()) {
      datos_2025 <- datos %>%
        filter(year == max(year)) %>%
        mutate(year = 2025)
      datos_2025$pred <- predict(modelo_ajustado(), newdata = datos_2025, allow.new.levels = TRUE, type = tipo_pred)
      datos_2025 <- datos_2025 %>%
        select(country, year, pred) %>%
        rename(happiness_score = pred) %>%
        mutate(tipo = "pred_2025")
      
      datos_pred_completo <- bind_rows(datos_pred_completo, datos_2025)
    }
    
    p <- ggplot(datos_pred_completo, aes(x = year, y = happiness_score, color = country, group = country)) +
      geom_line(data = subset(datos_pred_completo, tipo == "real"), size = 1) +
      geom_line(data = subset(datos_pred_completo, tipo == "ajustado"), linetype = "dashed", size = 1) +
      theme_minimal() +
      labs(x = "Año", y = "Happiness Score",
           title = "Predicción vs Realidad (incluye 2025 si el modelo es válido)") +
      scale_x_continuous(breaks = seq(min(datos$year), 2025, by = 1)) +
      theme(legend.position = "right")
    
    if (modelo_valido_flag()) {
      p <- p + geom_point(data = subset(datos_pred_completo, tipo == "pred_2025"),
                          shape = 17, size = 3)
    } else {
      p <- p + annotate("text",
                        x = 2023,
                        y = min(datos$happiness_score, na.rm = TRUE) - 0.1,  # baja el texto un poco
                        label = "⚠️ Modelo no válido para predecir 2025",
                        hjust = 1, vjust = 0,
                        size = 4, color = "red")
      
    }
    
    ggplotly(p)
  })
  
  
  
}

shinyApp(ui, server)