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


# Leer los datos base
df_happiness <- read.csv("../df_sin_democracia_sin_na.csv")
df_politicas <- read.csv("../df_completo_sin_na.csv")

# Variables políticas que se quieren mantener fijas desde 2020
vars_politicas <- c(
  "fair_election", "regime_category", "democracy",
  "electoral_category", "presidential", "alternation"
)

# Extraer valores de 2020 para esas variables
politicas_2020 <- df_politicas %>%
  filter(year == 2020) %>%
  select(country, all_of(vars_politicas))

# Filtrar df_happiness solo para países que tienen datos políticos válidos
paises_validos <- unique(politicas_2020$country)
df_happiness_filtrado <- df_happiness %>%
  filter(country %in% paises_validos)

# Expandir datos políticos de 2020 a todos los años 2015–2024
años <- 2015:2024
base_expansion <- expand.grid(country = paises_validos, year = años)

politicas_expandido <- base_expansion %>%
  left_join(politicas_2020, by = "country")

# Unir ambas fuentes para crear el dataframe completo y limpio
df <- df_happiness_filtrado %>%
  left_join(politicas_expandido, by = c("country", "year"))
write.csv(df, "df_unificado.csv", row.names = FALSE)

# Variables permitidas como efectos fijos
efectos_fijos_posibles <- c(
  "regional_indicator", "gdp", "support", "life_exp", "freedom", 
  "generosity", "corruption", "status", "political_rights", "civil_liberties",
  "fair_election", "regime_category", "democracy",
  "electoral_category", "presidential", "alternation"
)

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
                ),
                box(selectInput("familia_glmm", "Familia (GLMM):",
                                choices = c("Gamma" = "Gamma", "Inverse Gaussian" = "inverse.gaussian"),
                                selected = "Gamma"),
                    width = 6)
                
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
                box(plotlyOutput("grafico_predicciones", height = 400), width = 12)
              ),
              fluidRow(
                box(title = "Validación del modelo (gráficas)", plotOutput("grafico_validacion", height = 300), width = 12)
              ),
              fluidRow(
                box(title = "Validación del modelo (tests)", verbatimTextOutput("test_validacion"), width = 12)
              ),
              fluidRow(
                box(title = "¿Es el modelo válido para predecir?", verbatimTextOutput("modelo_valido"), width = 12, status = "warning")
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
    efectos_fijos <- unique(c("year", input$efectos_fijos))  # year siempre fijo
    efectos_fijos_txt <- paste(efectos_fijos, collapse = " + ")
    
    if (length(input$efectos_aleatorios) == 0) {
      formula_txt <- paste0("happiness_score ~ ", efectos_fijos_txt)
    } else {
      efectos_aleatorios_txt <- paste0("(1 | ", input$efectos_aleatorios, ")", collapse = " + ")
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
          fam <- switch(input$familia_glmm,
                        "Gamma" = Gamma(link = "inverse"),
                        "inverse.gaussian" = inverse.gaussian(link = "inverse"))
          
          glmer(formula, data = datos, family = fam)
          
        }
      }, error = function(e) {
        showNotification("No se pudo ajustar el modelo. Revisa las variables seleccionadas.", type = "error")
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
    
    # Ejecutar los tests
    res_uniformity <- testUniformity(sim_res)
    res_dispersion <- testDispersion(sim_res)
    res_outliers <- testOutliers(sim_res)
    
    # Identificar los que fallan
    fallos <- c()
    if (res_uniformity$p.value < 0.05) fallos <- c(fallos, "uniformidad")
    if (res_dispersion$p.value < 0.05) fallos <- c(fallos, "dispersión")
    if (res_outliers$p.value < 0.05) fallos <- c(fallos, "outliers")
    
    if (length(fallos) == 0) {
      cat("✅ Este modelo es válido para hacer predicciones porque pasa los tests de uniformidad, dispersión y outliers.")
    } else {
      cat("❌ Este modelo NO es válido para hacer predicciones.\n")
      cat("Falla en los siguientes aspectos:", paste(fallos, collapse = ", "), "\n")
    }
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