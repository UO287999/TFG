# Carga de paquetes 
library(readr)
library(readxl)
library(janitor)
library(mice)
library(visdat)
library(outliers)
library(plotly)
library(tidyverse)

# Cargamos los datos 
# Cargar la base de datos
df <- read_delim("world_happiness_combined.csv", delim = ";", show_col_types = FALSE)
# Limpiar nombres de columnas
df <- clean_names(df)
summary(df)
str(df)
# Hay algunas variables numérias que están puestas como categóricas
df <- df %>%
  mutate(
    gdp_per_capita = as.numeric(gsub(",", ".", gdp_per_capita)),
    social_support = as.numeric(gsub(",", ".", social_support)),
    freedom_to_make_life_choices = as.numeric(gsub(",", ".", freedom_to_make_life_choices)),
    generosity = as.numeric(gsub(",", ".", generosity)),
    perceptions_of_corruption = as.numeric(gsub(",", ".", perceptions_of_corruption))
  )
# Verificar que ahora sean numéricas
str(df)
# Hay un problema con las comas en el happiness score
df$happiness_score <- df$happiness_score / 100000

# Datos faltantes
is.na(df)
# En la base de datos no hay ningún dato faltante

# Detección de Outliers
boxplot(df$happiness_score)
boxplot(df$gdp_per_capita)
boxplot(df$social_support)  
boxplot(df$healthy_life_expectancy)
boxplot(df$freedom_to_make_life_choices)
boxplot(df$generosity)
boxplot(df$perceptions_of_corruption)
hist(df$perceptions_of_corruption)
# Como podemos apreciar gráficamente, hay algunas variables numéricas en las que puede haber valores atípicos
outliers::grubbs.test(df$happiness_score)
outliers::grubbs.test(df$gdp_per_capita)
outliers::grubbs.test(df$social_support)
outliers::grubbs.test(df$healthy_life_expectancy)
outliers::grubbs.test(df$freedom_to_make_life_choices)
outliers::grubbs.test(df$generosity)
outliers::grubbs.test(df$perceptions_of_corruption)
# Como el p-valor es > alpha en todos los test, no tenemos outliers

# Ahora, vamos a hacer RLM
# Paquetes necesarios -----
library(ggplot2) 
library(ggfortify)
library(GGally)
library(lmtest)
library(tseries)
library(corrplot)
library(car)

# Planteamiento del modelo ----)
plot(df) # Hay muchas variables categóricas de las cuales nos desharemos
df_filtrado <- df %>%
  filter(year == 2024) %>%
  select(happiness_score, gdp_per_capita, social_support, healthy_life_expectancy, 
         freedom_to_make_life_choices, generosity, perceptions_of_corruption)
df_filtrado_años <- df %>%
  filter(year == 2024) %>%
  select(happiness_score, gdp_per_capita, social_support, healthy_life_expectancy, 
         freedom_to_make_life_choices, generosity, perceptions_of_corruption)
plot(df_filtrado)
apply(df_filtrado, 2, shapiro.test) #el 2 es para aplicar por columnas
# Vemos como no hay normalidad en ninguna, pero como la muestra es grande podemos utilizar Pearson
round(cor(df_filtrado, method='pearson'),4) # redondeo a 4 decimales
# Me fijo en la primera columna porque quiero ver la relación de ventas con el resto de variables explicativas
# Parece ser que las variables que mejores predictoras serán de happines_score son social_support, gdp_per_capita y healthy_life_expectancy
cor.mtest(df_filtrado, method='pearson')
# Obtengo 3 matrices distintas: la primera tiene el p-valor asociado, la segunda el extremo inferior del intervalo de confianza y la tercera el extremo superior del intervalo de confianza
round(cor.mtest(df_filtrado, method='pearson')$p,4) # para ver el p-valor redondeado
ggpairs(df_filtrado)
ggpairs(df_filtrado, upper=list(continuous="cor", corMethod ="pearson"))
# Lo más probable es que generosity no sea un buen predictor de la felicidad

# Estimación de los parámetros ----
fit <- lm(happiness_score~gdp_per_capita+social_support+healthy_life_expectancy+freedom_to_make_life_choices+generosity+perceptions_of_corruption, data=df_filtrado)
fit
# Hay algún parámetro parecido a 0; por lo que tenemos que ver si nos sobra algún parámetro.

# Comparación y selección de modelos ----
## Multicolinealidad
vif(fit)
# Como todos menores que 5, no hay problema de multicolinealidad y no vamos a quitar ninguna de las variables, aunque hay alguno que se acerca a 5

## Selección de variables explicativas
summary(fit)
### AIC y BIC
#### AIC
step(fit, k=2)
#### BIC
step(fit, k=log(nrow(df_filtrado)))
# Empiezo con el modelo saturado, va probando a sacar de una en una las variables. Nos interesa que el AIC sea lo más pequeño posible.
# El modelo final sería la felicidad en función del apoyo social, la esperanza de vida, el libre albedrío y la percepción de corrupción
fit <- lm(happiness_score~social_support+healthy_life_expectancy+freedom_to_make_life_choices+perceptions_of_corruption, data=df_filtrado)
fit.f <- step(fit, trace=0)
# Por defecto est? eligiendo el mejor modelo por AIC, aunque en este caso AIC y BIC devuelven el mismo modelo

# Adecuaci?n del modelo
summary(fit)
# El valor del estad?stico F (859.6) y el p-valor < alpha nos permiten concluir que podemos rechazar H0 y, por tanto, el modelo explica los datos. 
# Adem?s, el valor del R2 (0.8134) nos indica que el 81.34% de la varianza de la felicidad es explicada por las variables

# Diagn?stico del modelo
## An?lisis de los residuos
autoplot(fit)
# Parece que los valores de los residuos observados se alejan bastante de los residuos te?ricos

### Normalidad ==> NO SE CUMPLE
jarque.bera.test(fit$residuals)
# Los residuos no siguen una distribuci?n normal, por lo que la normalidad no se cumple

### Media cero ==> SE CUMPLE
mean(fit$residuals)
# Mirando la primera gr?fica, podemos ver como la gr?fica se parece un poco al eje de abscisas, aunque no haya un comportamiento perfectamente lineal de los datos; por lo que diría que la media cero  se cumple

### Homocedasticidad ==> NO SE CUMPLE
# No se mantiene muy constante, por lo que no parece que haya homocedasticidad
bptest(fit, studentize = FALSE)
# Viendo el p-valor, rechazamos H0, por lo que no tenemos varianza constante

### No correlaci?n ==> SE CUMPLE
# Parece ser que no siguen una fluctuaci?n aleatoria del todo
dwtest(fit, alternative='two.sided')
# Como el p-valor > alpha, no rechazamos H0, por lo que los residuos no est?n autocorrelacionados
# Por todo esto, el modelo no es fiable.