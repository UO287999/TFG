# ADICIÓN DE OTRAS BASES DE DATOS
## BASE DE DATOS ORIGINAL (WORLD HAPPINESS)
library(readr)
library(readxl)
library(janitor)
library(mice)
library(visdat)
library(outliers)
library(plotly)
library(tidyverse)
library(dplyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
df_original <- read_delim("world_happiness_combined.csv", delim = ";", show_col_types = FALSE)
df_original <- clean_names(df_original)
df_original <- df_original %>%
  mutate(
    gdp_per_capita = as.numeric(gsub(",", ".", gdp_per_capita)),
    social_support = as.numeric(gsub(",", ".", social_support)),
    freedom_to_make_life_choices = as.numeric(gsub(",", ".", freedom_to_make_life_choices)),
    generosity = as.numeric(gsub(",", ".", generosity)),
    perceptions_of_corruption = as.numeric(gsub(",", ".", perceptions_of_corruption))
  )
df_original$happiness_score <- df_original$happiness_score / 100000
## FREEDOM IN THE WORLD
freedom_todos <- read_excel("All_data_FIW_2013-2024 (1).xlsx", sheet = "FIW13-25", skip = 1)
### Las variables que nos pueden llegar a interesar son:
#### Country/Territory (necesitamos el país/territorio)
#### Region (aunque sea muy parecida a la de regional_indicator, nos puede ser de utilidad)
#### c/T (indica si es país o territorio, aunque no se si deberíamos de meternos con el concepto de soberanía)
#### Edition (necesitamos el año)
#### Status (clasifica la libertad del país)
#### PR (derechos políticos) rating y CL (libertades civiles) rating (la otra opción sería elegir PR, CL y Total porque el rating va de 1 a 7)
freedom_final <- freedom_todos %>%
  select(
    `Country/Territory`, # Nombre del país o territorio
    `Region`,            # Región geográfica
    `C/T`,               # Indica si es país (c) o territorio (t)
    `Edition`,           # Año de edición de los datos
    `Status`,            # Clasificación de libertad (Libre, Parcialmente Libre, No Libre)
    `PR rating`,         # Puntuación de derechos políticos (1-7)
    `CL rating`          # Puntuación de libertades civiles (1-7)
  ) %>%
  rename(
    country = `Country/Territory`,
    region = `Region`,
    country_or_territory = `C/T`,
    year = `Edition`,
    status = `Status`,
    political_rights = `PR rating`,
    civil_liberties = `CL rating`
  )
freedom_final <- freedom_final %>%
  filter(year >= 2015 & year <= 2024)
## DEMOCRACY DATA ==> DEMOCRACY AND DICTATORSHIP
library(democracyData)
democracy_data <-
  democracyData::pacl_update |> 
  janitor::clean_names() |> 
  dplyr::select(
    "country_name" = "pacl_update_country",
    "country_code" = "pacl_update_country_isocode",
    "year",
    "regime_category_index" = "dd_regime",
    "regime_category" = "dd_category",
    "is_monarchy" = "monarchy",
    "is_commonwealth" = "commonwealth",
    "monarch_name",
    "monarch_accession_year" = "monarch_accession",
    "monarch_birthyear",
    "is_female_monarch" = "female_monarch",
    "is_democracy" = "democracy",
    "is_presidential" = "presidential",
    "president_name",
    "president_accesion_year" = "president_accesion",
    "president_birthyear",
    "is_interim_phase" = "interim_phase",
    "is_female_president" = "female_president",
    "is_colony" = "colony",
    "colony_of",
    "colony_administrated_by",
    "is_communist" = "communist",
    "has_regime_change_lag" = "regime_change_lag",
    "spatial_democracy",
    "parliament_chambers" = "no_of_chambers_in_parliament",
    "has_proportional_voting" = "proportional_voting",
    "election_system",
    "lower_house_members" = "no_of_members_in_lower_house",
    "upper_house_members" = "no_of_members_in_upper_house",
    "third_house_members" = "no_of_members_in_third_house",
    "has_new_constitution" = "new_constitution",
    "has_full_suffrage" = "fullsuffrage",
    "suffrage_restriction",
    "electoral_category_index" = "electoral",
    "spatial_electoral",
    "has_alternation" = "alternation",
    "is_multiparty" = "multiparty",
    "has_free_and_fair_election" = "free_and_fair_election",
    "parliamentary_election_year",
    "election_month" = "election_month_year",
    "has_postponed_election" = "postponed_election"
  ) |>
  dplyr::mutate(
    election_month = dplyr::na_if(.data$election_month, "?")
  ) |> 
  tidyr::separate_wider_regex(
    "election_month",
    patterns = c(
      election_month = "\\D+",
      election_year = "\\d{4}$"
    ),
    too_few = "align_start"
  ) |> 
  dplyr::mutate(
    electoral_category = dplyr::case_match(
      .data$electoral_category_index,
      0 ~ "no elections",
      1 ~ "single-party elections",
      2 ~ "non-democratic multi-party elections",
      3 ~ "democratic elections"
    ),
    .after = "electoral_category_index"
  ) |> 
  dplyr::mutate(
    election_month = stringr::str_squish(.data$election_month),
    dplyr::across(
      c(
        tidyselect::ends_with("_index"),
        tidyselect::contains("year"),
        tidyselect::ends_with("_members"),
        "parliament_chambers"
      ),
      as.integer
    ),
    dplyr::across(
      c(
        tidyselect::starts_with("is_"),
        tidyselect::starts_with("has_")
      ),
      as.logical
    )
  )
### Las variables que nos pueden llegar a interesar son:
#### country_name (necesitamos el país/territorio)
#### year (año)
#### regime_category (Parliamentary democracies, Mixed democracies (with weak presidents), Presidential democracies, Civilian autocracies, Military dictatorships, Royal dictatorships)
#### is_monarchy (es monarquía o no)
#### is_democracia (es democracia o no)
#### is_presidential (es presidencial o no)
#### is_colony (es colonia o no)
#### is_communist (es comunista o no)
#### spatial_democracy (como de democráticos son sus países vecinos) 
#### has_full_suffrage (tiene sufragio universal o no)
#### electoral_category (No elections, Single-party elections, non-democratic multi-party elections, democratic elections)
#### spatial_electoral (cómo son las elecciones en sus países vecinos)
#### has_free_and_fair_election (tienen elecciones justas o no)
#### has_alternation (cambian de gobierno o no)
democracy_final <- democracy_data %>%
  select(
    country_name, 
    year, 
    regime_category, 
    is_monarchy, 
    is_democracy, 
    is_presidential, 
    is_colony, 
    is_communist, 
    spatial_democracy, 
    has_full_suffrage, 
    electoral_category, 
    spatial_electoral, 
    has_free_and_fair_election, 
    has_alternation
  )
democracy_final <- democracy_final %>%
  filter(year >= 2015 & year <= 2024)
## UNIÓN DE LOS DATASETS
colnames(df_original)
colnames(democracy_final)
colnames(freedom_final)
# Renombrar la columna 'country_name' en democracy_final para que coincida con df_original
democracy_final <- democracy_final %>%
  rename(country = country_name)
# Ver los países en cada dataset para ver si hay alguna diferencia en la nomenclatura
unique(df_original$country)
unique(democracy_final$country)
unique(freedom_final$country)

setdiff(df_original$country, democracy_final$country)
setdiff(democracy_final$country, df_original$country)
setdiff(df_original$country, freedom_final$country)
setdiff(freedom_final$country, df_original$country)
# Corrección de nombres 
df_original <- df_original %>%
  mutate(country = case_when(
    country == "Trinidad & Tobago" ~ "Trinidad and Tobago",
    country == "Somaliland region" ~ "Somaliland",
    country == "Macedonia" ~ "North Macedonia",
    country == "Palestinian Territories" ~ "State of Palestine",
    country == "Congo" ~ "Congo (Kinshasa)",
    country == "Taiwan Province of China" ~ "Taiwan",
    country == "Hong Kong S.A.R. of China" ~ "Hong Kong",
    country == "Czechia" ~ "Czech Republic",
    country == "Turkiye" ~ "Turkey",
    country == "Argelia" ~ "Algeria",
    country == "Eswatini" ~ "Swaziland",
    TRUE ~ country
  ))
democracy_final <- democracy_final %>%
  mutate(country = case_when(
    country == "Slovak Republic" ~ "Slovakia",
    country == "Korea, Republic of" ~ "South Korea",
    country == "Trinidad &Tobago" ~ "Trinidad and Tobago",
    country == "Congo, Dem. Rep." ~ "Congo (Kinshasa)",
    country == "Congo, Republic of" ~ "Congo (Brazzaville)",
    country == "Gambia, The" ~ "Gambia",
    country == "Côte d`Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  ))
freedom_final <- freedom_final %>%
  mutate(country = case_when(
    country == "Northern Cyprus" ~ "North Cyprus",
    country == "The Gambia" ~ "Gambia",
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    country == "Eswatini" ~ "Swaziland",
    TRUE ~ country
  ))

# PRIMER DATAFRAME: df_completo (2015-2020, con todas las variables)
df_completo <- df_original %>%
  left_join(democracy_final, by = c("country", "year")) %>%
  left_join(freedom_final, by = c("country", "year")) %>%
  filter(year >= 2015 & year <= 2020)  # Filtrar solo hasta 2020

# SEGUNDO DATAFRAME: df_sin_democracia (2015-2024, sin democracy_final)
df_sin_democracia <- df_original %>%
  left_join(freedom_final, by = c("country", "year")) %>%
  filter(year >= 2015 & year <= 2024)  # Mantener toda la información de felicidad y libertad

summary(df_completo)
summary(df_sin_democracia)

na_presence_completo <- df_completo %>%
  summarise(across(everything(), ~ any(is.na(.)), .names = "NA_{.col}"))

na_presence_sin_democracia <- df_sin_democracia %>%
  summarise(across(everything(), ~ any(is.na(.)), .names = "NA_{.col}"))

# OPCIÓN 1: ELIMINAR LOS NA
df_completo_sin_na <- df_completo %>% drop_na()
df_sin_democracia_sin_na <- df_sin_democracia %>% drop_na()

# OPCIÓN 2: SUSTITUIR LOS NA
# VARIABLES NUMÉRICAS
df_completo_numeric <- df_completo %>%
  select_if(is.numeric)
df_sin_democracia_numeric <- df_sin_democracia %>%
  select_if(is.numeric)
imputacion_completo <- mice(df_completo_numeric, method = "pmm", m = 5, maxit = 50, seed = 123)
df_completo_pmm <- complete(imputacion_completo)
imputacion_sin_democracia <- mice(df_sin_democracia_numeric, method = "pmm", m = 5, maxit = 50, seed = 123)
df_sin_democracia_pmm <- complete(imputacion_sin_democracia)
df_completo_pmm <- bind_cols(df_completo %>% select(-colnames(df_completo_numeric)), df_completo_pmm)
df_sin_democracia_pmm <- bind_cols(df_sin_democracia %>% select(-colnames(df_sin_democracia_numeric)), df_sin_democracia_pmm)
# VARIABLES CATEGÓRICAS
md.pattern(df_completo_pmm)
vis_miss(df_completo_pmm, sort_miss=TRUE)
na_presence_completo_pmm <- df_completo_pmm %>%
  summarise(across(everything(), ~ any(is.na(.)), .names = "NA_{.col}"))
df_completo_pmm <- df_completo_pmm %>%
  mutate(
    regional_indicator = case_when(
      country == "Greece" ~ "Western Europe",
      country == "Cyprus" ~ "Western Europe",
      country == "Gambia" ~ "Sub-Saharan Africa",
      TRUE ~ regional_indicator
    ),
    is_colony = ifelse(country == "Libya", FALSE, is_colony)
  ) %>%
  filter(!country %in% c("North Cyprus", "Kosovo", "Somaliland", "State of Palestine"))


md.pattern(df_sin_democracia_pmm)
vis_miss(df_sin_democracia_pmm, sort_miss=TRUE)
na_presence_sin_democracia_pmm <- df_sin_democracia_pmm %>%
  summarise(across(everything(), ~ any(is.na(.)), .names = "NA_{.col}"))
df_sin_democracia_pmm <- df_sin_democracia_pmm %>%
  mutate(
    regional_indicator = case_when(
      country == "Greece" ~ "Western Europe",
      country == "Cyprus" ~ "Western Europe",
      country == "Gambia" ~ "Sub-Saharan Africa",
      TRUE ~ regional_indicator
    ),
  ) %>%
  filter(!country %in% c("State of Palestine"))

# ANÁLISIS DE LA BASE DE DATOS
## MAPAS CON LA PUNTUACIÓN DE FELICIDAD
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map %>%
  left_join(df_completo_sin_na, by = c("name" = "country"))
plot_ly(world_map, 
        type = "choropleth",
        locations = ~iso_a3,  # Código de país
        z = ~happiness_score, # Variable a mapear
        text = ~name,         # Texto emergente
        colorscale = "Blues"
) %>%
  layout(title = "Mapa de Felicidad por País (Completo sin NA)")

world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map %>%
  left_join(df_sin_democracia_sin_na, by = c("name" = "country"))
plot_ly(world_map, 
        type = "choropleth",
        locations = ~iso_a3,  # Código de país
        z = ~happiness_score, # Variable a mapear
        text = ~name,         # Texto emergente
        colorscale = "Blues"
) %>%
  layout(title = "Mapa de Felicidad por País (Sin democracia sin NA)")

world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map %>%
  left_join(df_completo_pmm, by = c("name" = "country"))
plot_ly(world_map, 
        type = "choropleth",
        locations = ~iso_a3,  # Código de país
        z = ~happiness_score, # Variable a mapear
        text = ~name,         # Texto emergente
        colorscale = "Blues"
) %>%
  layout(title = "Mapa de Felicidad por País (Completo PMM)")

world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map %>%
  left_join(df_sin_democracia_pmm, by = c("name" = "country"))
plot_ly(world_map, 
        type = "choropleth",
        locations = ~iso_a3,  # Código de país
        z = ~happiness_score, # Variable a mapear
        text = ~name,         # Texto emergente
        colorscale = "Blues"
) %>%
  layout(title = "Mapa de Felicidad por País (Sin democracia PMM)")

## GRÁFICOS POR REGIONES
p <- ggplot(df_completo_sin_na, aes(x = regional_indicator, y = happiness_score, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de Felicidad por Región", x = "Región", y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)

p <- ggplot(df_sin_democracia_sin_na, aes(x = regional_indicator, y = happiness_score, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de Felicidad por Región", x = "Región", y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)

p <- ggplot(df_completo_pmm, aes(x = regional_indicator, y = happiness_score, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de Felicidad por Región", x = "Región", y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)

p <- ggplot(df_sin_democracia_pmm, aes(x = regional_indicator, y = happiness_score, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de Felicidad por Región", x = "Región", y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)

p <- ggplot(df_sin_democracia_pmm, aes(x = region, y = happiness_score, fill = region)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de Felicidad por Región", x = "Región", y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)
# Se ve bastante mejor por region que por regional_indicator

## GRÁFICOS POR PERCEPCIÓN DE CORRUPCIÓN
p <- ggplot(df_completo_sin_na, aes(x = regional_indicator, y = perceptions_of_corruption, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de la Percepción de Corrupción por Región", x = "Región", y = "Percepción de Corrupción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)
p <- ggplot(df_sin_democracia_sin_na, aes(x = regional_indicator, y = perceptions_of_corruption, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de la Percepción de Corrupción por Región", x = "Región", y = "Percepción de Corrupción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)
p <- ggplot(df_completo_pmm, aes(x = regional_indicator, y = perceptions_of_corruption, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de la Percepción de Corrupción por Región", x = "Región", y = "Percepción de Corrupción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)
p <- ggplot(df_sin_democracia_pmm, aes(x = regional_indicator, y = perceptions_of_corruption, fill = regional_indicator)) +
  geom_violin(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribución de la Percepción de Corrupción por Región", x = "Región", y = "Percepción de Corrupción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p)

## VALORES ATÍPICOS
p <- ggplot(df_original, aes(x = factor(year), y = happiness_score)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de Felicidad por Año", x = "Año", y = "Happiness Score")
ggplotly(p)
# De 2015 a 2020 no vemos valores atípicos
# En 2021 vemos un valor atípico de 2.52, en 2022 de 2.40, en 2023 de 1.86 y en 2024 de 1.72 
# Corresponden todos con Afganistán
df_afganistan <- df_sin_democracia_pmm %>% filter(country == "Afghanistan")
ggplot(df_afganistan, aes(x = year, y = happiness_score)) +
  geom_line() +
  geom_point() +
  labs(title = "Evolución del Happiness Score en Afganistán", x = "Año", y = "Happiness Score")
df_region <- df_sin_democracia_pmm %>% filter(regional_indicator == "South Asia")
ggplot(df_region, aes(x = year, y = happiness_score, color = country, group = country)) +
  geom_line() +
  labs(title = "Evolución del Happiness Score en Asia del Sur", x = "Año", y = "Happiness Score")
# Afganistán sufre una gran caída de la felicidad a partir de 2017 y si lo comparamos con países de su región es el que menos puntuación tiene por bastante diferencia
# Esto puede haberse dado por diversos factores, como el constante estado de guerra y conflicto en el que se ha encontrado el país, la presencia de los talibanes y otros grupos armados que han aumentado la violencia y el temor en la población, la enorme tasa de pobreza...
ggplot(df_region, aes(x = year, y = gdp_per_capita, color = country, group = country)) +
  geom_line() +
  labs(title = "Evolución del PIB en Asia del Sur", x = "Año", y = "PIB")
# Es el más bajo, pero tampoco hay mucha diferencia con el resto; evolucionando acorde con sus países vecinos
ggplot(df_region, aes(x = year, y = social_support, color = country, group = country)) +
  geom_line() +
  labs(title = "Evolución del apoyo social en Asia del Sur", x = "Año", y = "Apoyo social")
# Aunque sigue la tendencia de sus vecinos, vemos cómo en el caso de Afganistán el apoyo social baja estrepitosamente hasta el punto de decir que es nulo a partir de 2021; justo cuando los talibanes toman el poder
ggplot(df_region, aes(x = year, y = freedom_to_make_life_choices, color = country, group = country)) +
  geom_line() +
  labs(title = "Evolución de la libertad de los ciudadanos en Asia del Sur", x = "Año", y = "Libertad de los ciudadanos")
# Afganistán es, con gran diferencia, el país de Asia del Sur donde los ciudadanos tienen menos libertad para tomar decisiones; privándoles de una libertad que afecta de manera directa al descontento de los ciudadanos
ggplot(df_region, aes(x = year, y = political_rights, color = country, group = country)) +
  geom_line() +
  labs(title = "Evolución de la privación de derechos políticos en Asia del Sur", x = "Año", y = "Privación de derechos políticos")
# Afganistán es el país donde más se ha privado de sus derechos políticos
ggplot(df_region, aes(x = year, y = civil_liberties, color = country, group = country)) +
  geom_line() +
  labs(title = "Evolución de la privación de libertades civiles en Asia del Sur", x = "Año", y = "Privación de libertades civiles")
# Afganistán vuelve a ser el país con menor libertad civil de todo Asia del Sur