# LME4
library(lme4)
## lmer ==> ajustar un modelo mixto
lmer(formula, data = NULL, REML = TRUE, control = lmerControl(),
     start = NULL, verbose = 0L, subset, weights, na.action,
     offset, contrasts = NULL, devFunOnly = FALSE, ...)
### Modelo normal con intercepto aleatorio
# Generación de datos
ni <- 50
G <- 10
nobs <- ni * G
grupo <- factor(rep(x=1:G, each=ni))
obs <- rep(x=1:ni, times=G)
set.seed(1234567)
x <- runif(n=nobs, min=-5, max=6)
set.seed(1234567)
b0 <- rnorm(n=G, mean=0, sd=sqrt(625)) # Intercepto aleatorio
b0 <- rep(x=b0, each=ni)               # El mismo intercepto aleatorio pero repetido
media <- 4 - 6 * x + b0
set.seed(1234567)
y <- rnorm(n=nobs, mean=media, sd=sqrt(16))
datos <- data.frame(obs, grupo, b0, x, media, y)
# Exploración de datos
library(rmarkdown)
paged_table(datos, options = list(rows.print = 6))
# Dibujo de datos
library(ggplot2)
ggplot(datos, aes(x, y, color=grupo) ) + 
  geom_point() + 
  labs(colour="Grupo/Cluster") # Misma pendiente
# Estimación de modelos
library(lme4)
fit1 <- lmer(y ~ x + (1 | grupo), data = datos)
summary(fit1)

## glmer ==> ajustar modelos lineales generalizados mixtos
glmer(formula, data = NULL, family = gaussian, control = glmerControl(), 
      start = NULL, verbose = 0L, nAGQ = 1L, subset, weights, na.action, 
      offset, contrasts = NULL, mustart, etastart, devFunOnly = FALSE) 
### Modelo gamma con intercepto aleatorio
# Generación de datos
rgamma_glm <- function(n, mu, phi) {
  x <- rgamma(n=n, shape=1/phi, scale=mu*phi)
  return(x)
}
ni <- 20
G <- 10
nobs <- ni * G
grupo <- factor(rep(x=1:G, each=ni))
obs <- rep(x=1:ni, times=G)
set.seed(123456)
x <- runif(n=nobs, min=0, max=1)
set.seed(123456)
b0 <- rnorm(n=G, mean=0, sd=sqrt(9)) # Intercepto aleatorio
b0 <- rep(x=b0, each=ni)             # El mismo intercepto aleatorio pero repetido
media <- exp(2 - 8 * x + b0)
set.seed(123456)
y <- rgamma_glm(n=nobs, mu=media, phi=0.5)
datos <- data.frame(obs, grupo, b0, x, media, y)
# Exploración de datos
paged_table(datos, options = list(rows.print = 6, cols.print=6))
# Dibujo de datos
ggplot(datos, aes(x, y, color=grupo) ) + 
  geom_point() + 
  labs(colour="Grupo/Cluster") # casi todos sobre 0
# Estimación de parámetros
fit2 <- glmer(y ~ x + (1 | grupo), family=Gamma(link="log"), data = datos)
summary(fit2)

## Aplicación con lme4 (con ejemplo)
head(sleepstudy)
ggplot(data = sleepstudy, aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ Subject) + labs(y = "Reaction time") + 
  theme(legend.position = "none") # el tiempo de reacción promedio varía según los individuos
# Modelo con intercepto y pendiente como efectos aleatorios
fit <- lmer(Reaction ~ Days + (Days | Subject), REML = TRUE, data = sleepstudy)
summary(fit)
ranef(fit) # intercepto y pendiente
fixef(fit) # efectos fijos
coef(fit) # efectos fijos y aleatorios para cada individuo
fit <- lmer(Reaction ~ Days + (Days | Subject), REML = TRUE, data = sleepstudy)
sleepstudy$pred_inter_pend_aleatorio <- predict(fit)
ggplot(data = sleepstudy, aes(x = Days, y = pred_inter_pend_aleatorio, color = Subject)) +
  geom_line() +
  geom_point(aes(x = Days, y = Reaction, color = Subject)) +
  geom_abline(intercept = 251.40, slope = 10.47, color = "black", linetype = "dashed", linewidth = 0.5) +
  theme_bw() +
  facet_wrap(~ Subject) + 
  theme(legend.position = "none") + labs(y = "Reaction time")

# NLME
## lme ==> ajustar un modelo mixto
lme(fixed, data, random, correlation, weights, subset, method,
    na.action, control, contrasts = NULL, keep.data = TRUE)
### Modelo normal con intercepto aleatorio
# Generación de datos
ni <- 50
G <- 10
nobs <- ni * G
grupo <- factor(rep(x=1:G, each=ni))
obs <- rep(x=1:ni, times=G)
set.seed(1234567)
x <- runif(n=nobs, min=-5, max=6)
set.seed(1234567)
b0 <- rnorm(n=G, mean=0, sd=sqrt(625)) # Intercepto aleatorio
b0 <- rep(x=b0, each=ni)               # El mismo intercepto aleatorio pero repetido
media <- 4 - 6 * x + b0
set.seed(1234567)
y <- rnorm(n=nobs, mean=media, sd=sqrt(16))
datos <- data.frame(obs, grupo, b0, x, media, y)
# Exploración de datos
paged_table(datos, options = list(rows.print = 6))
# Dibujo de datos
ggplot(datos, aes(x, y, color=grupo) ) + 
  geom_point() + 
  labs(colour="Grupo/Cluster")
# Estimación de parámetros
library(nlme)
fit1 <- lme(y ~ x, random = ~ 1 | grupo, data=datos)
summary(fit1)

## Aplicación con nlme (con ejemplo)
head(Oxboys)
plot(Oxboys)
# Modelo con intercepto y pendiente aleatoria
fit <- lme(height ~ age, random= ~ 1 + age | Subject, data=Oxboys, method="REML")
summary(fit)
ranef(fit) # intercepto y pendiente
fixef(fit) # efectos fijos
coef(fit) # efectos fijos y aleatorios para cada individuo
library(lattice)
xyplot(height ~ age | Subject, data=Oxboys, fit=fit,
       strip=strip.custom(bg="white"), 
       pch=16, cex=0.7, col='indianred1',
       panel = function(x, y, ..., fit, subscripts) {
         panel.xyplot(x, y, ...)
         ypred <- fitted(fit)[subscripts]
         panel.lines(x, ypred, col="deepskyblue3", lwd=1)
       },
       ylab="Height (cm)", xlab="Centered age")