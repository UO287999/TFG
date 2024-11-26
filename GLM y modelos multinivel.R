# GLM y Modelos multinivel
# Capítulo 8: INTRODUCCIÓN A MODELOS MULTINIVEL
# Packages required for Chapter 8
library(MASS)
library(gridExtra)  
library(mnormt) 
library(lme4) 
library(knitr) 
library(kableExtra)
library(tidyverse)  
library(gitcreds)

# CASO 1: INFLUENCIA DE LA ANSIEDAD EN LAS INTERPRETACIONES DE MÚSICOS
## CARGAMOS LOS DATOS
url <- "https://raw.githubusercontent.com/proback/BeyondMLR/master/data/musicdata.csv"
download.file(url, destfile = "musicdata.csv")
music = read.csv("musicdata.csv")
dim(music)        # should be 497 x 18
head(music)       

## PREPROCESAMIENTO DE DATOS
### NIVEL 1: variables medidas en la unidad de observación más frecuente
### Variables medidas en cada interpretación: ansiedad (Y), características de interpretación, nº actuaciones con diary
### NIVEL 2: variables medidas en unidades de observación más grandes
### Variables constantes: demográficas, instrumentos, medidas MPQ
### Seleccionamos las variables que nos interesan
select <- dplyr::select
keydata <- music %>% 
  dplyr::select(id, diary, perform_type, memory, audience, 
                na, gender, instrument, mpqab, mpqpem, mpqnem)
### Hacemos un dataset para NIVEL 2 escogiendo una observación por sujeto
music.lev2 <-  keydata %>%
  group_by(id) %>%
  filter(row_number() == 1) %>%
  select(id, gender:mpqnem)  
# Añadir media a cada sujeto para las gráficas
meanbysubj <- music %>% group_by(id) %>%
  summarise(meanbysubj = mean(na, na.rm = TRUE))
music.lev2 <- music.lev2 %>%
  left_join(meanbysubj, by = "id")

## ANÁLISIS EXPLORATORIO
### UNIVARIADOS ==> análisis de variables por separado
### Covariables NIVEL 1 (asumimos que son independientes)
music %>% count(perform_type) # 56.1% solos, 27.3% bandas grandes, 16.5% bandas pequeñas
music %>% count(memory) # 30% de memoria, 55.1% con partitura, 14.9% sin especificar
music %>% count(audience) # 41% público, 30% instructores, 20.1% estudiantes y 8.9% jurados
# Preparamos las gráficas 
theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))
# Histograma de las frecuencias de la ansiedad
na.all <- ggplot(data=music,aes(x=na)) + 
  geom_histogram(binwidth = 2, fill = "white",color = "black") + 
  theme.1 + xlim(10,35) +
  xlab("Negative Affect") + ylab("Frequency") + labs(title="(a)") 
na.mean <- ggplot(data=music.lev2,aes(x=meanbysubj)) + 
  geom_histogram(binwidth = 2, fill = "white", 
                 color = "black") + 
  theme.1 + xlim(10,35) +
  xlab("Mean Negative Affect") + ylab("Frequency") + labs(title="(b)") 
mli.hist1 <- grid.arrange(na.all,na.mean,ncol=1)
# Podemos ver que la ansiedad en los músicos suele estar entre un 15 y 20
### Covariables NIVEL 2 (una observación por sujeto)
music.lev2 %>% ungroup(id) %>% count(gender) # 70% mujeres, 30% hombres
music.lev2 %>% ungroup(id) %>% count(instrument) # 46% orquesta, 40.5% voz, 13.5% piano/órgano
# Preparamos las gráficas
nem1 <- ggplot(data=music.lev2,aes(x=mpqnem)) + 
  geom_histogram(binwidth = 5, fill = "white",
                 color = "black") + 
  theme.1 + 
  xlab("NEM") + ylab("Frequency") + labs(title="(a)")
pem1 <- ggplot(data=music.lev2,aes(x=mpqpem)) + 
  geom_histogram(binwidth = 5, fill = "white", 
                 color = "black") + 
  theme.1 + 
  xlab("PEM") + ylab("") + labs(title="(b)")
abs <- ggplot(data=music.lev2,aes(x=mpqab)) + 
  geom_histogram(binwidth = 5, fill = "white", 
                 color = "black ") + 
  theme.1 + 
  xlab("Absorption") + ylab("") + labs(title="(c)")
# Histograma de las covariables de NIVEL 2
mli.histmat1 <- grid.arrange(nem1,pem1,abs,ncol=3)
grid.arrange(nem1,pem1,abs,ncol=3)
# Podemos ver que las emociones positivas de los músicos suelen estar entre 50 y 60, mientras que las negativas están entre 20 y 40

### BIVARIADOS ==> análisis de relaciones entre covariables y respuestas
# NIVEL 1
# Ansiedad vs tipo de actuación
box.perform <- ggplot(data=music,aes(factor(perform_type),na)) +
  geom_boxplot() + 
  theme.1 + coord_flip() + ylab("Negative affect") + 
  xlab("") + labs(title="(a)")
# Podemos ver que la ansiedad es menor cuando se toca con más gente, lo cual es lógico porque la responsabilidad se reparte
# Ansiedad vs audiencia
box.audience <- ggplot(data=music,aes(factor(audience),na)) +
  geom_boxplot() +
  theme.1 + coord_flip() +  ylab("Negative affect") + 
  xlab("") + labs(title="(b)")
# Podemos ver que la ansiedad es menos cuando se toca con un instructor, y mayor cuando se toca con un juez, lo cual tiene sentido ya que con el instructor se aprende y con el juez se evalúa nuestra actuación; por lo que tiene sentido que haya más nervios 
# Ansiedad vs nº actuaciones anteriores
scatter.previous <- ggplot(data=music, aes(x=previous,y=na)) +
  geom_point() + 
  theme.1 + 
  geom_smooth(method="lm",color="black") + 
  ylab("Negative affect") + 
  xlab("Previous Performances") + labs(title="(c)")
# Podemos ver que la ansiedad disminuye a medida que se hacen más actuaciones, ya que se va cogiendo confianza con la experiencia
# Gráfica
mli.boxscatmat1 <- grid.arrange(box.perform,box.audience,scatter.previous,ncol=2)
## DATOS NIVEL 1 PARA CADA SUJETO (asegurar independencia)
# Ansiedad vs tipo de actuación
ggplot(music,aes(x=factor(perform_type),y=na)) + theme.1 + 
  geom_dotplot(binaxis="y",stackdir="center",binwidth=25/30) + 
  facet_wrap(~id,ncol=5) +   
  theme(strip.text.x=element_blank()) + coord_flip() +
  labs(x="Performance Type",y="Negative Affect")
# Podemos ver que la mayoría de los individuos con mayor nivel de ansiedad son aquellos que tocan solos
# Ansiedad vs audiencia
ggplot(music,aes(x=factor(audience),y=na)) + theme.1 + 
  geom_dotplot(binaxis="y",stackdir="center",binwidth=25/30) + 
  facet_wrap(~id,ncol=5) +   
  theme(strip.text.x=element_blank()) + coord_flip() +
  labs(x="Audience",y="Negative Affect")
# Podemos ver que, en general, los niveles más bajos de ansiedad son con instructores, y los más altos con jueces y público
# Ansiedad vs actuaciones anteriores
ggplot(music,aes(x=previous,y=na)) + theme.1 + 
  geom_point() + geom_smooth(method="lm",color="black") + 
  facet_wrap(~id,ncol=5) +   
  theme(strip.text.x=element_blank()) + ylim(10,35) +
  labs(x="Previous Performances",y="Negative Affect")
# A nivel general, se puede decir, igual que antes, que el nivel de ansiedad se reduce a medida que se hacen más actuaciones
## NIVEL 2
# Ansiedad vs instrumentos
instr.all <- ggplot(data=music,aes(factor(instrument),na)) +
  geom_boxplot() + 
  coord_flip() + theme.1 + ylab("Negative Affect") + 
  xlab("") + labs(title="(a)") + ylim(10,35)
instr.mean <- ggplot(data=music.lev2,
                     aes(factor(instrument),meanbysubj)) + 
  geom_boxplot() + coord_flip() + 
  theme.1 + ylab("Mean Negative Affect") + 
  xlab("") + labs(title="(b)") + ylim(10,35)
mli.boxmat1 <- grid.arrange(instr.all,instr.mean,ncol=1)
grid.arrange(instr.all, instr.mean, ncol = 1)
# La ansiedad suele ser mayor para los que tocan instrumentos que para los vocalistas
# Ansiedad vs emociones positivas, negativas y absorción
pem2.all <- ggplot(data=music,aes(x=mpqpem,y=na)) + 
  geom_point() + 
  geom_smooth(method="lm",color="black") + 
  theme.1 + ylab("Negative Affect") + 
  xlab("PEM") + labs(title="(a1)")
nem2.all <- ggplot(data=music,aes(x=mpqnem,y=na)) + 
  geom_point() + 
  geom_smooth(method="lm",color="black") + 
  theme.1 + ylab("") + xlab("NEM") + 
  labs(title="(b1)")
abs2.all <- ggplot(data=music,aes(x=mpqab,y=na)) + 
  geom_point() + 
  geom_smooth(method="lm",color="black") + 
  theme.1 + ylab("") + 
  xlab("Absorption") + labs(title="(c1)")
pem2.mean <- ggplot(data = music.lev2,
                    aes(x = mpqpem, y = meanbysubj)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "black") + 
  theme.1 + ylab("Mean Negative Affect") + 
  xlab("PEM") + labs(title = "(a2)")
nem2.mean <- ggplot(data = music.lev2,
                    aes(x = mpqnem, y = meanbysubj)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "black") + 
  theme.1 + ylab("") + xlab("NEM") + labs(title = "(b2)")
abs2.mean <- ggplot(data = music.lev2,
                    aes(x = mpqab, y = meanbysubj)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "black") + 
  theme.1 + ylab("") + 
  xlab("Absorption") + labs(title="(c2)")
mli.scatmat1 <- grid.arrange(pem2.all, nem2.all, abs2.all,
                             pem2.mean, nem2.mean, abs2.mean, ncol = 3)
grid.arrange(pem2.all, nem2.all, abs2.all,
             pem2.mean, nem2.mean, abs2.mean, ncol = 3)
# Como es de esperar, la ansiedad crece considerablemente con las emociones negativas, y se reduce un poco con las positivas 

## MODELOS DE DOS NIVELES
### Voy a descartar los modelos en los que se asumen que las observaciones son independientes y los de dos estaciones porque tienen algunas debilidades
### MODELO MULTINIVEL UNIFICADO
# Tendremos 2 niveles de modelos, utilizando métodos de estimación de parámetros basados en verosimilitud
# Yij es la ansiedad del sujeto i en su actuación j
# Yij puede definirse como el modelo compuesto por varios efectos (fijosy aleatorios) y un término de interacción entre niveles
# Efectos fijos: niveles de un factor sobre el que queremos hacer inferencias y que no cambiaría en replicaciones del modelo (tipo de actuación)
# Efectos aleatorios: niveles de un factor que puede considerarse una muestra de una población mayor (músicos)
## DISTRIBUCIÓN DE ERRORES: NORMAL MULTIVARIANTE
# NIVEL 1: relación entre interceptos y pendientes
int <- by(music, music$id, function(data)
  coefficients(lm(na ~ ifelse(perform_type=="Large Ensemble",1,0), data = data))[[1]])
rate <- by(music, music$id, function(data)
  coefficients(lm(na ~ ifelse(perform_type=="Large Ensemble",1,0), data = data))[[2]])
ggplot(data=music.lev2,aes(x=int,y=rate)) + 
  geom_point() + 
  geom_smooth(method="lm",color="black") + 
  theme.1 + ylab("Fitted slopes") + 
  xlab("Fitted intercepts")
cor(int,rate, use="complete.obs") # correlación negativa moderada
# Podemos interpretar que los músicos con más ansiedad tienen una mayor reducción de ansiedad cuando actúan en grupos grandes ==> Los individuos con un intercepto mayor (ansiedad cuando tocan sólos o banda pequeña) tendrán menores pendientes (mayor reducción de ansiedad con bandas grandes)
# NIVEL 2: NORMAL MULTIVARIANTE
# Errores NO correlacionados
e0 <- seq(-8,8,length=51)  
e1 <- seq(-4,4,length=51)
xy <- expand.grid(e0,e1)
Sigma <- matrix(c(4,0,0,1),2,2)
Mu <- c(0,0)
z <- dmnorm(xy, Mu, Sigma)
zframe <- data.frame(xy, z)
density <- xy[z==max(z),]
con.1 <- ggplot(data = zframe, aes(x = Var1, y = Var2, z = z)) + 
  theme.1 +
  geom_contour(stat = "contour", lineend = "butt", 
               linejoin = "round", linemitre = 1, 
               na.rm = FALSE, colour = "black") + 
  labs(x = "u", y = "v", title = "(a)") + 
  scale_y_continuous(limits = c(-5,5))
# Se puede ver que los errores son independientes por la forma circular: no hay relación entre sus dispersiones
# Errores correlacionados positivamente
Sigma <- matrix(c(4,1.5,1.5,1),2,2)
Mu <- c(0,0)
z <- dmnorm(xy, Mu, Sigma)
zframe <- data.frame(xy, z)
density <- xy[z==max(z),]
con.2 <- ggplot(data = zframe, aes(x = Var1, y = Var2, z = z)) + 
  theme.1 +
  geom_contour(stat = "contour", lineend = "butt", 
               linejoin = "round", linemitre = 1, 
               na.rm = FALSE, colour = "black") + 
  labs(x = "u", y = "v", title = "(b)") + 
  scale_y_continuous(limits = c(-5,5))
grid.arrange(con.1, con.2, ncol = 1)
# Se puede ver que los errores están correlacionados por la forma elíptica: los errores aumentan y disminuyen juntos (correlación positiva)
# MODELO 1 ==> MODELO DE INTERCEPTOS ALEATORIOS (sin predictores, para ver la variación en cada nivel)
music <- music %>%
  mutate(orch = ifelse(instrument=="orchestral instrument",1,0),
         large = ifelse(perform_type=="Large Ensemble",1,0) )
model.a <- lmer(na ~ 1 + (1 | id), REML = T, data = music)
VCrandom <- VarCorr(model.a)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.a)$ngrps)
coef(summary(model.a))
ints.a = fixef(model.a)[1] + ranef(model.a)[[1]][1]
n = length(music.lev2$id)
modela.plot = data.frame(id = music.lev2$id,
                         ints.a = ints.a[[1]],
                         slopes.a = rep(0,n))
ggplot(data=music) + 
  geom_boxplot(aes(factor(id),na)) + theme.1 + 
  xlab("Subject ID") + ylab("Negative Affect")
ggplot() +  
  geom_abline(data = modela.plot, 
              aes(intercept = ints.a, slope = slopes.a),
              color="dark gray") +
  geom_abline(aes(intercept = fixef(model.a)[1], 
                  slope = 0), size = 1) +
  theme.1 +
  scale_x_continuous(name = "Large Ensemble indicator",
                     limits = c(0,1), breaks = c(0,1)) +
  scale_y_continuous(name="Negative Affect", limits=c(10,25)) 
# Niveles de ansiedad sobre 16.2, varianza de 22.5 entre músicos; misma pendiente (0) pero distintos interceptos
# MODELO 2 ==> MODELO DE INTERCEPTOS Y PENDIENTES ALEATORIOS (hacer buenas predicciones en NIVEL 1)
model.b <- lmer(na ~ large +  (large | id), data = music)
VCrandom <- VarCorr(model.b)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.b)$ngrps)
coef(summary(model.b))
ints.b = fixef(model.b)[1] + ranef(model.b)[[1]][1]
slopes.b = fixef(model.b)[2] + ranef(model.b)[[1]][2]
modelb.plot = data.frame(id = music.lev2$id,
                         ints.b = ints.b[[1]],
                         slopes.b = slopes.b[[1]])
ggplot() +  
  geom_abline(data = modelb.plot, 
              aes(intercept = ints.b, slope = slopes.b),
              color="dark gray") +
  geom_abline(aes(intercept = fixef(model.b)[1],
                  slope = fixef(model.b)[2]), size = 1) +
  theme.1 +
  scale_x_continuous(name = "Large Ensemble indicator",
                     limits = c(0,1), breaks = c(0,1)) +
  scale_y_continuous(name="Negative Affect", limits=c(10,25)) 
# Niveles de ansiedad de 16.7 para solos y bandas pequeñas y de 15 para bandas grandes, con varianza de 21.8; pendiente negativa
# MODELO 3 ==> COVARIABLE EN NIVEL 2 (INTERCEPTOS ALEATORIOS)
model.c2 <- lmer(na ~ orch + large + orch:large +
                   (1|id), data = music)
VCrandom <- VarCorr(model.c2)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.c2)$ngrps)
coef(summary(model.c2))
model.c <- model0
AIC(model.c, model.c2)
BIC(model.c, model.c2)
# Niveles de ansiedad de 15.9, con varianza de 21.9
# MODELO 4 ==> AÑADIR EMOCIONES NEGATIVAS COMO OTRA COVARIABLE EN NIVEL 2
model.d <- lmer(na ~ orch + mpqnem + large + orch:large + 
                  mpqnem:large + (large | id), data = music)
VCrandom <- VarCorr(model.d)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.d)$ngrps)
coef(summary(model.d))
# Niveles de ansiedad de 11.57, con varianza de 21.8
# MODELO 5 ==> COVARIABLES CENTRADAS
music <- music %>%
  mutate(cmpqnem = mpqnem - mean(mpqnem))
model.e <- lmer(na ~ orch + cmpqnem + large + orch:large + 
                  cmpqnem:large + (large | id), REML = T, data = music)
VCrandom <- VarCorr(model.e)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.e)$ngrps)
coef(summary(model.e))
# Niveles de ansiedad de 16.26, con varianza de 21.8
# MODELO FINAL
music <- music %>%
  mutate(students = ifelse(audience=="Student(s)",1,0),
         juried = ifelse(audience=="Juried Recital",1,0),
         public = ifelse(audience=="Public Performance",1,0),
         solo = ifelse(perform_type=="Solo",1,0),
         memory1 = ifelse(memory=="Memory",1,0),
         female = ifelse(gender=="Female",1,0),
         vocal = ifelse(instrument=="voice",1,0) )
model.f <- lmer(na ~ previous + students + juried + 
                  public + solo + mpqpem + mpqab + orch + mpqnem + 
                  mpqnem:solo + (previous + students + juried + 
                                   public + solo | id), REML = T, data = music)
VCrandom <- VarCorr(model.f)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.f)$ngrps)
coef(summary(model.f))
# Niveles de ansiedad de 8.36, con varianza de 15.29
# Capítulo 9: DATOS LONGITUDINALES DE DOS NIVELES