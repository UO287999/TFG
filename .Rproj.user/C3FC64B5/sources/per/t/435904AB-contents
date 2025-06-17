# GLM y Modelos multinivel
# Capítulo 8: INTRODUCCIÓN A MODELOS MULTINIVEL
# Paquetes
library(MASS)
library(gridExtra)  
library(mnormt) 
library(lme4) 
library(knitr) 
library(kableExtra)
library(tidyverse)  
library(gitcreds)

# CASO: INFLUENCIA DE LA ANSIEDAD EN LAS INTERPRETACIONES DE MÚSICOS
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
# Paquetes
library(GGally)
library(data.table)
library(Hmisc)
library(mice)
library(lattice)
library(nlme)
library(reshape2)
library(MASS)
library(mnormt)
library(lme4)
library(gridExtra) 
library(knitr)
library(kableExtra)
library(broom)
library(tidyverse)
# CASO: COLEGIOS CONCERTADOS
## ANÁLISIS EXPLORATORIO
### ORGANIZACIÓN DE DATOS
url <- "https://raw.githubusercontent.com/proback/BeyondMLR/master/data/chart_wide_condense.csv"
download.file(url, destfile = "chart_wide_condense.csv")
chart.wide = read_csv("chart_wide_condense.csv") 
table1chp9 <- head(chart.wide[2:11])
kable(table1chp9, booktabs=T,
      caption = "The first six observations in the wide data set for the Charter Schools case study.") %>%
  kable_styling(latex_options = "scale_down", font_size = 9)
### DATOS FALTANTES
table2chp9 <- md.pattern(chart.wide[c(5,9,10,11)], plot=FALSE)
kable(table2chp9, booktabs=T, 
      caption="A frequency table of missing data patterns.") %>%
  kable_styling(latex_options = "scale_down")
### DATOS LONGITUDINALES
colnames(chart.wide)
select <- dplyr::select
chart.long <- chart.wide %>%
  gather(key = "key", value = "MathAvgScore",
         MathAvgScore.0:MathAvgScore.2) %>%
  separate(key, into = c("name", "year08"), sep = "\\.") %>%
  select(-c("...1", "name")) %>%
  arrange(schoolid, year08) %>%
  mutate(year08 = as.numeric(year08))
head(chart.long)
smallchart.long <- filter(chart.long, row_number() <= 72)
head(smallchart.long)
table3chp9 <- head(smallchart.long[,c(2,4,6,7,8,9)])
kable(table3chp9, booktabs=T,
      caption= "The first six observations in the long data set for the Charter Schools case study; these lines correspond to the first two observations from the wide data set illustrated in Table (ref:caplontable3chp9).") %>%
  kable_styling(latex_options = "scale_down")
### MODELOS MULTINIVEL
# Media de cada año a cada colegio para gráficas
chart.means <- chart.long %>%
  group_by(schoolid) %>%
  summarise(mean3yr = mean(MathAvgScore, na.rm=T))
chart.wide <- chart.wide %>%
  mutate(urban0 = ifelse(urban==1, "urban", "rural"),
         charter0 = ifelse(charter==1, "charter", 
                           "public non-charter")) %>%
  left_join(chart.means, by="schoolid")
wide.charter <- chart.wide %>% filter(charter == 1)
set.seed(27)  #pulls same random sample every time
samp = sample(1:length(chart.wide$charter==0), size=dim(wide.charter)[1])
samp   # getting equal number of charters and non-charters
wide.public <- chart.wide %>%
  filter(charter == 0) %>%
  sample_n( dim(wide.charter)[1] )
sampdata <- bind_rows(wide.charter, wide.public) %>%
  select(-...1) %>%
  mutate(vars = row_number())   # Just use numbers 1-146 as school ids
head(sampdata)
sampdata.l <- sampdata %>%
  gather(key = "key", value = "MathAvgScore", MathAvgScore.0:MathAvgScore.2) %>%
  separate(key, into = c("name", "year08"), sep = "\\.") %>%
  select(-name) %>%
  arrange(charter, vars, year08) %>%
  mutate(year08 = as.numeric(year08))
head(sampdata.l)
theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))
# GRÁFICAS
chart.wide %>% count(charter0)
chart.wide %>% count(urban0)
## PUNTUACIONES DE MATES
ggplot(data=chart.wide,aes(x=mean3yr)) + 
  geom_histogram(binwidth=5,color="black",fill="white") + 
  theme.1 + 
  xlab("Mean Math Scores by School") + ylab("Frequency")
## CONCERTADO VS NO CONCERTADO
chart.wide %>% group_by(charter0) %>%
  summarise(means = mean(mean3yr), 
            sds = sd(mean3yr), 
            meds = median(mean3yr),
            q1s = quantile(mean3yr, 0.25), 
            q3s = quantile(mean3yr, 0.75),
            mins = min(mean3yr), 
            maxs = max(mean3yr), ns = n())
## URBANO VS RURAL 
chart.wide %>% group_by(urban0) %>%
  summarise(means = mean(mean3yr), 
            sds = sd(mean3yr), 
            meds = median(mean3yr),
            q1s = quantile(mean3yr, 0.25), 
            q3s = quantile(mean3yr, 0.75),
            mins = min(mean3yr), 
            maxs = max(mean3yr), ns = n())
charter.school <- ggplot(data = chart.wide, 
                         aes(x = factor(charter0), y = mean3yr)) + 
  geom_boxplot() + coord_flip() + 
  theme.1 + ylab("Mean Math Scores by School") + 
  xlab("") + labs(title="(a)")
urban.school <- ggplot(data = chart.wide, 
                       aes(x = factor(urban0), y = mean3yr)) + 
  geom_boxplot() + coord_flip() + 
  theme.1 + ylab("Mean Math Scores by School") + 
  xlab("") + labs(title="(b)")
lon.box1 <- grid.arrange(charter.school, urban.school, 
                         ncol = 1, nrow = 2)
grid.arrange(charter.school,urban.school,ncol=1,nrow=2)
#Changing percentage scale to 0 to 100
chart.long <- chart.long %>%
  mutate(SchPctFree = schPctfree*100, 
         SchPctSped = schPctsped*100,
         SchPctNonw = schPctnonw*100)

chart.wide <- chart.wide %>%
  mutate(SchPctFree.wide = schPctfree*100, 
         SchPctSped.wide = schPctsped*100, 
         SchPctNonw.wide = schPctnonw*100)

# math score vs. continuous level two covariates
PctFree.school <- ggplot(data = chart.wide, 
                         aes(x = schPctfree, y = mean3yr)) + 
  geom_point(color="dark grey") + theme.1 + 
  geom_smooth(se=FALSE,method="lm",color="black") +
  xlab("Percent Free/Reduced Lunch") + 
  ylab("Mean Math Scores\nby School") + labs(title="(a)")

PctSped.school <- ggplot(data = chart.wide, 
                         aes(x = schPctsped, y = mean3yr)) + 
  geom_point(color="dark grey") + theme.1 + 
  geom_smooth(se=FALSE,method="lm",color="black") +
  xlab("Percent Special Ed") + 
  ylab("Mean Math Scores\nby School") + labs(title="(b)")

PctNonw.school <- ggplot(data = chart.wide, 
                         aes(x = schPctnonw, y = mean3yr)) + 
  geom_point(color="dark grey") + theme.1 + 
  geom_smooth(se=FALSE,method="lm",color="black") +
  xlab("Percent Non-white") + 
  ylab("Mean Math Scores\nby School") + labs(title="(c)") 

lon.scat1 <- grid.arrange(PctFree.school, PctSped.school,
                          PctNonw.school, ncol = 2)
grid.arrange(PctFree.school, PctSped.school, 
             PctNonw.school, ncol = 2)

## ANÁLISIS EXPLORATORIO DE DATOS LONGITUDINALES
#Lattice plots
#  First change names of Central and Chaska
smallchart.long$schoolName[7:9]="CENTRAL108"
smallchart.long$schoolName[37:39]="CHASKAEAST"
smallchart.long$schoolName[40:42]="CHASKAWEST"
smallchart.long$schoolName[64:66]="CENTRAL13"
ggplot(smallchart.long, aes(x = year08, y = MathAvgScore)) +
  geom_point() + geom_line() + 
  facet_wrap(~schoolName,ncol=6) + 
  scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) +
  theme.1 + theme(strip.text.x=element_blank()) + 
  labs(x="Years since 2008",y="Math Score")
ggplot(sampdata.l, aes(x = year08, y = MathAvgScore)) + 
  geom_line(aes(group = schoolid), color = "dark grey") + 
  geom_smooth(aes(group = 1), color = "black", size = 1) + 
  theme.1 + 
  labs(x = "Years since 2008", y = "Math Score")
ggplot(smallchart.long, aes(x = year08, y = MathAvgScore)) +
  geom_point() + stat_smooth(method=lm) +
  facet_wrap(~schoolName,ncol=6) + 
  scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) +
  scale_y_continuous(limits=c(640,665)) +
  theme.1 + theme(strip.text.x=element_blank()) + 
  labs(x="Years since 2008",y="Math Scores")
##Spaghetti Plots
#get rid of NA data
newsampdata.l <- sampdata.l %>% na.omit()
ggplot(newsampdata.l, aes(x = year08, y = MathAvgScore)) + 
  geom_line(aes(group=schoolid),color="dark grey") + 
  facet_grid(.~charter0) + 
  geom_smooth(aes(group=1),color="black",size=1) + 
  labs(x="Years since 2008",y="Math Scores") 
newsampdata.l <- newsampdata.l %>%
  mutate(splitup = paste("Quartile", 
                         as.numeric(cut2(schPctfree, g=4))))
ggplot(newsampdata.l,aes(x=year08,y=MathAvgScore)) + 
  geom_line(aes(group=schoolid),color="dark grey") + 
  geom_smooth(method="loess",color="black",se=FALSE,size=.75) +
  facet_grid(~splitup) +
  labs(x="Years since 2008",y="Math Scores") + 
  scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) +
  theme.1 

## MODELO DE DOS ESTACIONES
#95% CI's for slope and intercept of 24 schools	(2 are filtered out since 1 obs)		
regressions <- smallchart.long %>% 
  group_by(schoolid) %>% 
  do(fit = lm(MathAvgScore ~ year08, data=.))

sd_filter <- smallchart.long %>%
  group_by(schoolid) %>%
  summarise(sds = sd(MathAvgScore)) 

regressions <- regressions %>%
  right_join(sd_filter, by="schoolid") %>%
  filter(!is.na(sds))

lm_info1 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(rate = year08, int = `(Intercept)`)

lm_info2 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, std.error) %>%
  spread(key = term, value = std.error) %>%
  rename(se_rate = year08, se_int = `(Intercept)`)

lm_info <- regressions %>%
  glance(fit) %>%
  ungroup() %>%
  select(schoolid, r.squared, df.residual) %>%
  inner_join(lm_info1, by = "schoolid") %>%
  inner_join(lm_info2, by = "schoolid") %>%
  mutate(tstar = qt(.975, df.residual), 
         intlb = int - tstar * se_int, 
         intub = int + tstar * se_int,
         ratelb = rate - tstar * se_rate, 
         rateub = rate + tstar * se_rate)
head(data.frame(lm_info))

#lon-cis1.eps
slope.ci <- ggplot(lm_info, aes(y=int, x=1:22)) + 
  geom_point() + theme.1 + 
  geom_errorbar(aes(ymin=intlb, ymax=intub)) + 
  coord_flip() + labs(y="Intercepts",x="Schools",title="(a)")

int.ci <- ggplot(lm_info, aes(y=rate, x=1:22)) + 
  geom_point() + theme.1 + 
  geom_errorbar(aes(ymin=ratelb, ymax=rateub)) + 
  coord_flip() + labs(y="Slopes",x="Schools",title="(b)")

lon.cis1 <- grid.arrange(slope.ci, int.ci, ncol=2)
# Find slope and intercept of all 618 schools	(540 after filter those with 1 obs)		
regressions <- chart.long %>% 
  group_by(schoolid) %>% 
  do(fit = lm(MathAvgScore ~ year08, data=.))

sd_filter <- chart.long %>%
  group_by(schoolid) %>%
  summarise(sds = sd(MathAvgScore)) 

regressions <- regressions %>%
  right_join(sd_filter, by="schoolid") %>%
  filter(!is.na(sds))

lm_info1 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(rate = year08, int = `(Intercept)`)

lm_info2 <- regressions %>%
  tidy(fit) %>%
  ungroup() %>%
  select(schoolid, term, std.error) %>%
  spread(key = term, value = std.error) %>%
  rename(se_rate = year08, se_int = `(Intercept)`)

lm_info <- regressions %>%
  glance(fit) %>%
  ungroup() %>%
  select(schoolid, r.squared, df.residual) %>%
  inner_join(lm_info1, by = "schoolid") %>%
  inner_join(lm_info2, by = "schoolid") %>%
  mutate(tstar = qt(.975, df.residual), 
         intlb = int - tstar * se_int, 
         intub = int + tstar * se_int,
         ratelb = rate - tstar * se_rate, 
         rateub = rate + tstar * se_rate)
head(data.frame(lm_info))

# summary stats for intercepts  					
summary(lm_info$int)
sd(lm_info$int)

# summary stats for fitted rate of change
summary(lm_info$rate)
sd(lm_info$rate,na.rm=T)

# summary stats for R sq
summary(lm_info$r.squared)

# histograms for ints, rates of change, and Rsq values - lon-histmat1.eps
int.hist1 <- ggplot(lm_info) + 
  geom_histogram(aes(x=int), 
                 binwidth=4, color="black", fill="white") + 
  theme.1 + 
  labs(x="Intercepts", y="Frequency", title="(a)")

rate.hist1 <- ggplot(lm_info) + 
  geom_histogram(aes(x=rate), 
                 binwidth=2, color="black", fill="white") + 
  theme.1 + 
  labs(x="Slopes", y="Frequency", title="(b)")

rsq.hist1 <- ggplot(lm_info) + 
  geom_histogram(aes(x=r.squared), 
                 binwidth=0.2, color="black", fill="white") + 
  theme.1 +
  labs(x="Rsquared values", y="Frequency", title="(c)")

# correlation between slopes and intercepts for subjects with slope
with(lm_info, cor(int, rate, use="complete.obs")) 
# Boxplots to compare school types
chart.wide <- lm_info %>%
  select(schoolid, int, rate , r.squared) %>%
  right_join(chart.wide, by = "schoolid")

#lon-box2.eps
int.box1 <- ggplot(chart.wide) + 
  geom_boxplot(aes(x = factor(charter0), y = int)) + 
  theme.1 + coord_flip() + 
  labs(x="School Type", y="Fitted Intercepts", title="(a)")

rate.box1 <- ggplot(chart.wide) + 
  geom_boxplot(aes(x = factor(charter0), y = rate)) + 
  theme.1 + coord_flip() + 
  labs(x="School Type", y="Fitted Slopes", title="(b)")

lon.box2 <- grid.arrange(int.box1, rate.box1, nrow=2)


grid.arrange(int.box1,rate.box1,nrow=2)

#lon-box3.eps
year08.box <- chart.long %>% filter(year08 == 0) %>%
  ggplot() + 
  geom_boxplot(aes(x = factor(charter), y = MathAvgScore)) + 
  theme.1 + coord_flip() + 
  labs(x="School Type", y="Math Score in 2008", title="(a)")

year10.box <- chart.long %>% filter(year08 == 2) %>%
  ggplot() + 
  geom_boxplot(aes(x = factor(charter), y = MathAvgScore)) + 
  theme.1 + coord_flip() + 
  labs(x="School Type", y="Math Score in 2010", title="(b)")

lon.box3 <- grid.arrange(year08.box, year10.box, nrow=2)
grid.arrange(year08.box,year10.box, nrow=2)
# OLS estimates plotted against the predictor Pct Free and Reduced Lunch
box1 <- ggplot(chart.wide) + geom_boxplot(aes(x=factor(charter0),y=100*schPctfree)) + 
  theme.1 + coord_flip() + 
  labs(x="School Type",
       y="% Free/Reduce Lunch",title="(a)")

int.scat1 <- ggplot(chart.wide,aes(x=100*schPctfree,y=int)) +
  geom_point() + theme.1 + 
  labs(x="% Free/Reduce Lunch",
       y="Fitted Intercepts",title="(b)") + 
  geom_smooth(se=FALSE,method="lm",color="black",size=.75)

rate.scat1 <- ggplot(chart.wide,aes(x=100*schPctfree,y=rate)) +
  geom_point() + theme.1 + 
  labs(x="% Free/Reduce Lunch",
       y="Fitted Slopes",title="(c)") + 
  geom_smooth(se=FALSE,method="lm",color="black",size=.75)

lon.boxscatmat1 <- grid.arrange(box1, int.scat1, 
                                rate.scat1, ncol=2)
grid.arrange(box1,int.scat1,rate.scat1,ncol=2)

# Divide subjects into low and high percent FRL at median 
# in order to illustrate charter school effect by percent FRL
with(chart.wide, cor( cbind( schPctnonw, int, rate),
                      use="pairwise.complete.obs"))
medpctfree <- median(chart.wide$schPctfree)
chart.wide <- chart.wide %>%
  mutate(highpctfree = ifelse(schPctfree > medpctfree,
                              "High Pct Free/Reduced Lunch",
                              "Low Pct Free/Reduced Lunch"))

#lon-boxmat1.eps
int.box <- ggplot(chart.wide) + theme.1 +
  geom_boxplot(aes(x=factor(charter0),y=int)) + coord_flip() +
  labs(x="School Type",y="Fitted Intercepts",title="(a)") +
  facet_grid(highpctfree~.)

rate.box <- ggplot(chart.wide) + theme.1 +
  geom_boxplot(aes(x=factor(charter0),y=rate)) + coord_flip() +
  labs(x="School Type",y="Fitted Slopes",title="(b)") +
  facet_grid(highpctfree~.)

lon.boxmat1 <- grid.arrange(int.box,rate.box,ncol=2)
grid.arrange(int.box,rate.box,ncol=2)
### Error Structure Within Schools {#lineartwostageerror2}
#Correlation structure	
score.nonm <- chart.long %>%
  filter(is.na(MathAvgScore) == FALSE)
hgtm.lm <- lm(MathAvgScore~year08, data=score.nonm)
score.nonm <- score.nonm %>%
  mutate(lmres = resid(hgtm.lm))

hgtwm <- score.nonm %>%
  select(schoolid, lmres, year08) %>%
  mutate(name = rep("lmres", n())) %>%
  unite(newcol, name, year08, sep = ".") %>%
ggpairs(hgtwm[,c(2:4)],upper=list(),
        lower=list(continuous="smooth"),
        diag=list(continuous="bar", discrete="bar"),
        axisLabels="show")
## Initial Models {#lineartwostageerror}
### Unconditional Means Model {#modela}
#Model A (Unconditional means model)
model.a <- lmer(MathAvgScore~ 1 + (1|schoolid), 
                REML=T, data=chart.long)
VCrandom <- VarCorr(model.a)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.a)$ngrps)
coef(summary(model.a))
### Unconditional Growth Model {#modelb9}
#lon-scat3.eps
Norwood <- chart.long %>% slice(7:9)
model0 <- lm(MathAvgScore ~ year08, data = Norwood)
ggplot(Norwood, aes(x = year08, y = MathAvgScore)) + 
  theme.1 + geom_point() + 
  geom_smooth(se=FALSE, method="lm", color="black", size=.75) +
  scale_y_continuous(limits=c(654,661)) +
  labs(x="Years since 2008", y="Math Score", 
       title="Norwood Central") + 
  geom_segment(aes(x = year08[1], y = MathAvgScore[1], 
    xend = year08[1], yend = model0$fitted.values[1]),
    linetype=2) +
  geom_segment(aes(x = Norwood$year08[2], y = MathAvgScore[2], 
    xend = year08[2], yend = model0$fitted.values[2]),
    linetype=2) +
  geom_segment(aes(x = Norwood$year08[3], y = MathAvgScore[3], 
    xend = year08[3], yend = model0$fitted.values[3]),
    linetype=2)
#Model B (Unconditional growth)
model.b <- lmer(MathAvgScore~ year08 + (year08|schoolid), 
                REML=T, data=chart.long)

VCrandom <- VarCorr(model.b)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.b)$ngrps)
coef(summary(model.b))
cat(" AIC = ", AIC(model.b), ";  BIC = ", BIC(model.b))

chart.long <- chart.long %>%
  mutate(yearc = year08 - 1, yearc2 = yearc ^ 2)

#Model B1 (Unconditional growth but only random intercepts)
model.b1 <- lmer(MathAvgScore~ year08 + (1|schoolid), 
  REML=T, data=chart.long)
summary(model.b1)
cat(" AIC = ", AIC(model.b1), ";  BIC = ", BIC(model.b1))

#model.b3 <- lmer(MathAvgScore~ yearc + yearc2 + (yearc+yearc2|schoolid), 
#  REML=T, data=chart.long)
#summary(model.b3)   # won't run - too many random effects

# Modeling quadratic time trend
model.b2 <- lmer(MathAvgScore~ yearc + yearc2 + (1|schoolid), 
                 REML=T, data=chart.long)

VCrandom <- VarCorr(model.b2)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.b2)$ngrps)
coef(summary(model.b2))
cat(" AIC = ", AIC(model.b2), ";  BIC = ", BIC(model.b2))

# Modeling piecewise linear time trend with 3 time points
#   (won't work in general)
chart.long <- chart.long %>%
  mutate(year0809 = ifelse(year08==1, 1, 0),
         year0810 = ifelse(year08==2, 1, 0))

# tiny bit better than quadratic but same story:
#   slope of 0.2 in 0809 but 2.5-0.2=2.3 in 0910
model.b6 <- lmer(MathAvgScore~ year0809 + year0810 +
                   (1|schoolid), REML=T, data=chart.long)
summary(model.b6)
AIC(model.b6)
BIC(model.b6)

#Model C (uncontrolled effects of school type on 
#   intercept and slope)
model.c <- lmer(MathAvgScore~ charter + year08 + 
  charter:year08 + (year08|schoolid), 
  REML=T, data=chart.long)

VCrandom <- VarCorr(model.c)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.c)$ngrps)
coef(summary(model.c))
cat(" AIC = ", AIC(model.c), ";  BIC = ", BIC(model.c))

#    Model B
fixef.b <- fixef(model.b)
fit.b <- fixef.b[[1]] + c(0,1,2)*fixef.b[[2]]
fit.frame1 <- data.frame(fit.b=fit.b,num=c(0,1,2))
fit.plot1 <- ggplot(fit.frame1,aes(x=num,y=fit.b)) + 
  geom_point(shape=1,fill="black",size=3) + 
  geom_line() + theme.1 + 
  scale_y_continuous(limits=c(640,660)) + 
  labs(x="Years since 2008",y="Predicted Math Score",
    title="Model B \n Unconditional growth")

#    Model C.
fixef.c <- fixef(model.c)
fit.c0 <- fixef.c[[1]] + c(0,1,2)*fixef.c[[3]]
fit.c1 <- fixef.c[[1]] + fixef.c[[2]] + 
          c(0,1,2)*fixef.c[[3]] +
          c(0,1,2)*fixef.c[[4]]

fit.frame2 <- data.frame(fit=c(fit.c0,fit.c1),
  num=c(0,1,2,0,1,2),
  type0=c(rep("Public Non-charter",3),rep("Charter",3)))

fit.plot2 <- ggplot(fit.frame2,aes(x=num,y=fit)) + 
  geom_point(aes(shape=type0)) + 
  theme.1 + theme(legend.position=c(.2,.9)) +
  theme(legend.title=element_blank()) +
  geom_line(aes(linetype=type0)) +
  scale_y_continuous(limits=c(640,660)) + 
  labs(x="Years since 2008",y="Predicted Math Score",
    title="Model C \n Uncontrolled charter effect")
grid.arrange(fit.plot1, fit.plot2, ncol=2)

##Code after the final figure - playing around with 
##  potential final models using lmer and lme

#Model D (Introduce urban at level 2)
model.d <- lmer(MathAvgScore~ charter + urban + year08 + 
  charter:year08 + urban:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d)

#Model D2 (Introduce SchPctFree at level 2)
model.d2 <- lmer(MathAvgScore~ charter + SchPctFree + year08 + 
  charter:year08 + SchPctFree:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d2)
anova(model.d2,model.c)

#Model D3 (Introduce SchPctNonw at level 2)
model.d3 <- lmer(MathAvgScore~ charter + SchPctNonw + year08 + 
  charter:year08 + SchPctNonw:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d3)

#Model D4 (Introduce SchPctSped at level 2)
model.d4 <- lmer(MathAvgScore~ charter + SchPctSped + year08 + 
  charter:year08 + SchPctSped:year08 + (year08|schoolid),
  REML=T, data=chart.long)
summary(model.d4)

########## Potential Final Models #############

#Model F (add more level 2 covariates)
model.f <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctNonw + SchPctSped + year08 + 
  charter:urban + charter:SchPctFree +
  charter:SchPctNonw + charter:SchPctSped +
  charter:year08 + urban:year08 + 
  SchPctFree:year08 + SchPctNonw:year08 + SchPctSped:year08 +
  charter:urban:year08 + charter:SchPctSped:year08 +
  charter:SchPctFree:year08 + charter:SchPctNonw:year08 + 
  (year08|schoolid), REML=T, data=chart.long)
summary(model.f)

model.f1 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctNonw + year08 + charter:year08 + SchPctNonw:year08 +
  (year08|schoolid), REML=T, data=chart.long)
summary(model.f1)

model.f1a <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctNonw + year08 + charter:year08 + SchPctNonw:year08 +
  (1|schoolid) + (0+year08|schoolid), REML=T, data=chart.long)
summary(model.f1a)

model.f2a <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid) + (0+year08|schoolid), REML=T, data=chart.long)
summary(model.f2a)

model.f2b <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid), REML=T, data=chart.long)
summary(model.f2b)

model.f2 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (year08|schoolid), REML=T, data=chart.long)

anova(model.f2, model.f2a, test = "Chisq")
anova(model.f2, model.f2b, test = "Chisq")

model.f2ml <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (year08|schoolid), REML=F, data=chart.long)
summary(model.f2ml)
model.f2aml <- lmer(MathAvgScore ~ charter + urban + 
  SchPctFree + SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid) + (0+year08|schoolid), REML=F, data=chart.long)
summary(model.f2aml)
anova(model.f2ml,model.f2aml)

# estimates of variance terms (not SDs) from Model f2
#   (will vary slightly each time since convergence 
#    not fully achieved)
sig = 8.8231
sig0 = 16.9468
sig1 = .003569
sig01 = .981*sqrt(sig0*sig1)

# Cov(Yi) = error cov matrix for school i
foo1=c(0,1,2)            # values of time variable (Year08)
foov=(sig+sig0)+(foo1^2)*(sig1)+2*foo1*(sig01)
foov                     # variance terms (diagonal of Cov(Y))
# [1] 25.76990 26.25599 26.74922
foo2=c(0,0,1)            # foo2 and foo3 give all combos of Year08
foo3=c(1,2,2)            #   for off-diagonal terms of Cov(Y)
fooc=(sig0)+(foo2+foo3)*(sig01)+foo2*foo3*(sig1)
fooc                     # covariance terms (off-diagonal of Cov(Y))   
# [1] 17.18806 17.42932 17.67772
dim1=c(1,1,2)            # picks off all combos from variance terms
dim2=c(2,3,3)            #   for calculating correlations
foor=fooc/sqrt(foov[dim1]*foov[dim2]) 
foor                     # correlation terms of Corr(Y)
# [1] 0.6607791 0.6638479 0.6670471

# Repeat calculation of error cov matrices with sig01=0
sig = 8.8231
sig0 = 16.9468
sig1 = .003559
sig01 = 0

# Cov(Yi) = error cov matrix for school i
foo1=c(0,1,2)            # values of time variable (Year08)
foov=(sig+sig0)+(foo1^2)*(sig1)+2*foo1*(sig01)
foov                     # variance terms (diagonal of Cov(Y))
# [1] 25.76990 25.77346 25.78414
foo2=c(0,0,1)            # foo2 and foo3 give all combos of Year08
foo3=c(1,2,2)            #   for off-diagonal terms of Cov(Y)
fooc=(sig0)+(foo2+foo3)*(sig01)+foo2*foo3*(sig1)
fooc                     # covariance terms (off-diagonal of Cov(Y))   
# [1] 16.94680 16.94680 16.95392
dim1=c(1,1,2)            # picks off all combos from variance terms
dim2=c(2,3,3)            #   for calculating correlations
foor=fooc/sqrt(foov[dim1]*foov[dim2]) 
foor                     # correlation terms of Corr(Y)
# [1] 0.6575745 0.6574384 0.6576691


## Fit models with different error cov structures for Model C

# Standard error covariance structure
std.lme <- lme(MathAvgScore~year08*charter, data= chart.long, 
  random=~year08|schoolid, control=lmeControl(opt="optim"),
  na.action=na.exclude)
summary(std.lme)

# Compare to standard error cov structure in lmer - matches well
std.lmer=lmer(MathAvgScore~year08*charter+(year08|schoolid),
  data=chart.long)
summary(std.lmer)

# estimates of variance terms (not SDs) from Model C (std) in lme
sig = 2.9481^2                # 8.6913
sig0 = 5.9972^2               # 35.9664
sig1 = .4756^2                # 0.2262
sig01 = .634*sqrt(sig0*sig1)  # 1.8083

# Cov(Yi) = error cov matrix for school i
foo1=c(0,1,2)            # values of time variable (Year08)
foov=(sig+sig0)+(foo1^2)*(sig1)+2*foo1*(sig01)
foov                     # variance terms (diagonal of Cov(Y))
# [1] 44.65770 48.50057 52.79584
foo2=c(0,0,1)            # foo2 and foo3 give all combos of Year08
foo3=c(1,2,2)            #   for off-diagonal terms of Cov(Y)
fooc=(sig0)+(foo2+foo3)*(sig01)+foo2*foo3*(sig1)
fooc                     # covariance terms (off-diagonal of Cov(Y))   
# [1] 37.77475 39.58308 41.84381
dim1=c(1,1,2)            # picks off all combos from variance terms
dim2=c(2,3,3)            #   for calculating correlations
foor=fooc/sqrt(foov[dim1]*foov[dim2]) 
foor                     # correlation terms of Corr(Y)
# [1] 0.8116708 0.8151952 0.8269095

# Unstructured error covariance structure
corandcov <- function(glsob,cov=T,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)[[2]]
    # must be number of subject with complete data
    # corStruct is lower triangle of pairwise correlations
    # corMatrix creates a large number of symmetric matrices 
    #   with diag of 1s
  print(corm)
  covm <- getVarCov(glsob, individual=2)
  return(covm)}

unstruct <- gls(MathAvgScore~year08*charter, chart.long, 
  correlation=corSymm(form = ~ 1 |schoolid),
  weights=varIdent(form = ~ 1|year08),method="REML", na.action=na.exclude)
#corandcov(unstruct)

# Do VarCov matrix by hand since getVarCov() doesn't 
# always work with gls.  Thinks schools are identified by 
# number (e.g. 2) instead of distschNum

# 2=ind. with full data
S = corMatrix(unstruct$modelStruct$corStruct)[[2]] 
# 3 weights for ind. 2
vw = 1/varWeights(unstruct$modelStruct$varStruct)[4:6] 
vars = (unstruct$sigma^2)*vw
result = t(S * sqrt(vars))*sqrt(vars)
print(result)

unstr.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
              random =  ~ 1 | schoolid,
              correlation = corSymm(form =  ~ 1 | schoolid), na.action=na.exclude,
              weights=varIdent(form = ~ 1|year08))
summary(unstr.lme)

# Compound symmetry error structure
comsym <- gls(MathAvgScore~year08*charter,chart.long, 
              na.action=na.exclude,
              correlation=corCompSymm(form = ~ 1 |schoolid), method="REML")
corandcov(comsym)

cs.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
           random = ~ 1|schoolid,na.action=na.exclude,
           correlation=corCompSymm(form = ~ 1 |schoolid))
summary(cs.lme)

# Compare to comp symm error cov structure in lmer - matches well
#   However, this doesn't include same restrictions on corr
#   matrix and hence has one fewer df.
cs.lmer=lmer(MathAvgScore~year08*charter+(1|schoolid),data=chart.long)
summary(cs.lmer)
anova(cs.lme,std.lme)

# Heterogeneous compound symmetry error structure
# lmeControl(msMaxIter=200)  # didn't help
hcs.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
            random =  ~ 1 | schoolid, na.action=na.exclude,
            correlation=corCompSymm(form = ~ 1 |schoolid), 
            weights=varIdent(form = ~1|year08))
summary(hcs.lme)
hcs.lme$modelStruct
anova(hcs.lme,cs.lme)   # hcs not converging here

# Autoregressive error structure
auto1 <- gls(MathAvgScore~year08*charter,chart.long, 
             na.action=na.exclude,
             correlation=corAR1(form = ~ 1 |schoolid), method="REML")
corandcov(auto1)

ar1.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
            random =  ~ 1 | schoolid,
            correlation=corAR1(form = ~ 1 |schoolid), na.action=na.exclude)
summary(ar1.lme)

# Heterogeneous autoregressive error structure
har1.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
             random =  ~ 1 | schoolid, 
             correlation=corAR1(form = ~ 1 |schoolid), na.action=na.exclude,
             weights=varIdent(form = ~1|year08))
summary(har1.lme)
har1.lme$modelStruct
anova(har1.lme,ar1.lme)

#Toeplitz error covariance structure
toep <- gls(MathAvgScore~year08*charter,chart.long,
            na.action=na.exclude,
            correlation=corARMA(form = ~ 1 |schoolid,p=2,q=0), method="REML")
corandcov(toep)

toep.lme=lme(MathAvgScore ~ year08 * charter, chart.long, 
             random =  ~ 1 | schoolid,
             correlation=corARMA(form = ~ 1 |schoolid,p=2,q=0),
             na.action=na.exclude)
summary(toep.lme)

#Model D2 (Introduce SchPctFree at level 2)
model.d2 <- lmer(MathAvgScore~ charter + SchPctFree + year08 + 
  charter:year08 + SchPctFree:year08 + (year08|schoolid),
  REML=T, data=chart.long)

VCrandom <- VarCorr(model.d2)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.d2)$ngrps)
coef(summary(model.d2))
cat(" AIC = ", AIC(model.d2), ";  BIC = ", BIC(model.d2))

drop_in_dev <- anova(model.d2, model.c, test = "Chisq")

did_print <- data.frame(npar=drop_in_dev$npar,
    AIC=drop_in_dev$AIC, BIC=drop_in_dev$BIC, 
    logLik=drop_in_dev$logLik, dev=drop_in_dev$deviance,
    Chisq=drop_in_dev$Chisq, Df=drop_in_dev$Df,
    pval=drop_in_dev$`Pr(>Chisq)`)
row.names(did_print) <- row.names(drop_in_dev)
did_print

model.f2 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (year08|schoolid), REML=T, data=chart.long)

VCrandom <- VarCorr(model.f2)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.f2)$ngrps)
coef(summary(model.f2))
cat(" AIC = ", AIC(model.f2), ";  BIC = ", BIC(model.f2))

#Model F0 (remove 2 variance components from Model F)
model.f0 <- lmer(MathAvgScore ~ charter + urban + SchPctFree + 
  SchPctSped + charter:year08 + urban:year08 + 
  SchPctSped:year08 + year08 +
  (1|schoolid), REML=T, data=chart.long)

VCrandom <- VarCorr(model.f0)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.f0)$ngrps)
coef(summary(model.f0))
cat(" AIC = ", AIC(model.f0), ";  BIC = ", BIC(model.f0))

drop_in_dev <- anova(model.f2, model.f0, test = "Chisq")

did_print <- data.frame(npar=drop_in_dev$npar,
    AIC=drop_in_dev$AIC, BIC=drop_in_dev$BIC, 
    logLik=drop_in_dev$logLik, dev=drop_in_dev$deviance,
    Chisq=drop_in_dev$Chisq, Df=drop_in_dev$Df,
    pval=drop_in_dev$`Pr(>Chisq)`)
row.names(did_print) <- row.names(drop_in_dev)
did_print
knitr::include_graphics("data/ParametricBootstrapDiagram9.png")

# Modified final model
model.f2a <- lmer(MathAvgScore ~ charter + urban + SchPctFree +
  SchPctSped + charter:year08 + urban:year08 +
  SchPctSped:year08 + year08 +
  (1|schoolid) + (0+year08|schoolid), REML=T, data=chart.long)

VCrandom <- VarCorr(model.f2a)
print(VCrandom, comp = c("Variance", "Std.Dev."))
cat(" Number of Level Two groups = ",
    summary(model.f2a)$ngrps)
coef(summary(model.f2a))
cat(" AIC = ", AIC(model.f2a), ";  BIC = ", BIC(model.f2a))

# LRT comparing final model in chapter (model.f2ml) with maximum
#  likelihood estimates to modified final model (model.f2aml)
#  with uncorrelated Level Two errors.
drop_in_dev <- anova(model.f2ml, model.f2aml, test = "Chisq")

did_print <- data.frame(npar=drop_in_dev$npar,
    AIC=drop_in_dev$AIC, BIC=drop_in_dev$BIC, 
    logLik=drop_in_dev$logLik, dev=drop_in_dev$deviance,
    Chisq=drop_in_dev$Chisq, Df=drop_in_dev$Df,
    pval=drop_in_dev$`Pr(>Chisq)`)
row.names(did_print) <- row.names(drop_in_dev)
did_print