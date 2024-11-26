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
# TOKEN EJEMPLO GITHUB: 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
gitcreds::gitcreds_set()

# CASO 1: INFLUENCIA DE LA ANSIEDAD EN EL RENDIMIENTO DE MÚSICOS
music = read.csv("data/musicdata.csv")
# Capítulo 9: DATOS LONGITUDINALES DE DOS NIVELES