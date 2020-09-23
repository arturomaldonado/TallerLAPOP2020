#################################################################################################
################### REPLICACIÓN INSIGHTS 144 (IO0944.PDF) #######################################
#################################################################################################

#Cargando librerias básicas#

install.packages("Rmisc")
install.packages("lattice")
install.packages("huxtable")

library(foreign)
library(descr)
library(DescTools)
library(haven)
library(tidyverse)
library(ggplot2)
library(gplots)
library(dplyr)
library(psych)
library(Rmisc)
library(vcd)
library(psych)
library(car)
library(jtools)
library(huxtable)

#Abriendo la base de datos de Stata en RStudio#
lapop <- read_dta("LAPOP_Merge_2004_2018.dta")

## Conociendo la base de datos #
dim(lapop)
names(lapop)
str(lapop)
View(lapop)

# Exploración univariada: Tablas #
table(formal)
str(formal)

formal=as.factor(formal)
levels(formal)<-c("Sí","No")
levels(formal)
table(formal)
prop.table(table(formal))*100

pie(table(formal), col=c("Blue", "Green"))

cuadro1 <- prop.table(table(formal, year), 2)*100
barplot(cuadro1, legend = rownames(cuadro1), beside=F)

prop.table(table(formal, pais), 2)*100
cuadro2 <- prop.table(table(formal, pais), 2)*100
barplot(cuadro2, legend = rownames(cuadro2), beside=F)

#Creando la variable informalidad#
lapop$infor <- recode(lapop$formal, "1=0; 2=1")
lapop$infor = as.factor(lapop$infor)
levels(lapop$infor)<-c("No","Sí")
levels(lapop$infor)

str(lapop$infor)
table(lapop$infor)

prop.table(table(lapop$infor, lapop$pais), 2)*100
cuadro2 <- prop.table(table(lapop$infor, lapop$pais), 2)*100
barplot(cuadro2, legend = rownames(cuadro2), beside=F)

#Recodificando voto para usarla como VD#
lapop$voto <- recode(lapop$vb2, "1=1; 2=0")
lapop$voto = as.factor(lapop$voto)
levels(lapop$voto) <- c("No", "Sí")
levels(lapop$voto)
str(lapop$voto)
table(lapop$voto)

#Modelo logístico usando voto como VD e informalidad como VI#
modelo1 <- glm(lapop$voto ~ lapop$infor, data=lapop, family="binomial")
summary(modelo1)

modelo2 <- glm(lapop$voto ~ lapop$infor+lapop$ed+lapop$mujer+lapop$edad+lapop$ur, data=lapop, family="binomial")
summary(modelo2)

#Modelos lineales de redistribución#
modelo3 <- lm(lapop$ros4 ~ lapop$infor + lapop$ed + lapop$mujer + lapop$edad + lapop$ur)
summary(modelo3)

modelo4 <- lm(lapop$redist1 ~ lapop$infor + lapop$ed + lapop$mujer + lapop$edad + lapop$ur)
summary(modelo4)
