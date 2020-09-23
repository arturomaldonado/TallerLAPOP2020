#################################################################################################
################### REPLICACIÓN TOPICAL BRIEF 035 (ITB035es.pdf) ################################
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

lapop <- read_dta("LAPOP_Merge_2004_2018.dta")

#Seleccionando solo los años 2018 y 2019 en un nuevo dataframe#
lapop1819 <- subset(lapop, year==2018 | year==2019)
lapop1819 <- subset(lapop1819, pais<=23)
rm(lapop)
lapop1819$pais = as.factor(lapop1819$pais)
levels(lapop1819$pais) <- c("México", "Guatemala", "El Salvador", "Honduras", "Nicaragua",
                            "Costa Rica", "Panamá", "Colombia", "Ecuador", "Bolivia", 
                            "Perú", "Paraguay", "Chile", "Uruguay", "Brasil", "Argentina",
                            "Rep. Dom.", "Jamaica")
levels(lapop1819$pais)
str(lapop1819$pais)
table(lapop1819$pais)

#Recodificando variable JC15A#
lapop1819$golpe <- recode(lapop1819$jc15a, "1=1; 2=0")
lapop1819$golper <- recode(lapop1819$jc15a, "1=100; 2=0")

lapop1819$golpe = as.factor(lapop1819$golpe)
levels(lapop1819$golpe)<-c("No","Sí")
levels(lapop1819$golpe)
table(lapop1819$golpe)

#Comparación entre países para la última ronda#
prop.table(table(lapop1819$golpe, lapop1819$pais), 2)*100
cuadro3 <- prop.table(table(lapop1819$golpe, lapop1819$pais), 2)*100
barplot(cuadro3, legend = rownames(cuadro2), beside=F)

df <- summarySE(data=lapop1819, measurevar="golper", groupvar="pais", na.rm=T)

graf.punto <- ggplot(df, aes(x=pais, y=golper)) +
  geom_point(size=3) + ylim(0, 40)

graf.punto2 <- graf.punto + geom_errorbar(aes(ymin=golper-ci, ymax=golper+ci), width=0.2) + 
  ylab("Cierre del congreso es justificable (%)") + 
  ggtitle("Intervalo de confianza al 95% para el porcentaje de personas que justifican el cierre del congreso por país")

graf.barra1 <- ggplot(df, aes(x=reorder(pais, golper), y=golper)) +
  geom_bar(width=0.5, fill="white", colour="black", stat="identity") +
  geom_errorbar(aes(ymin=golper-ci, ymax=golper+ci), width= 0.2) +
  geom_text(aes(label=paste(round(golper, 1), "%")), hjust=-0.5) +
  ylab("Justifica cierre del congreso") + ggtitle("Intervalo de confianza al 95% para el porcentaje de personas que justifican el cierre del congreso por país") +
  coord_flip()

#Seleccionando solo los años 2018 y 2019 en un nuevo dataframe#
#peru <- subset(lapop, year>=2010 & pais==11)

#prop.table(table(peru$golpe, peru$year), 2)*100
#cuadro4 <- prop.table(table(peru$golpe, peru$year), 2)*100
#barplot(cuadro4, legend = rownames(cuadro2), beside=F)