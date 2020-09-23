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

#################################################################################################
################### REPLICACIÓN INSIGHTS 144 (IO0944.PDF) #######################################
#################################################################################################

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


#################################################################################################
################### REPLICACIÓN TOPICAL BRIEF 035 (ITB035es.pdf) ################################
#################################################################################################

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

#################################################################################################
################### REPLICACIÓN INSIGHTS 142 (IO0942en.PDF) #######################################
#################################################################################################

#Abriendo la base de datos de Stata en RStudio#
lapop <- read_dta("LAPOP_Merge_2004_2018.dta")
lapop1617 <- subset(lapop, year==2016 | year==2017)
lapop1617 <- subset(lapop1617, pais<=23 | pais>=40)
rm(lapop)

install.packages("dummies")
library(dummies)

lapop1617$pais = as.factor(lapop1617$pais)
levels(lapop1617$pais) <- c("México", "Guatemala", "El Salvador", "Honduras", "Nicaragua",
                            "Costa Rica", "Panamá", "Colombia", "Ecuador", "Bolivia", 
                            "Perú", "Paraguay", "Chile", "Uruguay", "Brasil", "Argentina",
                            "Rep. Dom.", "Jamaica")
levels(lapop1617$pais)
str(lapop1617$pais)
table(lapop1617$pais)

lapop1617$b37r <- recode(lapop1617$b37, "1:4=0 ; 5:7=100")
table(lapop1617$b37r)

lapop <- dummy_cols(lapop1617, select_columns="pais")
rm(lapop1617)

df <- summarySE(data=lapop, measurevar="b37r", groupvar="pais", na.rm=T)

graf1 <- ggplot(df, aes(x=reorder(pais, b37r), y=b37r)) +
  geom_bar(width=0.5, fill="white", colour="black", stat="identity") +
  geom_errorbar(aes(ymin=b37r-ci, ymax=b37r+ci), width= 0.2) +
  geom_text(aes(label=paste(round(b37r, 1), "%")), hjust=-0.5) +
  xlab("Trust in Media (%)")  +
  coord_flip()

#Transformando variables a rango 0-1 para incluirlas en regresión#

lapop$edrr <- lapop$edr/3
lapop$size <- (5-lapop$tamano)/4
lapop$edadr <- (lapop$edad -1)/5
lapop$tono <- (lapop$colorr-1)/10
lapop$nse <- (lapop$quintall-1)/4

modelo1 <- lm(b37 ~ mujer + edrr + size +
                edadr + tono + nse + factor(pais), data=lapop)
summ(modelo1)
summ(modelo1, robust=T)
summ(modelo1, scale=T)
summ(modelo1, confint=T, ci.width=0.5)

effect_plot(modelo1, pred=tono, interval=T, plot.points=F, coefs=c("mujer",
                                                          "edrr", "size", "edadr", 
                                                          "tono", "nse"))
plot_summs(modelo1, coefs=c("Mujer" = "mujer", "Nivel de educación" = "edrr", 
                            "Tamaño del lugar" = "size", "Cohortes de edad" = "edadr",
                            "Tono de piel más oscuro" = "tono", 
                            "Nivel de riqueza" = "nse"))

#Transformando las otras variables independientes#

lapop$it1r <- (4 - lapop$it1) / 3
lapop$q5a01 <- recode(lapop$q5a, "1:4=1; 5=0")
lapop$satisdem <- (4 - lapop$pn4) / 3
lapop$useinternet <- (5 - lapop$www1) / 4
lapop$newsattn <- (5 - lapop$gi0) / 4
lapop$polinterest <- (4 - lapop$pol1) / 3
lapop$idpty <- recode(lapop$vb10, "1=1; 2=0")
lapop$voted <- recode(lapop$vb2, "1=1; 2=0")
lapop$votewinner <- NA
lapop$votewinner <- recode(lapop$vb2, "1=NA; 2=0")
lapop$votewinner <- recode(lapop$vb3n_16, "0=0; 97=0; 102=0; 103=0; 104=0;
                           177=0; 202=0; 203=0; 204=0; 205=0; 206=0; 207=0;
                           208=0; 209=0; 210=0; 211=0; 212=0; 213=0; 214=0;
                           277=0; 301=0; 303=0; 377=0; 402=0; 403=0; 404=0;
                           407=0; 408=0; 477=0; 501=0; 503=0; 504=0; 505=0;
                           577=0; 601=0; 602=0; 604=0; 605=0; 677=0; 702=0;
                           703=0; 777=0; 801=0; 802=0; 803=0; 805=0; 877=0;
                           902=0; 903=0; 904=0; 905=0; 906=0; 908=0; 977=0;
                           1002=0; 1003=0; 1004=0; 1005=0; 1077=0; 1102=0;
                           1103=0; 1104=0; 1105=0; 1106=0; 1107=0; 1108=0;
                           1177=0; 1202=0; 1203=0; 1204=0; 1205=0; 1206=0;
                           1277=0; 1303=0; 1306=0; 1307=0; 1309=0; 1310=0;
                           1311=0; 1377=0; 1402=0; 1403=0; 1404=0; 1405=0;
                           1406=0; 1477=0; 1502=0; 1503=0; 1577=0; 1602=0;
                           1604=0; 1605=0; 1677=0; 1702=0; 1703=0; 1704=0;
                           1705=0; 1777=0; 2102=0; 2103=0; 2177=0; 2202=0;
                           2203=0; 2204=0; 2205=0; 2206=0; 2277=0; 2301=0;
                           2377=0; 2402=0; 2477=0; 4001=0; 4003=0; 4004=0;
                           4077=0; 1701=1; 1001=1; 1501=1; 1305=1; 804=1;
                           603=1; 2101=1; 901=1; 302=1; 201=1; 2401=1; 
                           2201=1; 401=1; 2302=1; 101=1; 502=1; 701=1;
                           1201=1; 1101=1; 4002=1; 1401=1; 1601=1")

#El problema es que el segudo recode chanca el primer recode. Burcar recode de
# 2 variables#

#Gráfico 3#

modelo2 <- lm(b37 ~ it1r + q5a01 + newsattn + useinternet + polinterest + 
                idpty + satisdem + votewinner + tono + edrr + edadr + 
                size + nse + mujer + factor(pais), data=lapop)
summ(modelo2)
plot_summs(modelo2, coefs=c("Confianza" = "it1r", 
                            "Asiste serv. religiosos" = "q5a01", 
                            "Presta atención a las notivias" = "newsattn", 
                            "Usa internet diariamente" = "useinternet",
                            "Interés en política" = "polinterest", 
                            "Se identifica con un partido" = "idpty",
                            "Satisfacción con la democracia" = "satisdem", 
                            "Votó por el ganador" = "votewinner"))

