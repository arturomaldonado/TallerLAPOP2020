#################################################################################################
################### REPLICACIÓN INSIGHTS 142 (IO0942en.PDF) #######################################
#################################################################################################

#Cargando librerias básicas#
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
library(survey)

#Abriendo la base de datos de Stata en RStudio#
lapopmerge <- read_dta("LAPOP_Merge_2004_2018.dta")
lapop <- subset(lapopmerge, wave==2016)
lapop <- subset(lapop, pais<=23 | pais>=40)
rm(lapopmerge)

## Conociendo la base de datos #
dim(lapop)
names(lapop)
str(lapop)
View(lapop)

lapop$pais = as.factor(lapop$pais)
levels(lapop$pais) <- c("México", "Guatemala", "El Salvador", "Honduras",
                        "Nicaragua","Costa Rica", "Panamá", "Colombia", 
                        "Ecuador", "Bolivia", "Perú", "Paraguay", "Chile",
                        "Uruguay", "Brasil", "Venezuela", "Argentina", 
                        "Rep. Dom.", "Haití", "Jamaica", "Estados Unidos", 
                        "Canada")
levels(lapop$pais)
table(lapop$pais)

lapop$b37r <- recode(lapop$b37, "1:4=0 ; 5:7=100")
table(lapop$b37r)

df <- summarySE(data=lapop, measurevar="b37r", groupvar="pais", na.rm=T)

##############################################################################
lapop$b37r <- as.numeric(lapop$b37r)

##############################################################################

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
lapop <- within(lapop, {
                votewinner[vb2==2] <- 0
                votewinner[vb3n_16==0] <- 0
                votewinner[vb3n_16==97] <- 0
                votewinner[vb3n_16==102] <- 0
                votewinner[vb3n_16==103] <- 0
                votewinner[vb3n_16==104] <- 0
                votewinner[vb3n_16==177] <- 0
                votewinner[vb3n_16==202] <- 0
                votewinner[vb3n_16==203] <- 0
                votewinner[vb3n_16==204] <- 0
                votewinner[vb3n_16==205] <- 0
                votewinner[vb3n_16==206] <- 0
                votewinner[vb3n_16==207] <- 0
                votewinner[vb3n_16==208] <- 0
                votewinner[vb3n_16==209] <- 0
                votewinner[vb3n_16==210] <- 0
                votewinner[vb3n_16==211] <- 0
                votewinner[vb3n_16==212] <- 0
                votewinner[vb3n_16==213] <- 0
                votewinner[vb3n_16==214] <- 0
                votewinner[vb3n_16==277] <- 0
                votewinner[vb3n_16==301] <- 0
                votewinner[vb3n_16==303] <- 0
                votewinner[vb3n_16==377] <- 0
                votewinner[vb3n_16==402] <- 0
                votewinner[vb3n_16==403] <- 0
                votewinner[vb3n_16==404] <- 0
                votewinner[vb3n_16==407] <- 0
                votewinner[vb3n_16==408] <- 0
                votewinner[vb3n_16==477] <- 0
                votewinner[vb3n_16==501] <- 0
                votewinner[vb3n_16==503] <- 0
                votewinner[vb3n_16==504] <- 0
                votewinner[vb3n_16==505] <- 0
                votewinner[vb3n_16==577] <- 0
                votewinner[vb3n_16==601] <- 0
                votewinner[vb3n_16==602] <- 0
                votewinner[vb3n_16==604] <- 0
                votewinner[vb3n_16==605] <- 0
                votewinner[vb3n_16==677] <- 0
                votewinner[vb3n_16==702] <- 0
                votewinner[vb3n_16==703] <- 0
                votewinner[vb3n_16==777] <- 0
                votewinner[vb3n_16==801] <- 0
                votewinner[vb3n_16==802] <- 0
                votewinner[vb3n_16==803] <- 0
                votewinner[vb3n_16==805] <- 0
                votewinner[vb3n_16==877] <- 0
                votewinner[vb3n_16==902] <- 0
                votewinner[vb3n_16==903] <- 0
                votewinner[vb3n_16==904] <- 0
                votewinner[vb3n_16==905] <- 0
                votewinner[vb3n_16==906] <- 0
                votewinner[vb3n_16==908] <- 0
                votewinner[vb3n_16==977] <- 0
                votewinner[vb3n_16==1002] <- 0
                votewinner[vb3n_16==1003] <- 0
                votewinner[vb3n_16==1004] <- 0
                votewinner[vb3n_16==1005] <- 0
                votewinner[vb3n_16==1077] <- 0
                votewinner[vb3n_16==1102] <- 0
                votewinner[vb3n_16==1103] <- 0
                votewinner[vb3n_16==1104] <- 0
                votewinner[vb3n_16==1105] <- 0
                votewinner[vb3n_16==1106] <- 0
                votewinner[vb3n_16==1107] <- 0
                votewinner[vb3n_16==1108] <- 0
                votewinner[vb3n_16==1177] <- 0
                votewinner[vb3n_16==1202] <- 0
                votewinner[vb3n_16==1203] <- 0
                votewinner[vb3n_16==1204] <- 0
                votewinner[vb3n_16==1205] <- 0
                votewinner[vb3n_16==1206] <- 0
                votewinner[vb3n_16==1277] <- 0
                votewinner[vb3n_16==1303] <- 0
                votewinner[vb3n_16==1306] <- 0
                votewinner[vb3n_16==1307] <- 0
                votewinner[vb3n_16==1309] <- 0
                votewinner[vb3n_16==1310] <- 0
                votewinner[vb3n_16==1311] <- 0
                votewinner[vb3n_16==1377] <- 0
                votewinner[vb3n_16==1402] <- 0
                votewinner[vb3n_16==1403] <- 0
                votewinner[vb3n_16==1404] <- 0
                votewinner[vb3n_16==1405] <- 0
                votewinner[vb3n_16==1406] <- 0
                votewinner[vb3n_16==1477] <- 0
                votewinner[vb3n_16==1502] <- 0
                votewinner[vb3n_16==1503] <- 0
                votewinner[vb3n_16==1577] <- 0
                votewinner[vb3n_16==1602] <- 0
                votewinner[vb3n_16==1604] <- 0
                votewinner[vb3n_16==1605] <- 0
                votewinner[vb3n_16==1677] <- 0
                votewinner[vb3n_16==1702] <- 0
                votewinner[vb3n_16==1703] <- 0
                votewinner[vb3n_16==1704] <- 0
                votewinner[vb3n_16==1705] <- 0
                votewinner[vb3n_16==1777] <- 0
                votewinner[vb3n_16==2102] <- 0
                votewinner[vb3n_16==2103] <- 0
                votewinner[vb3n_16==2177] <- 0
                votewinner[vb3n_16==2202] <- 0
                votewinner[vb3n_16==2203] <- 0
                votewinner[vb3n_16==2204] <- 0
                votewinner[vb3n_16==2205] <- 0
                votewinner[vb3n_16==2206] <- 0
                votewinner[vb3n_16==2277] <- 0
                votewinner[vb3n_16==2301] <- 0
                votewinner[vb3n_16==2377] <- 0
                votewinner[vb3n_16==2402] <- 0
                votewinner[vb3n_16==2477] <- 0
                votewinner[vb3n_16==4001] <- 0
                votewinner[vb3n_16==4003] <- 0
                votewinner[vb3n_16==4004] <- 0
                votewinner[vb3n_16==4077] <- 0
                votewinner[vb3n_16==1701] <- 1
                votewinner[vb3n_16==1001] <- 1
                votewinner[vb3n_16==1501] <- 1
                votewinner[vb3n_16==1305] <- 1
                votewinner[vb3n_16==804] <- 1
                votewinner[vb3n_16==603] <- 1
                votewinner[vb3n_16==2101] <- 1
                votewinner[vb3n_16==901] <- 1
                votewinner[vb3n_16==302] <- 1
                votewinner[vb3n_16==201] <- 1
                votewinner[vb3n_16==2401] <- 1
                votewinner[vb3n_16==2201] <- 1
                votewinner[vb3n_16==401] <- 1
                votewinner[vb3n_16==2302] <- 1
                votewinner[vb3n_16==101] <- 1
                votewinner[vb3n_16==502] <- 1
                votewinner[vb3n_16==701] <- 1
                votewinner[vb3n_16==1201] <- 1
                votewinner[vb3n_16==1101] <- 1
                votewinner[vb3n_16==4002] <- 1
                votewinner[vb3n_16==1401] <- 1
                votewinner[vb3n_16==1601] <- 1
})

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
           
           
           
