#################################################################################################
################### REPLICACIÓN INSIGHTS 142 (IO0942en.PDF) #######################################
#################################################################################################

#Cargando librerias básicas#
library(rio) # Para importar los datos
library(ggplot2) # Para hacer gráficos tipo ggplot
library(Rmisc) # Para poder usar la función summarySE
library(descr) # Para poder usar la función crosstab y compmeans

#Abriendo la base de datos de Stata en RStudio si se trabajara con la base
#de datos descargada#
lapopmerge <- import("LAPOP_Merge_2004_2018.dta")
lapop <- subset(lapopmerge, wave==2016)
lapop <- subset(lapop, pais<=23 | pais>=40)
rm(lapopmerge)

#Trabajaremos con esta base de datos que está en el repositorio
lapop <- import("LAPOP_reduced_merge.dta")
lapop <- subset(lapop, wave==2016)

#Recodificando la variable pais#
lapop$pais = as.factor(lapop$pais)
levels(lapop$pais) <- c("México", "Guatemala", "El Salvador", "Honduras",
                        "Nicaragua","Costa Rica", "Panamá", "Colombia", 
                        "Ecuador", "Bolivia", "Perú", "Paraguay", "Chile",
                        "Uruguay", "Brasil", "Venezuela", "Argentina", 
                        "Rep. Dom.", "Haití", "Jamaica", "Estados Unidos", 
                        "Canada")
table(lapop$pais)

#Recodificando la variable dependiente
library(car) # Para poder usar el comando recode
lapop$b37r <- recode(lapop$b37, "1:4=0 ; 5:7=100")
table(lapop$b37r)

#Sin tomar en cuenta el efecto de diseño
df <- summarySE(data=lapop, measurevar="b37r", groupvar="pais", na.rm=T)
graf1 <- ggplot(df, aes(x=reorder(pais, b37r), y=b37r)) +
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity") +
  geom_errorbar(aes(ymin=b37r-ci, ymax=b37r+ci), width= 0.2) +
  geom_text(aes(label=paste(round(b37r, 1), "%")), hjust=-0.8, size=2) +
  xlab("") +
  ylab("Confianza en los medios de comunicación (%)")  +
  coord_flip()
graf1

#Gráfico 1
#Para tomar en cuenta el efecto de diseño
tab.b37r <- as.data.frame(compmeans(lapop$b37r, lapop$pais, lapop$weight1500, plot=FALSE))
tab.b37r
varnames <- c("media", "n", "sd")
colnames(tab.b37r) <- varnames
tab.b37r$pais <- row.names(tab.b37r)
tab.b37r$err.st <- tab.b37r$sd/sqrt(tab.b37r$n)
tab.b37r$ci <- tab.b37r$err.st*1.96
tab.b37r <- tab.b37r[-23, ]
tab.b37r

graf142_1 <- ggplot(tab.b37r, aes(x=reorder(pais, media), y=media)) +
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity")+
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=0.2)+
  geom_text(aes(label=paste(round(media, 1), "%")), hjust=-0.8, size=2)+
  xlab("") + ylab("Confianza en los medios de comunicación (%)")+
  coord_flip()
graf142_1

#Gráfico 2
#Transformando variables a rango 0-1 para incluirlas en regresión#
lapop$edrr <- lapop$edr/3
lapop$size <- (5-lapop$tamano)/4
lapop$edadr <- (lapop$edad -1)/5
lapop$tono <- (lapop$colorr-1)/10
lapop$nse <- (lapop$quintall-1)/4

library(survey) # Para poder definir las características del diseño muestral
lapop.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=lapop)

modelo142_1 <- svyglm(b37 ~ mujer + edrr + size +
                        edadr + tono + nse + factor(pais), design=lapop.design)
modelo142_1

library(jtools) # Para poder usar el comando siguiente
export_summs(modelo142_1)
plot_summs(modelo1.pond, coefs=c("Mujer" = "mujer", 
                                 "Nivel de educación" = "edrr", 
                                 "Tamaño del lugar" = "size", 
                                 "Cohortes de edad" = "edadr",
                                 "Tono de piel más oscuro" = "tono", 
                                 "Nivel de riqueza" = "nse"))
#Gráfico 3
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
table(lapop$votewinner)

lapop.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=lapop)
modelo142_2 <- svyglm(b37 ~ it1r + q5a01 + newsattn + useinternet + 
                        polinterest + idpty + satisdem + votewinner + 
                        tono + edrr + edadr + size + nse + mujer +
                        factor(pais), design=lapop.design)
modelo142_2
export_summs(modelo142_1, modelo142_2)
plot_summs(modelo142_2.pond, coefs=c("Confianza interpersonal" = "it1r", 
                                     "Asiste servicios religiosos" = "q5a01", 
                                     "Presta atención a las noticias" = "newsattn", 
                                     "Usa internet diariamente" = "useinternet",
                                     "Interés en política" = "polinterest", 
                                     "Se identifica con un partido" = "idpty",
                                     "Satisfacción con la democracia" = "satisdem", 
                                     "Votó por el ganador" = "votewinner"))

