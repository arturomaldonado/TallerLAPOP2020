#################################################################################################
################### REPLICACIÓN TOPICAL BRIEF 035 (ITB035es.pdf) ################################
#################################################################################################

#Cargando librerias básicas#
library(rio) # Para importar los datos
library(ggplot2) # Para hacer gráficos tipo ggplot
library(Rmisc) # Para poder usar la función summarySE
library(descr) # Para poder usar la función crosstab y compmeans

#Si se trabajara con la base de datos descargada
lapopmerge <- import("LAPOP_Merge_2004_2018.dta")
lapop <- subset(lapopmerge, wave==2016)
lapop <- subset(lapop, pais<=23 | pais>=40)
rm(lapopmerge)

#Abriendo la base de datos de Stata en RStudio#
lapop <- import("LAPOP_reduced_merge.dta")
lapop <- subset(lapop, wave==2018)
lapop <- subset(lapop, pais<=23)

#Seleccionando solo los años 2018 y 2019 en un nuevo dataframe#
lapop$pais = as.factor(lapop$pais)
levels(lapop$pais) <- c("México", "Guatemala", "El Salvador", "Honduras",
                        "Nicaragua","Costa Rica", "Panamá", "Colombia", 
                        "Ecuador", "Bolivia", "Perú", "Paraguay", "Chile",
                        "Uruguay", "Brasil", "Argentina", 
                        "Rep. Dom.", "Jamaica")
table(lapop$pais)
crosstab(lapop$pais, lapop$year, weight=lapop$weight1500, plot=F)

tab.jc15ar <- as.data.frame(compmeans(lapop18$jc15ar, lapop18$pais, lapop18$weight1500, plot=FALSE))
tab.jc15ar
varnames <- c("media", "n", "sd")
colnames(tab.jc15ar) <- varnames
tab.jc15ar$pais <- row.names(tab.jc15ar)
tab.jc15ar$err.st <- tab.jc15ar$sd/sqrt(tab.jc15ar$n)
tab.jc15ar$ci <- tab.jc15ar$err.st*1.96
tab.jc15ar <- tab.jc15ar[-19, ]
tab.jc15ar

graf035_1 <- ggplot(tab.jc15ar, aes(x=reorder(pais, media), y=media)) +
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity")+
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=0.2)+
  geom_text(aes(label=paste(round(media, 1), "%")), hjust=-1.3, size=2)+
  xlab("") + ylab("Cree que cierre del Congreso
                  es justificable en tiempos difíciles (%)")+
  coord_flip()
graf035_1

#Seleccionando solo los años 2018 y 2019 en un nuevo dataframe#
peru <- import("Peru reduced.dta")

tab.peru <- as.data.frame(compmeans(peru$jc15ar, peru$year, peru$weight1500, plot=FALSE))
colnames(tab.peru) <- varnames
tab.peru$year <- row.names(tab.peru)
tab.peru$err.st <- tab.peru$sd/sqrt(tab.peru$n)
tab.peru$ci <- tab.peru$err.st*1.96
tab.peru <- tab.peru[-6, ]
tab.peru

graf2 <- ggplot(tab.peru, aes(x=year, y=media, group=1)) + 
  geom_line() +
  geom_point() +
  ylab("Tolerancia a `golpes de Estado` ejecutivos (%)") +
  xlab("Año")
graf035_2 <- graf2 + geom_ribbon(aes(ymin=media-ci, 
                             ymax=media+ci),
                            linetype=1,
                            fill="grey80", outline.type="upper") + 
            geom_line(aes(y=media), colour="green4") + 
            geom_text(aes(label=paste(round(media, 1), "%")), 
                      hjust=-0.8, size=3)
graf035_2

