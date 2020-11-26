#Cargando librerias básicas#
library(rio) # Para importar los datos
library(ggplot2) # Para hacer gráficos tipo ggplot
library(Rmisc) # Para poder usar la función summarySE
library(descr) # Para poder usar la función crosstab y compmeans

peru <- import("Peru LAPOP merge 2006-2019 (v1.0).dta")

#Análisis de la protesta en el tiempo#
prot.peru <- as.data.frame(compmeans(peru$prot3r, peru$year, peru$weight1500, plot=FALSE))
colnames(prot.peru) <- varnames
prot.peru$year <- row.names(prot.peru)
prot.peru$err.st <- prot.peru$sd/sqrt(prot.peru$n)
prot.peru$ci <- prot.peru$err.st*1.96
prot.peru <- prot.peru[-c(1,2,8), ]
prot.peru

grafprot <- ggplot(prot.peru, aes(x=year, y=media, group=1)) + 
  geom_line() +
  geom_point() +
  ylab("Participación en protestas (%)") +
  xlab("Año")
grafprot_2 <- grafprot + geom_ribbon(aes(ymin=media-ci, 
                                     ymax=media+ci),
                                 linetype=1,
                                 fill="grey80", outline.type="upper") + 
  geom_line(aes(y=media), colour="green4") + 
  geom_text(aes(label=paste(round(media, 1), "%")), 
            hjust=-0.8, size=3)
grafprot_2

#Análisis de la protesta por grupos de edad#
peru$edad = as.factor(peru$edad)
levels(peru$edad) <- c("18-25", "26-35", "36-45", "46-55", "56-65", "66+")
table(peru$edad)
                        
prot.peru2 <- as.data.frame(compmeans(peru$prot3r, peru$edad, peru$weight1500, plot=FALSE))


