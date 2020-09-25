##################################################################################################
################### REPLICACIÓN INSIGHTS 135 (IO935es.PDF) #######################################
#################################################################################################

lapop$ros1per <- recode(lapop$ros1, "1:4=0 ; 5:7=100")
table(lapop$ros1per)

df135 <- summarySE(data=lapop, measurevar="ros1per", groupvar="pais", na.rm=T)

graf135 <- ggplot(df135, aes(x=reorder(pais, ros1per), y=ros1per)) +
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity") +
  geom_errorbar(aes(ymin=ros1per-ci, ymax=ros1per+ci), width= 0.2) +
  geom_text(aes(label=paste(round(ros1per, 1), "%")), hjust=-0.8, size=2) +
  xlab("") +
  ylab("Estado debería controlar industrias importantes")  +
  coord_flip()
graf135

tab.ros1per <- as.data.frame(compmeans(lapop$ros1per, lapop$pais, lapop$weight1500, plot=FALSE))
tab.ros1per
colnames(tab.ros1per) <- varnames
tab.ros1per$pais <- row.names(tab.ros1per)
tab.ros1per$err.st <- tab.ros1per$sd/sqrt(tab.ros1per$n)
tab.ros1per$ci <- tab.ros1per$err.st*1.96
tab.ros1per <- tab.ros1per[-23, ]
tab.ros1per

graf135_pond <- ggplot(tab.ros1per, aes(x=reorder(pais, media), y=media)) +
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity")+
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=0.2)+
  geom_text(aes(label=paste(round(media, 1), "%")), hjust=-0.8, size=2)+
  xlab("") + ylab("Estado debería controlar industrias importantes")+
  coord_flip()
graf135_pond



library(survey) # Para poder definir las características del diseño muestral
lapop.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=lapop)

modelo135.pond <- svyglm(ros1per ~ mujer + urban + edadr + nse + edrr 
                       + factor(pais), exp=TRUE, design=lapop.design)
modelo1.pond
export_summs(modelo1.pond)
plot_summs(modelo1.pond, coefs=c("Mujer" = "mujer",
                                 "Urbano" = "urban",
                                 "Edad" = "edadr",
                                 "Nivel de riqueza" = "nse",
                                 "Nivel de educación" = "edrr"
                                 ))
