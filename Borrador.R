

#Para inspeccionar la ponderación#
lapop$year <- factor(lapop$year)
table(lapop$pais, lapop$year)

crosstab(lapop$pais, lapop$year, weight=lapop$weight1500, plot=F)

lapop$vict.r <- as.numeric(lapop$vic1ext)
table(lapop$vict.r)

lapop$vict.r[lapop$vict.r > 2] <- NA
lapop$vict.r <- factor(lapop$vict.r)
levels(lapop$vict.r) <- c("Sí", "No")

prop.table(table(lapop$vict.r, lapop$year), 2)*100

crosstab(lapop$vict.r, lapop$year, weight=lapop$weight1500, prop.c=T, plot=F)

#Para calcular los valores para cada país y  luego imputarlos en el data frame#
peru <- subset(lapop, pais=="Perú")
peru.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=peru)
meanperu<-svymean(peru$b37r, peru.design, na.rm=T)
sdperu<-svysd(~peru$b37r, peru.design, na.rm=T)
df$b37r[df$pais=="Perú"]<- meanperu
df$sd[df$pais=="Perú"]<- sdperu
df$se[df$pais=="Perú"]<- sdperu/sqrt(2631)
df$ci[df$pais=="Perú"]<- qt(1-.05/2,2631)*df$se[df$pais=="Perú"]

bra <- subset(lapop, pais=="Brasil")
bra.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=bra)
meanbra<-svymean(bra$b37r, bra.design, na.rm=T)
sdbra<-svysd(~bra$b37r, bra.design, na.rm=T)
df$b37r[df$pais=="Brasil"]<- meanbra
df$sd[df$pais=="Brasil"]<- sdbra
df$se[df$pais=="Brasil"]<- sdbra/sqrt(1521)
df$ci[df$pais=="Brasil"]<- qt(1-.05/2,1521)*df$se[df$pais=="Brasil"]

hai <- subset(lapop, pais=="Haití")
hai.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=hai)
meanhai<-svymean(hai$b37r, hai.design, na.rm=T)
sdhai<-svysd(~hai$b37r, hai.design, na.rm=T)
df$b37r[df$pais=="Haití"]<- meanhai
df$sd[df$pais=="Haití"]<- sdhai
df$se[df$pais=="Haití"]<- sdhai/sqrt(2221)
df$ci[df$pais=="Haití"]<- qt(1-.05/2,2221)*df$se[df$pais=="Haití"]

usa <- subset(lapop, pais=="Estados Unidos")
usa.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=usa)
meanusa<-svymean(usa$b37r, usa.design, na.rm=T)
sdusa<-svysd(~usa$b37r, usa.design, na.rm=T)
df$b37r[df$pais=="Estados Unidos"]<- meanusa
df$sd[df$pais=="Estados Unidos"]<- sdusa
df$se[df$pais=="Estados Unidos"]<- sdusa/sqrt(1500)
df$ci[df$pais=="Estados Unidos"]<- qt(1-.05/2,1500)*df$se[df$pais=="Estados Unidos"]

can <- subset(lapop, pais=="Canada")
can.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=can)
meancan<-svymean(can$b37r, can.design, na.rm=T)
sdcan<-svysd(~can$b37r, can.design, na.rm=T)
df$b37r[df$pais=="Canada"]<- meancan
df$sd[df$pais=="Canada"]<- sdcan
df$se[df$pais=="Canada"]<- sdcan/sqrt(1511)
df$ci[df$pais=="Canada"]<- qt(1-.05/2,1511)*df$se[df$pais=="Canada"]

lapop.design<-svydesign(ids =~upm, strata =~ estratopri, weights = ~weight1500, nest=TRUE, data=lapop)
