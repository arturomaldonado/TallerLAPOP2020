*Do-file para replicar Brief 035*
*Tolerancia a los "golpes de Estado" ejecutivos en Perú*

*Se trabajará con el Grand Merge*

*cambiar ruta de acuerdo al directorio donde haya guardado el archivo del Grand Merge*
use "C:\ruta\Grand_Merge.dta" 

*Definir lenguaje a español*
lab lang es

*******************************
*Figura 1*

*Recodificación de la variable
recode jc15a (1=100 "Sí, es justificable") (2=0 "No, no es justificable"), gen(jc15a_r)

lp_comparative jc15a_r pais if wave==2018, ytitle("") xtitle("Cree que cierre del Congreso es justificable en tiempos difíciles (%)") xscale(range(0 60)) date("2018/19")  version(v.GM20190821_E) wrap percent sort(+jc15a_r) scheme(lapop_2016_green)


*******************************
*Figura 2*

lp_simple jc15a_r year if pais==11, line perc ytitle("Tolerancia a golpes" "de Estado ejecutivos (%)") xtitle("") date("2010-2018/19") scale(1.2) yscale(r(0 25)) version(v.GM20190821_E) scheme(lapop_2016_green)
