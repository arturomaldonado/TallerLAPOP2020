*Do-file para replicar Insights 142*
*Confianza en los medios de comunicación en las Américas*

*Se trabajará con el Grand Merge*

*cambiar ruta de acuerdo al directorio donde haya guardado el archivo del Grand Merge*
use "C:\ruta\Grand_Merge.dta" 

*Definir lenguaje a español*
lab lang es

*Para seleccionar solo los datos de la ronda 2016/17*
drop if wave!=2016

*Recodificación de variables*

*Educación
lp_resc edr, gen(edrr) min(0) max(1)
tab edrr
label variable edrr "Nivel de educación"

*Cohortes de edad
lp_resc edad, gen(edadr) min(0) max(1)
tab edadr
label variable edadr "Cohortes de edad"

*Residencia urbana
gen urban=.
replace urban=1 if ur==1
replace urban=0 if ur==2
tab urban
label variable urban "Residencia urbana"

*Residencia en la capital nacional 
lp_resc tamano, gen(tamanor) min(1) max(0)
tab tamanor if wave==2016
*rural=0, natl cap=1
label variable tamanor "Tamaño del lugar de residencia"

*Riqueza
lp_resc quintall, gen(quintallr) min(0) max(1)
tab quintallr
label variable quintallr "Nivel de riqueza"

*Mujer
tab mujer
label variable mujer "Mujer"

*Color de piel
lp_resc colorr, gen(colorrr) min(0) max(1)
tab colorrr
label variable colorrr "Tono de piel más oscuro"

*******************************
*Figura 1*

*Recodificación de variable dependiente*
tab b37 if wave==2016
recode b37(1/4 = 0) (5/7 = 100), gen(b37pct)

lp_comparative b37pct pais if wave==2016, per sort(+b37pct) xtitle("Confianza en los medios" "de comunicación (%)") date("2016/17") version(v. GM_v.07172017) scheme(lapop_2016_purple)

*******************************
*Figura 2
lp_grcoef reg b37 mujer edrr tamanor edadr colorrr quintallr i.pais if wave==2016, xtitle("Predictores demogr·ficos y socioeconÛmicos" "de confianza en los medios de comunicaciÛn") omit(i.pais) version(v.GM_v.07172017) date("2016/17") scheme(lapop_2016_purple)

*******************************

*Recodificación de variables

*Confianza interpersonal
tab it1 if wave==2016
lp_resc it1, gen(trustppl) min(1) max(0)
tab trustppl
label variable trustppl "Confianza interpersonal"

*Asiste a servicios religiosos
recode q5a (1/4=1) (5=0), gen(q5a01)
tab q5a01
label variable q5a01 "Asiste servicios religiosos"

*Satisfacción con la democracia
tab pn4 if wave==2016
lp_resc pn4, gen(satisdem) min(1) max(0)
tab satisdem
label variable satisdem "Satisfacción con la democracia"

*Uso de internet
tab www1 if wave==2016
lp_resc www1, gen(useinternet) min(1) max(0)
tab useinternet
label variable useinternet "Usa internet diariamente"

*Atención a las noticias
tab gi0 if wave==2016
lp_resc gi0, gen(newsattn) min(1) max(0)
tab newsattn
label variable newsattn "Presta atención a las noticias"

*Interés en la política
tab pol1 if wave==2016
lp_resc pol1, gen(polinterest) min(1) max(0)
tab polinterest 
label variable polinterest "Interés en política"

*Identificación partidaria
tab vb10 if wave==2016
gen idpty=.
replace idpty=1 if vb10==1
replace idpty=0 if vb10==2
tab idpty
label variable idpty "Se identifica con un partido político"

*Votó en elecciones
tab vb2 if wave==2016
gen voted=.
replace voted=1 if vb2==1
replace voted=0 if vb2==2
tab voted
label variable voted "Votó en las últimas elecciones"

*Votó por el ganador
gen votewinner=.
replace votewinner=0 if vb2==2 & wave==2016
replace votewinner=0 if vb3n_16==0
replace votewinner=0 if vb3n_16==1
replace votewinner=0 if vb3n_16==97
replace votewinner=0 if vb3n_16==102
replace votewinner=0 if vb3n_16==103
replace votewinner=0 if vb3n_16==104
replace votewinner=0 if vb3n_16==177
replace votewinner=0 if vb3n_16==202
replace votewinner=0 if vb3n_16==203
replace votewinner=0 if vb3n_16==204
replace votewinner=0 if vb3n_16==205
replace votewinner=0 if vb3n_16==206
replace votewinner=0 if vb3n_16==207
replace votewinner=0 if vb3n_16==208
replace votewinner=0 if vb3n_16==209
replace votewinner=0 if vb3n_16==210
replace votewinner=0 if vb3n_16==211
replace votewinner=0 if vb3n_16==212
replace votewinner=0 if vb3n_16==213
replace votewinner=0 if vb3n_16==214
replace votewinner=0 if vb3n_16==277
replace votewinner=0 if vb3n_16==301
replace votewinner=0 if vb3n_16==303
replace votewinner=0 if vb3n_16==377
replace votewinner=0 if vb3n_16==402
replace votewinner=0 if vb3n_16==403
replace votewinner=0 if vb3n_16==404
replace votewinner=0 if vb3n_16==407
replace votewinner=0 if vb3n_16==408
replace votewinner=0 if vb3n_16==477
replace votewinner=0 if vb3n_16==501
replace votewinner=0 if vb3n_16==503
replace votewinner=0 if vb3n_16==504
replace votewinner=0 if vb3n_16==505
replace votewinner=0 if vb3n_16==577
replace votewinner=0 if vb3n_16==601
replace votewinner=0 if vb3n_16==602
replace votewinner=0 if vb3n_16==604
replace votewinner=0 if vb3n_16==605
replace votewinner=0 if vb3n_16==677
replace votewinner=0 if vb3n_16==702
replace votewinner=0 if vb3n_16==703
replace votewinner=0 if vb3n_16==777
replace votewinner=0 if vb3n_16==801
replace votewinner=0 if vb3n_16==802
replace votewinner=0 if vb3n_16==803
replace votewinner=0 if vb3n_16==805
replace votewinner=0 if vb3n_16==877
replace votewinner=0 if vb3n_16==902
replace votewinner=0 if vb3n_16==903
replace votewinner=0 if vb3n_16==904
replace votewinner=0 if vb3n_16==905
replace votewinner=0 if vb3n_16==906
replace votewinner=0 if vb3n_16==908
replace votewinner=0 if vb3n_16==977
replace votewinner=0 if vb3n_16==1002
replace votewinner=0 if vb3n_16==1003
replace votewinner=0 if vb3n_16==1004
replace votewinner=0 if vb3n_16==1005
replace votewinner=0 if vb3n_16==1077
replace votewinner=0 if vb3n_16==1102
replace votewinner=0 if vb3n_16==1103
replace votewinner=0 if vb3n_16==1104
replace votewinner=0 if vb3n_16==1105
replace votewinner=0 if vb3n_16==1106
replace votewinner=0 if vb3n_16==1107
replace votewinner=0 if vb3n_16==1108
replace votewinner=0 if vb3n_16==1177
replace votewinner=0 if vb3n_16==1202
replace votewinner=0 if vb3n_16==1203
replace votewinner=0 if vb3n_16==1204
replace votewinner=0 if vb3n_16==1205
replace votewinner=0 if vb3n_16==1206
replace votewinner=0 if vb3n_16==1277
replace votewinner=0 if vb3n_16==1303
replace votewinner=0 if vb3n_16==1306
replace votewinner=0 if vb3n_16==1307
replace votewinner=0 if vb3n_16==1309
replace votewinner=0 if vb3n_16==1310
replace votewinner=0 if vb3n_16==1311
replace votewinner=0 if vb3n_16==1377
replace votewinner=0 if vb3n_16==1402
replace votewinner=0 if vb3n_16==1403
replace votewinner=0 if vb3n_16==1404
replace votewinner=0 if vb3n_16==1405
replace votewinner=0 if vb3n_16==1406
replace votewinner=0 if vb3n_16==1477
replace votewinner=0 if vb3n_16==1502
replace votewinner=0 if vb3n_16==1503
replace votewinner=0 if vb3n_16==1577
replace votewinner=0 if vb3n_16==1602
replace votewinner=0 if vb3n_16==1604
replace votewinner=0 if vb3n_16==1605
replace votewinner=0 if vb3n_16==1677
replace votewinner=0 if vb3n_16==1702
replace votewinner=0 if vb3n_16==1703
replace votewinner=0 if vb3n_16==1704
replace votewinner=0 if vb3n_16==1705
replace votewinner=0 if vb3n_16==1777
replace votewinner=0 if vb3n_16==2102
replace votewinner=0 if vb3n_16==2103
replace votewinner=0 if vb3n_16==2177
replace votewinner=0 if vb3n_16==2202
replace votewinner=0 if vb3n_16==2203
replace votewinner=0 if vb3n_16==2204
replace votewinner=0 if vb3n_16==2205
replace votewinner=0 if vb3n_16==2206
replace votewinner=0 if vb3n_16==2277
replace votewinner=0 if vb3n_16==2301
replace votewinner=0 if vb3n_16==2377
replace votewinner=0 if vb3n_16==2402
replace votewinner=0 if vb3n_16==2477
replace votewinner=0 if vb3n_16==4001
replace votewinner=0 if vb3n_16==4003
replace votewinner=0 if vb3n_16==4004
replace votewinner=0 if vb3n_16==4077
replace votewinner=1 if vb3n_16==1701 
replace votewinner=1 if vb3n_16==1001
replace votewinner=1 if vb3n_16==1501
replace votewinner=1 if vb3n_16==1305
replace votewinner=1 if vb3n_16==804
replace votewinner=1 if vb3n_16==603
replace votewinner=1 if vb3n_16==2101
replace votewinner=1 if vb3n_16==901
replace votewinner=1 if vb3n_16==302
replace votewinner=1 if vb3n_16==201
replace votewinner=1 if vb3n_16==2401
replace votewinner=1 if vb3n_16==2201
replace votewinner=1 if vb3n_16==401
replace votewinner=1 if vb3n_16==2302
replace votewinner=1 if vb3n_16==101
replace votewinner=1 if vb3n_16==502
replace votewinner=1 if vb3n_16==701
replace votewinner=1 if vb3n_16==1201
replace votewinner=1 if vb3n_16==1101
replace votewinner=1 if vb3n_16==4002
replace votewinner=1 if vb3n_16==1401
replace votewinner=1 if vb3n_16==1601
tab votewinner 
tab votewinner if wave==2016
sum votewinner
label variable votewinner "Votó por el ganador"

*Figura 3
lp_grcoef reg b37 trustppl q5a01 newsattn useinternet polinterest idpty satisdem votewinner colorrr edrr edadr tamanor quintallr mujer i.pais if wave==2016, xtitle("Predictores polÌticos" "de confianza en los medios de comunicaciÛn") omit(colorrr edrr edadr tamanor quintallr mujer i.pais) version(v.GM_v.07172017) date("2016/17") scheme(lapop_2016_purple)
