#importamos el dataframe
library(readxl)
spearheads <- read_excel("spearheads.xlsx")
spear <- c(spearheads)
spear <- as.data.frame(spear)  #funcion para convertirlo en dataframe
class(spear) #vemos que clase de objeto es

# nombramos los nombres de las variables
  # el doble = (==) compara, en vez de otorgar equivalencia
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservacion"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Maxle"] <- "longitud_max"
names(spear)[names(spear) == "Weight"] <- "Peso"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Data"] <- "Fecha"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Maxwit"] <- "Ancho_max_encaje"

#3 asignar etiquetas, pasa de números a categorías
spear$Conservacion=factor(spear$Conservacion, levels = c(1,2,3,4), labels = c("Excelente", "Bueno", "Regular",  "Malo"))
spear$Contexto=factor(spear$Contexto, levels = c(1,2,3), labels = c("S/C", "Habitacional", "Funerario"))
spear$Remache=factor(spear$Remache, levels = c(1,2), labels = c("Si", "No"))
spear$Materiales=factor(spear$Materiales, levels = c(1,2), labels = c("Bronce", "Hierro"))

#4 Tablas de frecuencia
  #nombrar cada tabla 
tabla_mat <- table(spear$Materiales)
tabla_cond <- table(spear$Conservacion)
tabla_con <-table(spear$Contexto)

#5 Tablas cruzadas, tan solo añadimos más variables a la función "table"
tabla_con_mat <- table(spear$Contexto, spear$Materiales)
View(tabla_con_mat)
tabla_cond_mat <- table(spear$Materiales, spear$Conservacion)
View(tabla_cond_mat)

#6 con "prop.table" obtenemos una tabla de proporción, que al ser
  #multiplicado por 100 adquirimos el %
porcentaje_materiales <- prop.table(table(spear$Materiales)) * 100
View(porcentaje_materiales)

porcentaje_contextos <- prop.table(table(spear$Contexto)) * 100
View(porcentaje_contextos)

porcentaje_conservacion <- prop.table(table(spear$Conservacion)) *100
View(porcentaje_conservacion)

#7 Como en el ejercicio anterior ahora obtenemos la tabla cruzada de porcentaje
  #con "round" redondeamos los porcentajes para que la suma sea 100%
porcentaje_mat_con <- round(prop.table(table(spear$Materiales, spear$Contexto)) *100)
View(porcentaje_mat_con)

porcentaje_mat_cond <- round(prop.table(table(spear$Materiales, spear$Conservacion)) *100)
View(porcentaje_mat_cond)

#8 "barplot" para crear el grafico de barras

grafica_con <- barplot(tabla_con, main = "Grafico contexto")

grafica_cond <- barplot(tabla_cond, main = "Grafico conservacion")

#9 añadiendo "horiz = TRUE" la hacemos horizontal
grafica_mat <- barplot(tabla_mat, main = "Grafico materiales", horiz = T)

tabla_remache <- table(spear$Remache)
barplot(tabla_remache, main = "Grafico remache", horiz = T)

#10 para este ejercicio se ha usado la tabla cruzada antes realizada
  #con el codigo de clase, en el que se han delimitado los límites
  #en el caso del eje y el límite es la suma total de las variables
bar.cond <- barplot(tabla_cond_mat, width = 0.85, ylim = c(0,sum(tabla_cond_mat[,1])*1.1),
                    main = "Grafica agrupada",
                    xlab = "Conservacion",
                    ylab = "contexto",
                    legend = T)

#11 con la funcion "pie" creamos un grafico de sectores.
  #No he conseguido aun añadir la frecuencia y el porcentaje en el mismo grafico
  #Si por separado

grafico_sectores <- pie(tabla_cond)
pie(tabla_cond, labels = tabla_cond)
pie(tabla_cond, labels = (paste0(tabla_cond, "%")))

#12
 #no he comprendido bien como hacer este ejercicio

