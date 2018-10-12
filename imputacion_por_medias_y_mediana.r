library(readxl) 
library(dplyr)

#Abrimos el archivo exel y cambiamos el tipo de datos de algunas columnas
CalibrationMissingData <- read_excel("C:/Users/junio/Desktop/Unidad 2/CalibrationMissingData.xlsx", 
                                     col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric"))
"
#Cuantos datos nos faltan?
colSums(is.na(CalibrationMissingData)) #Cuantos en cada variable?
sum(is.na(CalibrationMissingData))
"

#Estos tendrán los valores ya inputados
vectorImputados <- CalibrationMissingData[, -1]
vectorImputadosMedianas <- CalibrationMissingData[, -1]

#Quitamos todos los na y ponemos 0 en su lugar
CalibrationMissingData[is.na(CalibrationMissingData)]= 0

#View(CalibrationMissingData)
longitud <- length(CalibrationMissingData)
vectorMedias <- numeric(longitud-1)
#vectorDesviado<- numeric(longitud-1)
vectorMedianas<- numeric(longitud-1)
otraLongitud <- length(vectorImputados)

#Calculamos la media de cada columna y la desviacion estandar
for (i in 2:longitud) {
  #print(CalibrationMissingData[i])
  vectorMedias[i-1] <- mean( CalibrationMissingData[i] %>% unlist ) #Metemos los promedios
  #vectorDesviado[i-1] <- sd( CalibrationMissingData[i] %>% unlist ) #metemos la desviacion estandar
  vectorMedianas[i-1] <- median( CalibrationMissingData[i] %>% unlist ) #metemos la mediana
}

"
View(varNormalizados)
View(datosExel)
View(datosNormalizado)
class(datosNormalizado)
length(varNormalizados$V1)
numRenglones<- length(datosExel[1] %>% unlist) #<- RESOLVIENDO ESTO RESOLVI LO DE HASTA ABAJO, quitando bastantes lineas de código
numRenglones
"

#hacemos el reemplazo
vectorImputados 
vectorMedias
vectorMedianas

"
#COMO LOS PROS EN R lo harían
vectorImputados$C_SIO2[is.na(vectorImputados$C_SIO2)]= vectorMedias[1]
vectorImputados$C_TIO2[is.na(vectorImputados$C_TIO2)]= vectorMedias[1]
vectorImputados$C_AL2O3[is.na(vectorImputados$C_AL2O3)]= vectorMedias[1]

*** otras 20 lineas de código, suponiendo que siempre trabajaremos con un exel de 20 renglones
"

#Como el barrio pesado lo hace
for (i in 1:otraLongitud) {
  vectorImputados[i][ is.na(vectorImputados[i]) ]= vectorMedias[i] #para el de medias
  vectorImputadosMedianas[i][ is.na(vectorImputadosMedianas[i]) ]= vectorMedianas[i]#para el de medianas
}

#Ya con los datos imputados de tres formas
View(vectorImputados)
View(vectorImputadosMedianas)
View(CalibrationMissingData)

#Generamos los archivos
write.csv(vectorImputados, "ImputadosMedias.csv")
write.csv(vectorImputadosMedianas, "ImputadosMedianas.csv")
write.csv(CalibrationMissingData, "ImputadosCeros.csv")

#Graficamos
plot(vectorImputados)
plot(vectorImputadosMedianas)
plot(CalibrationMissingData[, -1])