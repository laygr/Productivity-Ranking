setwd("~/Freelance/1. Nov 16 - PTF Edo de México")

#Consulta de datos: http://www.beta.inegi.org.mx/app/saic/
###Código optimizado para el Estado de México en 2014


#####Lectura y limpieza de datos#####

base <- read.csv("Consulta inicial INEGI.csv", 
                 stringsAsFactors=FALSE)

###Renombrar variables
#names(base)
names(base)[names(base) %in% "Actividad.Económica"] <- "Act.Econ"
names(base)[names(base) %in% "A131A.Valor.agregado.censal.bruto..millones.de.pesos."] <- "VA.millones"
names(base)[names(base) %in% "H001D.Horas.trabajadas.por.Personal.ocupado.total..miles.de.Horas."] <- "HT.miles"
names(base)[names(base) %in% "Q000A.Acervo.total.de.activos.fijos..millones.de.pesos."] <- "AF.millones"

###Crear variable con código INEGI
base$Clave.INEGI <- as.character(paste0("15",substr(base$Municipio,1,3)))
base$Municipio <- substr(base$Municipio,5,100)


###Reescalar variables
base$VA <- base$VA.millones*1000000
base$HT <- base$HT.miles*1000
base$AF <- base$AF.millones*1000000

###Ordenar base final
#names(base)
base <- base[c("Año.Censal","Municipio","Clave.INEGI","Act.Econ","VA","HT","AF")]


###Escribir .csv con la base a usar en la regresión
#write.csv(base, "Edo. de Mex - Base para regresión PTF.csv", row.names=FALSE)



#####Estimación de la Productividad Total de los Factores#####

##Se puede utilizar la misma base o leer el .csv preparado anteriormente 
#datos <- read.csv("Edo. de Mex - Base para regresión PTF.csv",
#                  stringsAsFactors=FALSE)
datos <- base

##Limpiar inconsistencias
datos <- datos[datos$VA>0, ]
datos <- datos[datos$HT>0, ]
datos <- datos[datos$AF>0, ]

##Crear relaciones Y/L y K/Y
datos$YeL <- datos$VA/datos$HT
datos$KeY <- datos$AF/datos$VA

##Calcular logaritmos: ln(Y/L) y ln(K/Y)
datos$ln_YeL <- log(datos$YeL)
datos$ln_KeY <- log(datos$KeY)


###Correr regresión para cada municipio

##Preparar lista de resultados
Municipio <- unique(datos$Municipio)
PTF <- numeric(length(Municipio))
Resultados <- data.frame(Municipio=Municipio,ln_A=PTF,
                         stringsAsFactors=FALSE)


for(i in 15001:15125){
  
  aux <- subset(datos, Clave.INEGI==i)
  reg <- lm(ln_YeL ~ ln_KeY, data=aux)
  
  constante <- reg$coefficients[1]
  beta <- -(reg$coefficients[2])
  alpha <- beta/(beta+1)
  ln_a <- constante/(beta+1)
  
  Resultados$ln_A[(i-15000)] <- ln_a
  
}


##Añadir claves INEGI
Claves <- unique(base[c("Clave.INEGI","Municipio")])
Resultados <- merge(Claves,Resultados)


##Generar Ranking, Calificación y ordenar resultado final
Resultados$Ranking <- rank(-Resultados$ln_A)
Resultados$Score <- 100*(Resultados$ln_A-min(Resultados$ln_A))/(max(Resultados$ln_A)-min(Resultados$ln_A))

Resultados <- Resultados[order(Resultados$Ranking), ]
row.names(Resultados) <- NULL



##Guardar .csv de Resultados
write.csv(Resultados,"Ranking PTF - Edo. de México 2014.csv", row.names=FALSE)



