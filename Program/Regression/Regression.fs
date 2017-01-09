module Regression

open System
open RDotNet
open RProvider
open RProvider.stats

let regression host port database username =
  let dataMatrix = array2D (DbQuery.buildResults host port database username)
  let dataFrame = 
        namedParams [
            "Ano.Censal", box dataMatrix.[1.., 0];
            "Municipio", box dataMatrix.[1.., 1];
            "Clave.INEGI", box dataMatrix.[1.., 2];
            "Act.Econ", box dataMatrix.[1.., 3];
            "VA.millones", box (Array.map float dataMatrix.[1.., 4]);
            "HT.miles", box (Array.map float dataMatrix.[1.., 5]);
            "AF.millones", box (Array.map float dataMatrix.[1.., 6]);
            ]
        |> R.data_frame

  let e = REngine.GetInstance()
  e.SetSymbol("dataFrame", dataFrame)
  e.Evaluate(
        """
          dataFrame$VA <- dataFrame$VA.millones * 1000000
          dataFrame$HT <- dataFrame$HT.miles * 1000
          dataFrame$AF <- dataFrame$AF.millones * 1000000
          dataFrame <- dataFrame[dataFrame$VA > 0, ]
          dataFrame <- dataFrame[dataFrame$HT > 0, ]
          dataFrame <- dataFrame[dataFrame$AF > 0, ]
          
          ##Crear relaciones Y/L y K/Y
          dataFrame$YeL <- dataFrame$VA/dataFrame$HT
          dataFrame$KeY <- dataFrame$AF/dataFrame$VA

          ##Calcular logaritmos: ln(Y/L) y ln(K/Y)
          dataFrame$ln_YeL <- log(dataFrame$YeL)
          dataFrame$ln_KeY <- log(dataFrame$KeY)

          ###Correr regresión para cada municipio

          ##Preparar lista de resultados
          Municipio <- unique(dataFrame$Municipio)
          PTF <- numeric(length(Municipio))
          Resultados <- data.frame(Municipio=Municipio,ln_A=PTF, stringsAsFactors=FALSE)

          for(i in 15001:15125){
                aux <- subset(dataFrame, Clave.INEGI==i)
                reg <- lm(ln_YeL ~ ln_KeY, data=aux)

                constante <- reg$coefficients[1]
                beta <- -(reg$coefficients[2])
                alpha <- beta/(beta+1)
                ln_a <- constante/(beta+1)

                Resultados$ln_A[(i-15000)] <- ln_a

          }

          ##Añadir claves INEGI
          Claves <- unique(dataFrame[c("Clave.INEGI","Municipio")])
          Resultados <- merge(Claves,Resultados)

          ##Generar Ranking, Calificaci�n y ordenar resultado final
          Resultados$Ranking <- rank(-Resultados$ln_A)
          Resultados$Score <- 100*(Resultados$ln_A-min(Resultados$ln_A))/(max(Resultados$ln_A)-min(Resultados$ln_A))

          Resultados <- Resultados[order(Resultados$Ranking), ]
          row.names(Resultados) <- NULL

        """) |> ignore
  e.GetSymbol("Resultados")

let resultsToCSV (results:SymbolicExpression) =
  results.Engine.Evaluate("""
      paste(capture.output(write.csv(Resultados, row.names = FALSE)), collapse = "\n")
    """).GetValue<String>()

let resultsToJSON (results:SymbolicExpression) =
  results.Engine.Evaluate("""
      jsonlite::toJSON(Resultados)
    """).GetValue<String>()
