module WebService

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Http
open Suave.Writers

let processRequest host port database username =
    choose [    
      Suave.Filters.GET
        >=> request (fun r ->
              printf "Iniciando"
              let results = Regression.regression host port database username
              match r.queryParam "format" with
              | Choice1Of2 "csv" -> Successful.OK (Regression.resultsToCSV results)
                                    >=> setMimeType "application/csv; charset-utf-8"
              | _                -> Successful.OK (Regression.resultsToJSON results)
                                    >=> setMimeType "application/json; charset-utf-8"
            )
    ]

let main args =
    let host, port, database, username = args.[0], args.[1], args.[2], args.[3]
    startWebServer defaultConfig (processRequest host port database username)
    0
