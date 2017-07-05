module WebService

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Http
open Suave.Writers
open Suave.Web

let processRequest host port database username =
    choose [    
      Suave.Filters.GET >=>
        choose [
            path "/" >=>
                request (fun r ->
                      printf "Iniciando"
                      let results = Regression.regression host port database username
                      match r.queryParam "format" with
                      | Choice1Of2 "csv" -> Successful.OK (Regression.resultsToCSV results)
                                            >=> setMimeType "application/csv; charset-utf-8"
                      | _                -> Successful.OK (Regression.resultsToJSON results)
                                            >=> setMimeType "application/json; charset-utf-8"
                    )
            path "/check" >=> Successful.OK "It works!"
          ]
    ]

[<EntryPoint>]
let main args =
    let ip = args.[0]
    let port =  System.Int32.Parse args.[1]
    let serverConfig = Web.defaultConfig.withBindings [HttpBinding.createSimple Protocol.HTTP ip port]
    let host, port, database, username = args.[2], args.[3], args.[4], args.[5]
    startWebServer serverConfig (processRequest host port database username)
    0
