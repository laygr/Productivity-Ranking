module DbQuery

open Npgsql

let commandToArray (command:NpgsqlCommand) =
    let dataReader = command.ExecuteReader()
    seq {
        while dataReader.Read() do
            yield [for i in [0..dataReader.FieldCount-1] -> dataReader.[i].ToString()] |> List.toArray
    } |> Seq.toArray

let getColumnNames =
    [|"Ano.Censal";  "Municipio"; "Clave.INEGI"; "Act.Econ"; "VA.millones"; "HT.miles"; "AF.millones"|]

let actividadesEconomicas =
    [|
        "11 Agricultura cría y explotación de animales aprovechamiento forestal pesca y caza (sólo pesca acuicultura y servicios relacionados con las actividades agropecuarias y forestales)";
        "31 - 33 industrias manufactureras";
        "43 Comercio al por mayor";
        "46 Comercio al por menor";
        "53 Servicios inmobiliarios y de alquiler de bienes muebles e intangibles";
        "54 Servicios profesionales científicos y técnicos";
        "56 Servicios de apoyo a los negocios y manejo de desechos y servicios de remediación";
        "61 Servicios educativos";
        "62 Servicios de salud y de asistencia social";
        "71 Servicios de esparcimiento culturales y deportivos y otros servicios recreativos";
        "72 Servicios de alojamiento temporal y de preparación de alimentos y bebidas";
        "81 Otros servicios excepto actividades gubernamentales";
        "21 Minería";
        "22 Generación transmisión y distribución de energía eléctrica suministro de agua y de gas por ductos al consumidor final";
        "23 Construcción";
        "48 - 49 transportes correos y almacenamiento";
        "51 Información en medios masivos";
        "52 Servicios financieros y de seguros";
        "55 Corporativos";
    |]
let getData conn =
    let claveInegiIndex = 1
    let valorAgregadoBase = 41
    let horasTrabajadasBase = 22
    let acervoActivosFijosBase = 3
    let dbData = 
        new NpgsqlCommand("SELECT * FROM k_l_va ORDER BY gid;", conn)
        |> commandToArray
    let municipio (row:string []) =
        row.[60].Substring 4
    seq {
        for row in dbData do
            for ae in [0..actividadesEconomicas.Length - 1] do
                if row.[valorAgregadoBase + ae] <> ""
                then
                    yield [|
                            "2014";                             //Año.Censal
                            municipio row                       //Municipio
                            row.[claveInegiIndex];              //Clave.INEGI
                            actividadesEconomicas.[ae];         //Act.Econ
                            row.[valorAgregadoBase + ae];       //VA
                            row.[horasTrabajadasBase + ae];     //HT
                            row.[acervoActivosFijosBase + ae]   //AF
                          |]
    } |> Seq.toArray


let buildResults host port database username =
    let connectionString = sprintf "Host=%s;Port=%s;Database=%s;Username=%s" host port database username
    let conn = new NpgsqlConnection(connectionString)

    try
        conn.Open()
        let columnNames = getColumnNames
        let data = getData conn
        Array.append [|columnNames|] data
    finally
        conn.Close()

let printResults (data:string [] []) =
    for r in data do
        for d in r do
            printf "%s\t" d
        printfn "\n"

let writeResultsToCSV data =
    let wr = new System.IO.StreamWriter("Data.csv")
    for row in data do
        String.concat "," row |> wr.Write
        wr.WriteLine()
    wr.Close()

let createCSV host port database username =
    buildResults host port database username
    |> writeResultsToCSV

