namespace SmsReader.Lib

module ScriptRunner =
    open MetaLang

    let private extFunctions =
        Map.ofList
            [ "if",
              (fun (args: (unit -> obj) list) ->
                  let condition =
                      match args.[0] () with
                      | :? bool as b -> b
                      | :? RSexp as x -> let (RSexp x) = x in System.Boolean.Parse(x)
                      | x -> failwithf "Can't parse '%O' to bool" x

                  if condition then args.[1] () else args.[2] ())
              "+",
              (fun (args: (unit -> obj) list) ->
                  let toInt (arg: (unit -> obj)) =
                      match arg () with
                      | :? int as x -> x
                      | :? RSexp as x -> let (RSexp x) = x in int x
                      | x -> failwithf "Can't parse '%O' to int" x

                  let a = toInt args.[0]
                  let b = toInt args.[1]
                  a + b |> box) ]

    let main (code: string) : string =
        code
        |> LanguageParser.compile
        |> mapToCoreLang
        |> Interpreter.run extFunctions "main" []
        |> function
            | :? RSexp as (RSexp r) -> r
            | :? int as r -> string r
            | result -> failwithf "Unsupported result (%O) %O" result (result.GetType())
