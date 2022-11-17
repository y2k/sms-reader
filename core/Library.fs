namespace SmsReader.Lib

module ScriptRunner =
    open MetaLang

    let private extFunctions interopFuncResolve (name: string) (argCount: int) =
        if name.StartsWith "." then
            interopFuncResolve name argCount |> Some
        else
            Map.ofList
                [ "if",
                  (fun (args: (unit -> obj) list) ->
                      let condition =
                          match args.[0] () with
                          | :? bool as b -> b
                          | :? RSexp as x -> let (RSexp x) = x in System.Boolean.Parse(x)
                          | x -> failwithf "Can't parse '%O' to bool" x

                      if condition then args.[1] () else args.[2] ())
                  "first",
                  (fun (args: (unit -> obj) list) ->
                      let arg: obj list = args[0]() |> unbox
                      List.tryHead arg |> Option.defaultValue null)
                  "rest",
                  (fun (args: (unit -> obj) list) ->
                      let arg: obj list = args[0]() |> unbox
                      List.tail arg |> box)
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
            |> Map.tryFind name

    let main interopFuncResolve (code: string) (arg: Map<string, obj>) : string =
        code
        |> LanguageParser.compile
        |> mapToCoreLang
        |> Interpreter.run2 (extFunctions interopFuncResolve) "main" [ arg ]
        |> sprintf "%A"
