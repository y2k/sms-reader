#r "nuget: FParsec, 1.1.1"
#r "../language/meta-lang/src/bin/Debug/net5.0/lib.dll"

open System.IO
open MetaLang

let findNativeFunction (name: string) _ =
    match name with
    | "map" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (xs: obj list) = args[1]() |> unbox
            List.map (fun x -> f [ x ]) xs |> box)
    | _ -> None

let ctx =
    TypeResolver.defaultContext
    |> TypeResolver.registerFunc "first" ([ Specific "list" ], Unknown)
    |> TypeResolver.registerFunc "second" ([ Specific "list" ], Unknown)

let env = ExternalTypeResolver.loadDefault ()

let prog =
    File.ReadAllText "translates.clj"
    |> sprintf "(module %s)"
    |> LanguageParser.compile
    |> mapToCoreLang
    |> TypeResolver.resolve env ctx
    |> ConstantValidator.validate
        (TypeResolver.fundFunctionByArgs ctx)
        (TypeResolver.findFuncArgType ctx)
        (ConstLevelFunctions.invoke)

// printfn "PROG:\n%A\n\n=== === ===\n" prog
prog
|> Interpreter.run findNativeFunction "init" []
|> fun x -> x :?> Map<string, obj> |> Map.find "ui"
|> fun x -> x :?> Map<string, obj> |> Map.find "view"
|> fun x -> (x :?> (obj list -> obj)) [ [ box "foo"; box "bar" ] ]
|> printfn "LOG:\n%A"

// prog
// |> Interpreter.run Map.empty "local-event-handler" [ "" ]
// |> printfn "LOG:\n%A"
