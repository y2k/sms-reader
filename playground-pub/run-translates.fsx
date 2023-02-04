#r "nuget: FParsec, 1.1.1"
#r "nuget: Suave, 2.6.2"
#r "../language/meta-lang/src/bin/Debug/net6.0/lib.dll"

open System.IO
open MetaLang
open System.Text.Json

let findNativeFunction (name: string) _ =
    match name with
    | _ -> DefaultFunctions.findNativeFunction name ()

let ctx = DefaultFunctions.defaultContext

let env = ExternalTypeResolver.loadDefault ()

printfn "\n=== VALIDATE ===\n"

let globalEffects: obj ResizeArray = ResizeArray()
let inputHistory: obj ResizeArray = ResizeArray()
let stateHistory: obj ResizeArray = ResizeArray()

let runProgram (arg: Map<string, obj>) =
    let prog =
        File.ReadAllLines "translates.clj"
        |> Seq.filter (fun x -> not <| x.TrimStart().StartsWith(";;"))
        |> Seq.reduce (sprintf "%s\n%s")
        |> sprintf "(module %s)"
        |> LanguageParser.parse
        |> MacroExpand.expandSexp
        |> LanguageParser.compileToExtNode
        |> mapToCoreLang
        |> MacroExpand.run
        |> TypeResolver.resolve env ctx
        |> ConstantValidator.validate
            (TypeResolver.fundFunctionByArgs ctx)
            (TypeResolver.findFuncArgType ctx)
            (ConstLevelFunctions.invoke)

    printfn "\n=== RUN ===\n"

    let call name (args: obj list) =
        prog
        |> Interpreter.run findNativeFunction "init" []
        |> fun x -> x :?> Map<string, obj> |> Map.find "ui"
        |> fun x -> x :?> Map<string, obj> |> Map.find name
        |> fun x -> (x :?> (obj list -> obj)) args

    // let input = [ box "web"; arg ]
    let input = arg
    inputHistory.Add input

    let effect = call "update" [ input ]
    globalEffects.Add effect

    let state: Map<string, obj> = Map.empty

    let state =
        globalEffects |> Seq.fold (fun db eff -> call "restore-state" [ db; eff ]) state

    stateHistory.Add state

    // input |> JsonSerializer.Serialize |> printfn "LOG: IN: %O"
    // effect |> JsonSerializer.Serialize |> printfn "LOG: OUT: %O"
    inputHistory |> JsonSerializer.Serialize |> printfn "LOG: IN's: %O"
    // globalEffects |> JsonSerializer.Serialize |> printfn "LOG: EFFECTS: %O"
    stateHistory |> JsonSerializer.Serialize |> printfn "LOG: STATE's: %O"
    // state |> JsonSerializer.Serialize |> printfn "LOG: STATE: %O"

    call "view" [ state ] |> string

module Server =
    open Suave
    open Suave.Operators
    open Suave.Filters

    choose
        [ POST
          >=> request (fun r ->
              r.form
              |> Map.ofList
              |> Map.map (fun _ x -> Option.defaultValue "" x |> box)
              |> runProgram
              |> Successful.OK)
          GET >=> request (fun _ -> Successful.OK(runProgram Map.empty)) ]
    |> startWebServer defaultConfig
