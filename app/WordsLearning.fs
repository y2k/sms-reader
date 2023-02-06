module SmsReader.WordsLearning

open System.Text.Json
open MetaLang

let private findNativeFunction (name: string) _ =
    match name with
    | _ -> DefaultFunctions.findNativeFunction name ()

let private ctx = DefaultFunctions.defaultContext

let private env = ExternalTypeResolver.loadDefault ()

printfn "\n=== VALIDATE ===\n"

let private globalEffects: obj ResizeArray = ResizeArray()
let private inputHistory: obj ResizeArray = ResizeArray()
let private stateHistory: obj ResizeArray = ResizeArray()

module private CodeResolver =
    open System.Net.Http

    let resolve (appUrl: string) =
        let client = new HttpClient()
        client.GetStringAsync appUrl

let private runProgram appUrl (arg: Map<string, obj>) =
    task {
        let! code = CodeResolver.resolve appUrl

        let prog =
            code.Split [| '\n'; '\r' |]
            |> Seq.filter (fun x -> not <| x.TrimStart().StartsWith(";;"))
            |> Seq.reduce (sprintf "%s\n%s")
            |> sprintf "(module %s)"
            |> LanguageParser.parse
            |> MacroExpand.expandSexp
            |> LanguageParser.compileToExtNode
            |> mapToCoreLang
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

        let input = arg
        inputHistory.Add input

        let effect = call "update" [ input ]
        globalEffects.Add effect

        let state: Map<string, obj> = Map.empty

        let state =
            globalEffects |> Seq.fold (fun db eff -> call "restore-state" [ db; eff ]) state

        stateHistory.Add state

        inputHistory |> JsonSerializer.Serialize |> printfn "LOG: IN's: %O"
        stateHistory |> JsonSerializer.Serialize |> printfn "LOG: STATE's: %O"

        return call "view" [ state ] |> string
    }

module Server =
    open System.IO
    open System.Net
    open System.Text
    open System.Web

    let private appUrl =
        // "https://raw.githubusercontent.com/y2k/sms-reader/master/playground-pub/translates.clj"
        "http://192.168.20.19:8081/y2k/sms-reader/master/playground-pub/translates.clj"

    let run () =
        let listner = new HttpListener()
        listner.Prefixes.Add("http://localhost:8080/")
        listner.Start()

        task {
            while true do
                let! ctx = listner.GetContextAsync()

                let! response =
                    if ctx.Request.HttpMethod = "POST" then
                        task {
                            let! formString = (new StreamReader(ctx.Request.InputStream)).ReadToEndAsync()

                            let form =
                                HttpUtility.ParseQueryString(formString)
                                |> fun xs -> xs.AllKeys |> Seq.map (fun k -> k, box xs[k]) |> Map.ofSeq

                            return! runProgram appUrl form
                        }
                    else
                        runProgram appUrl Map.empty

                do! ctx.Response.OutputStream.WriteAsync(Encoding.UTF8.GetBytes response)
                ctx.Response.Close()
        }
