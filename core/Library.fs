namespace SmsReader.Lib

module TelegramEffect =
    open Telegram.Bot

    let send (token: string) (user: string) message =
        task {
            let client = TelegramBotClient token
            let! _ = client.SendTextMessageAsync(user, message)
            return ()
        }

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
                  "reduce",
                  (fun (args: (unit -> obj) list) ->
                      let f: obj list -> obj = args[0]() |> unbox
                      let init: obj = args[1]()
                      let collection: obj seq = args[2]() |> unbox
                      Seq.fold (fun a x -> f [ a; x ]) init collection |> box)
                  "rest",
                  (fun (args: (unit -> obj) list) ->
                      let arg: obj list = args[0]() |> unbox
                      List.tail arg |> box)
                  "empty?",
                  (fun (args: (unit -> obj) list) ->
                      let arg: obj list = args[0]() |> unbox
                      List.isEmpty arg |> box)
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

    let parseString (value: obj) : string =
        match value with
        | :? RSexp as (RSexp x) when x.StartsWith "\"" -> x.Substring(1, x.Length - 2)
        | :? string as x -> x
        | e -> failwithf "Can't parse '%A' to string" e

    let private getString (r: Map<string, obj>) (key: string) = parseString r[key]

    let executeCode interopFuncResolve code fmain args =
        code
        |> LanguageParser.compile
        |> mapToCoreLang
        |> Interpreter.run2 (extFunctions interopFuncResolve) fmain args

    open System.Threading.Tasks

    let rec main translate interopFuncResolve (code: string) fmain (args: obj list) =
        task {
            let result = executeCode interopFuncResolve code fmain args

            match result with
            | :? Map<string, obj> as dic ->
                let! result =
                    dic
                    |> Map.toSeq
                    |> Seq.map (fun (k, v) ->
                        match k, v with
                        | "telegram", (:? Map<string, obj> as r) ->
                            let token = getString r "token"
                            let user = getString r "user"
                            let message = getString r "message"

                            task {
                                let! _ = TelegramEffect.send token user message
                                return box ()
                            }
                        | "translate", (:? Map<string, obj> as r) ->
                            task {
                                let body = r["body"] :?> string

                                let callback =
                                    match r["callback"] with
                                    | :? RSexp as (RSexp x) -> x
                                    | e -> failwithf "%A" e

                                let! (translateResult: string) = translate body
                                return! main translate interopFuncResolve code callback [ args[0]; translateResult ]
                            }
                        | "log", _ -> Task.FromResult(box $"%A{v}")
                        | eff -> Task.FromResult(box $"Unknown effect %A{eff}"))
                    |> Task.WhenAll

                return sprintf "%A" result |> box
            | r -> return box r
        }

    let main_ translate interopFuncResolve (code: string) fmain (args: obj list) =
        main translate interopFuncResolve $"(module %s{code})" fmain args

module ConfigParse =
    open System.IO
    open System.Reflection

    let private readConfig () =
        let assembly = Assembly.GetExecutingAssembly()
        use stream = assembly.GetManifestResourceStream("core.scripts.config.local.edn")
        (new StreamReader(stream)).ReadToEnd()

    let private makeMain = sprintf "(module (defn main [] %s))"

    let load () =
        ScriptRunner.executeCode (fun _ _ -> failwith "Interop not allowed") (makeMain (readConfig ())) "main" []
        :?> Map<string, obj>
        |> Map.map (fun k v -> ScriptRunner.parseString v |> box)

module SmsReaderApp =
    open System.Net.Http
    module R = ScriptRunner

    let main translate funcResolver env host (sms: Map<string, obj>) log =
        task {
            use client = new HttpClient()
            let mutable lastProgram = ""

            while true do
                try
                    let! program = client.GetStringAsync($"http://%s{host}:8080/")

                    if program <> lastProgram then
                        lastProgram <- program
                        let! result = R.main_ translate funcResolver program "main" [ env; box sms ]
                        result |> string |> log

                    do! Async.Sleep 1_000
                with e ->
                    log (string e)
        }

    let main_ translateEffect androidFunctionResolver sms log =
        let env = ConfigParse.load ()
        main translateEffect androidFunctionResolver (box env) (env["script-host"] |> string) sms log
        |> ignore
