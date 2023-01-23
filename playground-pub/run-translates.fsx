#r "nuget: FParsec, 1.1.1"
#r "../language/meta-lang/src/bin/Debug/net5.0/lib.dll"

open System.IO
open MetaLang

let private asString (instance: obj) =
    match instance with
    | :? string as s -> s
    | :? RSexp as (RSexp s) when s.StartsWith '"' && s.EndsWith '"' -> s.Substring(1, s.Length - 2)
    | :? RSexp as (RSexp s) -> s
    | s -> failwithf "Can't parse to string: %A (%O)" s (instance.GetType())

// let private clearQuotes (instance: obj) =
//     printfn "1) %O" instance

//     match instance with
//     | :? RSexp as (RSexp s) when s.StartsWith '"' && s.EndsWith '"' ->
//         printfn "2) %O" instance
//         RSexp(s.Substring(1, s.Length - 2)) |> box
//     | :? RSexp as (RSexp s) ->
//         let t = s.StartsWith '"' && s.EndsWith '"'
//         printfn "3.1) |%O|%A|%O" s s t
//         instance
//     | instance ->
//         printfn "3.2) %O" instance
//         instance

let private unwrapRSexp (instance: obj) =
    match instance with
    | :? RSexp as (RSexp s) when s.StartsWith '"' && s.EndsWith '"' -> s.Substring(1, s.Length - 2) |> box
    | :? RSexp as (RSexp s) -> s |> box
    | instance -> instance

let findNativeFunction (name: string) _ =
    match name with
    | "if" ->
        Some(fun (args: (unit -> obj) list) ->
            let condition =
                match args.[0] () with
                | :? bool as b -> b
                | :? RSexp as x -> let (RSexp x) = x in System.Boolean.Parse(x)
                | x -> failwithf "Can't parse '%O' to bool" x

            if condition then args.[1] () else args.[2] ())
    | "some?" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj) = args[0]() |> unbox
            not (isNull l))
    | "str" -> Some(fun (args: (unit -> obj) list) -> Seq.fold (fun a x -> a + (x () |> asString)) "" args |> box)
    | "name" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj) = args[0]()
            l |> asString |> box)
    | "first" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            List.head l |> box)
    | "get" ->
        Some(fun (args: (unit -> obj) list) ->
            let (m: Map<string, obj>) = args[0]() |> unbox
            let (k: string) = args[1]() |> asString
            Map.tryFind k m |> Option.defaultValue null |> box)
    | "and" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: bool) = args[0]() |> unbox
            if l then args[1]() |> unbox else false |> box)
    | "not=" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj) = args[0]() |> unwrapRSexp
            let (r: obj) = args[1]() |> unwrapRSexp
            // printfn "LOG: not= | %O | %O || %O | %O" l (l.GetType()) r (r.GetType())
            l <> r |> box)
    | "rest" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            List.tail l |> box)
    | "second" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            l.[1] |> box)
    | "concat" ->
        Some(fun (args: (unit -> obj) list) ->
            let (l: obj list) = args[0]() |> unbox
            let (r: obj list) = args[1]() |> unbox
            List.concat [ l; r ] |> box)
    | "map" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (xs: obj list) = args[1]() |> unbox
            List.map (fun x -> f [ x ]) xs |> box)
    | "reduce" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (i: obj) = args[1]()
            let (xs: obj list) = args[2]() |> unbox
            List.fold (fun a x -> f [ a; x ] |> box) i xs)
    | "vec" ->
        Some(fun (args: (unit -> obj) list) ->
            let (m: Map<string, obj>) = args[0]() |> unbox
            Map.toSeq m |> Seq.map (fun (k, v) -> box [ box k; box v ]) |> Seq.toList |> box)
    | "filter" ->
        Some(fun (args: (unit -> obj) list) ->
            let (f: obj list -> obj) = args[0]() |> unbox
            let (xs: obj list) = args[1]() |> unbox
            List.filter (fun x -> f [ x ] |> unbox) xs |> box)
    | _ -> None

let ctx =
    TypeResolver.defaultContext
    |> TypeResolver.registerFunc "vec" ([ Unknown ], Specific "list")
    |> TypeResolver.registerFunc "concat" ([ Specific "list"; Specific "list" ], Specific "list")
    |> TypeResolver.registerFunc "get" ([ Unknown; Keyword ], Unknown)
    |> TypeResolver.registerFunc "first" ([ Specific "list" ], Unknown)
    |> TypeResolver.registerFunc "some?" ([ Unknown ], Specific "bool")
    |> TypeResolver.registerFunc "second" ([ Specific "list" ], Unknown)
    |> TypeResolver.registerFunc "rest" ([ Specific "list" ], Specific "list")
    |> TypeResolver.registerFunc "name" ([ Keyword ], Specific "string")
    |> TypeResolver.registerFunc "and" ([ Specific "bool"; Specific "bool" ], Specific "bool")
    |> TypeResolver.registerFunc "not=" ([ Specific "bool"; Specific "bool" ], Specific "bool")
    |> TypeResolver.registerFunc
        "reduce"
        ([ Function([ Unknown ], Specific "bool"); Unknown; Specific "list" ], Specific "list")
    |> TypeResolver.registerFunc "filter" ([ Function([ Unknown ], Specific "bool"); Specific "list" ], Specific "list")
    |> TypeResolver.registerVarArgsFunc "str" Unknown (Specific "string")

let env = ExternalTypeResolver.loadDefault ()

printfn "\n=== VALIDATE ===\n"

let prog =
    File.ReadAllLines "translates.clj"
    |> Seq.filter (fun x -> not <| x.TrimStart().StartsWith(";;"))
    |> Seq.reduce (sprintf "%s\n%s")
    |> sprintf "(module %s)"
    |> LanguageParser.compile
    |> mapToCoreLang
    |> TypeResolver.resolve env ctx
    |> ConstantValidator.validate
        (TypeResolver.fundFunctionByArgs ctx)
        (TypeResolver.findFuncArgType ctx)
        (ConstLevelFunctions.invoke)

printfn "\n=== RUN ===\n"
// printfn "PROG:\n%A\n\n=== === ===\n" prog
prog
// |> Interpreter.run findNativeFunction "view" [ [ box "foo"; box "bar" ] ]
|> Interpreter.run findNativeFunction "init" []
|> fun x -> x :?> Map<string, obj> |> Map.find "ui"
|> fun x -> x :?> Map<string, obj> |> Map.find "view"
|> fun x -> (x :?> (obj list -> obj)) [ [ box "foo"; box "bar" ] ]
|> printfn "LOG:\n%A"

// prog
// |> Interpreter.run Map.empty "local-event-handler" [ "" ]
// |> printfn "LOG:\n%A"