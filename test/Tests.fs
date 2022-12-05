module Tests

open Xunit

module private TestFuncResolver =
    open MetaLang

    let private asString (instance: obj) =
        match instance with
        | :? string as s -> s
        | :? RSexp as (RSexp s) when s.StartsWith '"' && s.EndsWith '"' -> s.Substring(1, s.Length - 2)
        | s -> failwithf "%A" s

    let private asInt (instance: obj) =
        match instance with
        | :? int as s -> s
        | :? RSexp as (RSexp s) -> int s
        | s -> failwithf "%A" s

    let resolve name argCount =
        match name, argCount with
        | ".isEmpty", 1 ->
            (fun (args: (unit -> obj) list) ->
                let inst: string = args[0]() |> asString
                inst.Length = 0 |> box)
        | ".toUpperCase", 1 ->
            (fun (args: (unit -> obj) list) ->
                let instance = args[0]()
                let inst: string = unbox instance
                inst.ToUpperInvariant() |> box)
        | ".length", 1 ->
            (fun (args: (unit -> obj) list) ->
                let inst: string = args[0]() |> asString
                inst.Length |> box)
        | ".startsWith", 2 ->
            (fun (args: (unit -> obj) list) ->
                let (RSexp argWithQuotes) = args[1]() |> unbox
                let instance = args[0]() |> asString
                let arg = argWithQuotes.Substring(1, argWithQuotes.Length - 2)
                instance.StartsWith(arg) |> box)
        | ".concat", 2 ->
            (fun (args: (unit -> obj) list) ->
                let instance = args[0]() |> asString
                let arg = args[1]() |> asString
                instance + arg |> box)
        | ".substring", 2 ->
            (fun (args: (unit -> obj) list) ->
                let inst: string = args[0]() |> asString
                let from = args[1]() |> asInt
                inst.Substring(int from) |> box)
        | ".substring", 3 ->
            (fun (args: (unit -> obj) list) ->
                let inst: string = args[0]() |> unbox
                let (RSexp from) = args[1]() |> unbox
                let (RSexp len) = args[2]() |> unbox
                inst.Substring(int from, int len) |> box)
        | _ -> failwithf "Cant resolve method %s [%i]" name argCount

    let main code arg =
        (task {
            let! r = SmsReader.Lib.ScriptRunner.main (fun _ -> failwith "???") resolve code "main" [ box arg ]

            return string r
        })
            .Result

[<Fact>]
let test8 () =
    let arg = Map.ofList []

    let actual =
        TestFuncResolver.main
            """
(module
 (defn check [s rules]
   (let [r (first rules)
         from (first r)
         to (first (rest r))]
     (if (.startsWith s from)
       to
       (check s (rest rules)))))

 (def rules [["a" "ა"] ["b" "ბ"] ["g" "გ"]])

 (defn decode [s]
   (if (.isEmpty s)
     ""
     (let [ch (check s rules)]
       (.concat ch (decode (.substring s (.length ch)))))))

 (defn main [_] (decode "abgba")))
"""
            arg

    Assert.Equal("აბგბა", actual)

[<Fact>]
let test7 () =
    let arg = Map.ofList [ "body", box "abcdef" ]

    let actual =
        TestFuncResolver.main
            """
(module
 (defn main [event]
   (.substring (:body event) 1 2)))
"""
            arg

    Assert.Equal("bc", actual)

[<Fact>]
let test6 () =
    let arg = Map.ofList [ "body", box "abc" ]

    let actual =
        TestFuncResolver.main
            """
(module
 (defn main [event]
   (.toUpperCase (:body event))))
"""
            arg

    Assert.Equal("ABC", actual)

[<Fact>]
let test5 () =
    let arg = Map.ofList [ "body", box "7" ]

    let actual =
        TestFuncResolver.main
            """
(module

 (defn on-translate-end [resp]
   [[:send-telegram
     {:message (:body resp)}]])

 (defn main [event]
   [[:http
     {:method :POST
      :url "https://translate.google.com/api?d=ka-ru"
      :body (:body event)
      :callback :on-translate-end}]]))
"""
            arg

    ()

[<Fact>]
let test4 () =
    let arg = Map.ofList [ "body", box "7" ]

    let actual =
        TestFuncResolver.main
            """
(module
  (defn main [arg]
    [(:body arg)]))"""
            arg

    Assert.Equal("[7]", actual)
