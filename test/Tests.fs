module Tests

open Xunit

module private TestFuncResolver =
    open MetaLang

    let resolve name argCount =
        match name, argCount with
        | ".toUpperCase", 1 ->
            (fun (args: (unit -> obj) list) ->
                let instance = args[0]()
                let inst: string = unbox instance
                inst.ToUpperInvariant() |> box)
        | ".substring", 3 ->
            (fun (args: (unit -> obj) list) ->
                let inst: string = args[0]() |> unbox
                let (RSexp from) = args[1]() |> unbox
                let (RSexp len) = args[2]() |> unbox
                inst.Substring(int from, int len) |> box)
        | _ -> failwithf "Cant resolve method %s [%i]" name argCount

    let main = SmsReader.Lib.ScriptRunner.main resolve

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

    Assert.Equal("\"bc\"", actual)

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

    Assert.Equal("\"ABC\"", actual)

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

    Assert.Equal("[\"7\"]", actual)
