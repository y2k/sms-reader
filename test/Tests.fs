module Tests

open Xunit

module R = SmsReader.Lib.ScriptRunner

[<Fact>]
let test3 () =
    let arg = Map.ofList [ "body", box "7" ]

    let actual =
        R.main
            """
(module
  (defn main [arg]
    [(get arg :body)]))"""
            arg

    Assert.Equal("[7]", actual)

[<Fact>]
let test2 () =
    let arg = Map.ofList [ "body", box "7" ]

    let actual =
        R.main
            """
(module
  (defn main [arg]
    (get arg :body)))"""
            arg

    Assert.Equal("7", actual)

[<Fact>]
let test () =
    let arg = Map.ofList [ "body", box "7" ]

    let actual =
        R.main
            """
(module
  (defn main [arg]
    (get arg "body")))"""
            arg

    Assert.Equal("7", actual)
