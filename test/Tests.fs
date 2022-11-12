module Tests

open Xunit

module R = SmsReader.Lib.ScriptRunner

[<Fact>]
let test () =
    let actual = R.main "(module (defn main [] 7))"
    Assert.Equal("7", actual)
