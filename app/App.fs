namespace SmsReader.App

open Android.App
open Android.Webkit
open SmsReader

[<Activity(Label = "@string/app_name", MainLauncher = true, Theme = "@android:style/Theme.NoTitleBar")>]
type MainActivity() =
    inherit Activity()

    override this.OnCreate savedInstanceState =
        base.OnCreate savedInstanceState

        let webview = new WebView(this)
        webview.Settings.JavaScriptEnabled <- true
        base.SetContentView(webview)

        WordsLearning.Server.run () |> ignore

        webview.LoadUrl "http://localhost:8080/"
