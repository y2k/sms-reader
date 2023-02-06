namespace SmsReader.App

open Android.App
open Android.Webkit
open Android.Views
open SmsReader

[<Activity(Label = "@string/app_name", MainLauncher = true, Theme = "@android:style/Theme.DeviceDefault.NoActionBar")>]
type MainActivity() =
    inherit Activity()

    override this.OnCreate savedInstanceState =
        base.OnCreate savedInstanceState

        let webview = new WebView(this)
        webview.Id <- 1
        webview.Settings.JavaScriptEnabled <- true
        base.SetContentView(webview)

        WordsLearning.Server.run () |> ignore

        webview.LoadUrl "http://localhost:8080/"

    override this.DispatchKeyEvent(e: KeyEvent) =
        if e.KeyCode = Keycode.Menu && e.Action = KeyEventActions.Down then
            let webview = this.FindViewById(1) :?> WebView
            webview.LoadUrl(webview.Url)

        base.DispatchKeyEvent(e)
