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

        let webView = new WebView(this)
        webView.Id <- 1
        webView.Settings.JavaScriptEnabled <- true
        webView.SetWebViewClient(new MyWebViewClient())
        base.SetContentView(webView)

        WordsLearning.Server.run ()
        |> ignore

        webView.LoadUrl "http://localhost:8080/"

    override this.DispatchKeyEvent(e: KeyEvent) =
        if e.KeyCode = Keycode.Menu && e.Action = KeyEventActions.Down then
            let webview = this.FindViewById(1) :?> WebView
            webview.LoadUrl(webview.Url)

        base.DispatchKeyEvent(e)

and MyWebViewClient() =
    inherit WebViewClient()

    override _.ShouldOverrideUrlLoading((webView: WebView), (url: string)) =
        webView.LoadUrl url
        true
