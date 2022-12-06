namespace SmsReader.App

open Android.App
open Android.Widget
open SmsReader.Lib
open SmsReader.Common

[<Activity(Label = "@string/app_name", MainLauncher = true)>]
type MainActivity() =
    inherit Activity()

    override this.OnCreate savedInstanceState =
        base.OnCreate savedInstanceState

        let scroll = new ScrollView(this)
        scroll.SetBackgroundColor(Android.Graphics.Color.Black)
        let logTextView = new TextView(this)
        logTextView.SetTextColor(Android.Graphics.Color.DarkSeaGreen)
        scroll.AddView(logTextView)
        base.SetContentView(scroll)

        SmsReaderApp.main_ TranslateEffect.main AndroidFunctionResolver.resolve (SmsSource.run this) (fun log ->
            logTextView.Text <- $"%s{log}\n==================\n%s{logTextView.Text}")
        |> ignore
