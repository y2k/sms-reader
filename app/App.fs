namespace SmsReader.App

module Config =
    open System.IO
    open System.Reflection

    let readConfig () =
        let assembly = Assembly.GetExecutingAssembly()
        use stream = assembly.GetManifestResourceStream("app.Resources.config.edn")
        (new StreamReader(stream)).ReadToEnd()

module TranslateSample =
    open Android.Gms.Extensions
    open Xamarin.Google.MLKit.NL.Translate

    let main text =
        task {
            let client =
                (new TranslatorOptions.Builder())
                    .SetSourceLanguage(TranslateLanguage.Georgian)
                    .SetTargetLanguage(TranslateLanguage.Russian)
                    .Build()
                |> Translation.GetClient

            try
                do! client.DownloadModelIfNeeded().AsAsync()
                let rt = client.Translate(text)
                do! rt.AsAsync()
                return rt.Result.ToString()
            with _ ->
                return "ERROR"
        }

module SmsReaderApp =
    open System.Net
    module R = SmsReader.Lib.ScriptRunner
    module RV = SmsReader.Common.AndroidFunctionResolver

    let main host (sms: Map<string, obj>) log =
        task {
            let mutable lastProgram = ""

            while true do
                try
                    let! program = (new WebClient()).DownloadStringTaskAsync($"http://%s{host}:8080/")

                    if program <> lastProgram then
                        lastProgram <- program
                        let! result = R.main TranslateSample.main RV.resolve program "main" sms
                        result |> string |> log

                    do! Async.Sleep 1_000
                with e ->
                    log (string e)
        }

open Android.App
open Android.Widget
open Android.Net
open Android.Content

module SmsClient =
    let run (context: Activity) log =
        // date_sent, subject, body, creator, seen, address, person, protocol, read, status, type, service_center
        use cursor =
            context.ContentResolver.Query(Uri.Parse "content://sms/inbox", null, null, null, null)

        let columnNames = cursor.GetColumnNames()
        cursor.MoveToFirst() |> ignore

        let sms =
            columnNames
            |> Seq.map (fun k -> k, cursor.GetString(cursor.GetColumnIndexOrThrow(k)) |> box)
            |> Map.ofSeq

        SmsReaderApp.main "192.168.0.102" sms log |> ignore

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

        // logTextView.Text <- sprintf "Config:%A" (Config.readConfig ())

        SmsClient.run this (fun log -> logTextView.Text <- $"%s{log}\n==================\n%s{logTextView.Text}")
        ()

[<BroadcastReceiver(Enabled = true, Exported = false, Permission = "android.permission.BROADCAST_SMS")>]
[<IntentFilter([| "android.provider.Telephony.SMS_RECEIVED"
                  "android.provider.Telephony.SMS_DELIVER" |])>]
type SmsReceiver() =
    inherit BroadcastReceiver()

    override _.OnReceive(context, _intent) =
        Toast.MakeText(context, "TEST", ToastLength.Long).Show()
