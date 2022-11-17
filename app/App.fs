namespace SmsReader.App

module SmsReaderApp =
    open System.Net
    module R = SmsReader.Lib.ScriptRunner
    module RV = SmsReader.Common.ReflectionFunctionResolver

    let main (sms: Map<string, obj>) log =
        async {
            let mutable lastProgram = ""

            while true do
                let! programResp =
                    // (new WebClient()).DownloadStringTaskAsync("http://192.168.0.102:8080/")
                    (new WebClient()).DownloadStringTaskAsync("http://192.168.137.162:8080/")
                    |> Async.AwaitTask
                    |> Async.Catch

                match programResp with
                | Choice1Of2 program when program <> lastProgram ->
                    lastProgram <- program
                    R.main RV.resolve program sms |> log
                | _ -> ()

                do! Async.Sleep 1_000
        }
        |> Async.Catch
        |> fun a ->
            async.Bind(
                a,
                fun r ->
                    match r with
                    | Choice1Of2 _ -> log "END"
                    | Choice2Of2 e -> log (string e)

                    async.Zero()
            )
        |> Async.StartImmediate

open Android.App
open Android.Widget
open Android.Net

module SmsClient =
    let run (context: Activity) (logTextView: TextView) =
        // date_sent, subject, body, creator, seen, address, person, protocol, read, status, type, service_center
        let cursor =
            context.ContentResolver.Query(Uri.Parse "content://sms/inbox", null, null, null, null)

        let columnNames = cursor.GetColumnNames()

        cursor
        |> Seq.unfold (fun cur ->
            if cur.MoveToNext() then
                let kv =
                    columnNames
                    |> Seq.map (fun k -> k, cur.GetString(cur.GetColumnIndexOrThrow(k)) |> box)
                    |> Map.ofSeq

                Some(kv, cur)
            else
                None)
        |> Seq.truncate 3
        |> Seq.iter (fun sms ->
            SmsReaderApp.main sms (fun log -> logTextView.Text <- $"%s{log}\n==================\n%s{logTextView.Text}"))

[<Activity(Label = "@string/app_name", MainLauncher = true)>]
type MainActivity() =
    inherit Activity()

    override this.OnCreate savedInstanceState =
        base.OnCreate savedInstanceState

        let logTextView = new TextView(this)
        logTextView.SetBackgroundColor(Android.Graphics.Color.Black)
        logTextView.SetTextColor(Android.Graphics.Color.DarkSeaGreen)
        base.SetContentView(logTextView)

        SmsClient.run this logTextView
