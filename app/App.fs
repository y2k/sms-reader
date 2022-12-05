﻿namespace SmsReader.App

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
    open System.Net.Http
    module R = SmsReader.Lib.ScriptRunner
    module RV = SmsReader.Common.AndroidFunctionResolver

    let main env host (sms: Map<string, obj>) log =
        task {
            use client = new HttpClient()
            let mutable lastProgram = ""

            while true do
                try
                    let! program = client.GetStringAsync($"http://%s{host}:8080/")

                    if program <> lastProgram then
                        lastProgram <- program
                        let! result = R.main TranslateSample.main RV.resolve program "main" [ env; box sms ]
                        result |> string |> log

                    do! Async.Sleep 1_000
                with e ->
                    log (string e)
        }

open Android.App
open Android.Widget
open Android.Net

module SmsClient =
    let run (context: Activity) log =
        use cursor =
            context.ContentResolver.Query(Uri.Parse "content://sms/inbox", null, null, null, null)

        let columnNames = cursor.GetColumnNames()
        cursor.MoveToFirst() |> ignore

        let sms =
            columnNames
            |> Seq.map (fun k -> k, cursor.GetString(cursor.GetColumnIndexOrThrow(k)) |> box)
            |> Map.ofSeq

        let env = SmsReader.Lib.ConfigParse.load ()
        SmsReaderApp.main (box env) "192.168.88.125" sms log |> ignore

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

        SmsClient.run this (fun log -> logTextView.Text <- $"%s{log}\n==================\n%s{logTextView.Text}")
        ()
