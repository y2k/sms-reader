namespace SmsReader.Common

module TranslateEffect =
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

module SmsSource =
    open Android.App
    open Android.Net

    let run (context: Activity) =
        use cursor =
            context.ContentResolver.Query(Uri.Parse "content://sms/inbox", null, null, null, null)

        let columnNames = cursor.GetColumnNames()
        cursor.MoveToFirst() |> ignore

        columnNames
        |> Seq.map (fun k -> k, cursor.GetString(cursor.GetColumnIndexOrThrow(k)) |> box)
        |> Map.ofSeq
