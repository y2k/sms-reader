namespace sms_reader

open Android.App
open Android.Widget
open Android.Net

[<Activity(Label = "@string/app_name", MainLauncher = true)>]
type MainActivity() =
    inherit Activity()

    override this.OnCreate savedInstanceState =
        base.OnCreate savedInstanceState

        // let cursor =
        //     base.ContentResolver.Query(Uri.Parse "content://sms/inbox", null, null, null, null)
        // Android.Telephony.SmsResult.
        let cursor =
            base.ContentResolver.Query(Uri.Parse "content://sms/inbox", null, null, null, null)

        let count = cursor.Count

        let result = cursor.GetColumnNames() |> Seq.reduce (sprintf "%s, %s")

        let xs =
            Seq.initInfinite (fun i ->
                cursor.MoveToPosition i |> ignore
                cursor)

        // date_sent, subject, body, creator, seen
        // address, person, protocol, read, status, type, service_center

        let result =
            xs
            |> Seq.take 20
            |> Seq.map (fun c ->
                sprintf
                    "%s = %s"
                    (c.GetString(c.GetColumnIndex "address"))
                    (c.GetString(c.GetColumnIndex "service_center")))
            |> Seq.reduce (sprintf "%s\n%s")
        // let result =
        //     xs
        //     |> Seq.take 1
        //     |> Seq.map (fun c ->
        //         c.GetColumnNames()
        //         |> Seq.map (fun n -> sprintf "%s = %s" n (c.GetString(c.GetColumnIndexOrThrow n)))
        //         |> Seq.reduce (sprintf "%s\n%s")
        //     )
        //     |> Seq.reduce (sprintf "%s\n%s")

        base.SetContentView(new TextView(this, Text = result))
