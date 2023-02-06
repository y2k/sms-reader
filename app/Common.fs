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

module AndroidFunctionResolver =
    open System
    open MetaLang

    let private resolveParam (a: unit -> obj) =
        match a () with
        | :? RSexp as (RSexp v) ->
            let (success, i) = Int32.TryParse v

            if success then
                Java.Lang.Object.op_Implicit i
            else if v.StartsWith '"' && v.EndsWith '"' then
                Java.Lang.Object.op_Implicit (v.Substring(1, v.Length - 2))
            else
                failwith $"Can't parse RSexp arg = %A{v}"
        | :? int as i -> Java.Lang.Object.op_Implicit i
        | :? string as i -> Java.Lang.Object.op_Implicit i
        | x -> failwithf "Unsupported arg type %O (%O)" (x.GetType()) x

    let resolveStatic (name: string) _ (args: (unit -> obj) list) =
        let (fullClsName, methodName) =
            match name.Split '/' with
            | [| pkg; clsName |] -> pkg, clsName
            | x -> failwithf "invalid class name %O" x

        let cls = Java.Lang.Class.ForName fullClsName

        let method =
            cls.GetMethods()
            |> Seq.find (fun m -> m.Name = methodName && m.ParameterCount = (Seq.length args - 1))

        let result =
            match args with
            | [ _ ] -> method.Invoke(null) |> box
            | [ _; a ] -> method.Invoke(null, resolveParam a) |> box
            | [ _; a; b ] -> method.Invoke(null, resolveParam a, resolveParam b) |> box
            | [ _; a; b; c ] -> method.Invoke(null, resolveParam a, resolveParam b, resolveParam c) |> box
            | _ -> failwithf "Can't call '%s' with arg count = %i (%A)" methodName (Seq.length args - 1) args

        match result with
        | :? Java.Lang.String as x -> x.ToString() |> box
        | :? Java.Lang.Integer as x -> x.IntValue() |> box
        | :? Java.Lang.Boolean as x -> x.BooleanValue() |> box
        | _ -> failwith $"Unsupported return type {result.GetType()} ({result})"

    let resolve (name: string) _ (args: (unit -> obj) list) =
        let inst = args[0]()
        let methodName: string = name.Substring 1

        let instance =
            match inst with
            | :? string as s -> new Java.Lang.String(s)
            | :? RSexp as (RSexp s) when s.StartsWith '"' -> new Java.Lang.String(s.Substring(1, s.Length - 2))
            | x -> failwith $"Unsupported type {x} ({x.GetType()})"

        let cls = Java.Lang.Class.FromType(instance.GetType())

        let method =
            cls.GetMethods()
            |> Seq.find (fun m -> m.Name = methodName && m.ParameterCount = (Seq.length args - 1))

        let result =
            match args with
            | [ _ ] -> method.Invoke(instance) |> box
            | [ _; a ] -> method.Invoke(instance, resolveParam a) |> box
            | [ _; a; b ] -> method.Invoke(instance, resolveParam a, resolveParam b) |> box
            | [ _; a; b; c ] -> method.Invoke(instance, resolveParam a, resolveParam b, resolveParam c) |> box
            | _ -> failwithf "Can't call '%s' with arg count = %i (%A)" methodName (Seq.length args - 1) args

        match result with
        | :? Java.Lang.String as x -> x.ToString() |> box
        | :? Java.Lang.Integer as x -> x.IntValue() |> box
        | :? Java.Lang.Boolean as x -> x.BooleanValue() |> box
        | _ -> failwith $"Unsupported return type {result.GetType()} ({result})"
