namespace SmsReader.Common

module ReflectionFunctionResolver =
    open System
    open MetaLang

    let resolve (name: string) (_: int) (args: (unit -> obj) list) =
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

        let resolveParam (a: unit -> obj) =
            match a () with
            | :? RSexp as (RSexp v) ->
                let (success, i) = Int32.TryParse v

                if success then
                    Java.Lang.Object.op_Implicit i
                else
                    failwith $"Can't parse RSexp arg = %A{v}"
            | :? int as i -> Java.Lang.Object.op_Implicit i
            | :? string as i -> Java.Lang.Object.op_Implicit i
            | x -> failwithf "Unsupported arg type %O (%O)" (x.GetType()) x

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
        | _ -> failwith $"Unsup return type {result.GetType()} ({result})"
