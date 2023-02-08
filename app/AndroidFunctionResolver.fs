module SmsReader.Common.AndroidFunctionResolver

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
    | :? Java.Lang.Object as x -> x
    | x -> failwithf "Unsupported arg type %O (%O)" (x.GetType()) x

let private compareType (cls: Java.Lang.Class) (obj: Java.Lang.Object) =
    let cls =
        if cls = Java.Lang.Integer.Type then
            (new Java.Lang.Integer(0)).Class
        else
            cls

    // printfn "FIXME :: 1) %O -> %O -> %O" cls obj.Class (cls.IsAssignableFrom obj.Class)
    cls.IsAssignableFrom obj.Class

let resolveStatic (name: string) _ (args: (unit -> obj) list) =
    let (fullClsName, methodName) =
        match name.Split '/' with
        | [| pkg; clsName |] -> pkg, clsName
        | x -> failwithf "invalid class name %O" x

    let cls = Java.Lang.Class.ForName fullClsName

    let argValues = args |> List.map resolveParam

    let method =
        cls.GetMethods()
        |> Seq.tryFind (fun m ->
            m.Name = methodName
            && m.ParameterCount = (Seq.length args)
            && Seq.forall2 compareType (m.GetParameterTypes()) argValues)
        |> Option.defaultWith (fun _ ->
            failwithf "Can't find static java method '%O' [%i] for class '%O'" methodName (Seq.length args) cls)

    let result =
        match argValues with
        | [] -> method.Invoke(null) |> box
        | [ a ] -> method.Invoke(null, a) |> box
        | [ a; b ] -> method.Invoke(null, a, b) |> box
        | [ a; b; c ] -> method.Invoke(null, a, b, c) |> box
        | _ -> failwithf "Can't call '%s' with arg count = %i (%A)" methodName (Seq.length args - 1) args

    match result with
    | :? Java.Lang.String as x -> x.ToString() |> box
    | :? Java.Lang.Integer as x -> x.IntValue() |> box
    | :? Java.Lang.Boolean as x -> x.BooleanValue() |> box
    | :? Java.Lang.Object as x -> x
    | _ -> failwith $"Unsupported return type {result.GetType()} ({result})"

let resolve (name: string) _ (args: (unit -> obj) list) =
    let inst = args[0]()
    let methodName: string = name.Substring 1

    let (instance: Java.Lang.Object) =
        match inst with
        | :? string as s -> new Java.Lang.String(s)
        | :? RSexp as (RSexp s) when s.StartsWith '"' -> new Java.Lang.String(s.Substring(1, s.Length - 2))
        | :? Java.Lang.Object as x -> x
        | x -> failwith $"Unsupported type {x} ({x.GetType()})"

    let cls = instance.Class

    let method =
        cls.GetMethods()
        |> Seq.tryFind (fun m -> m.Name = methodName && m.ParameterCount = (Seq.length args - 1))
        |> Option.defaultWith (fun _ ->
            failwithf "Can't find java method '%O' [%i] for class '%O'" methodName (Seq.length args - 1) cls)

    let result =
        match args with
        | [ _ ] -> method.Invoke(instance) |> box
        | [ _; a ] -> method.Invoke(instance, resolveParam a) |> box
        | [ _; a; b ] -> method.Invoke(instance, resolveParam a, resolveParam b) |> box
        | [ _; a; b; c ] -> method.Invoke(instance, resolveParam a, resolveParam b, resolveParam c) |> box
        | _ -> failwithf "Can't call '%s' with arg count = %i (%A)" methodName (Seq.length args - 1) args

    match result with
    | null -> null
    | :? Java.Lang.String as x -> x.ToString() |> box
    | :? Java.Lang.Integer as x -> x.IntValue() |> box
    | :? Java.Lang.Boolean as x -> x.BooleanValue() |> box
    | _ -> failwith $"Unsupported return type {result.GetType()} ({result})"
