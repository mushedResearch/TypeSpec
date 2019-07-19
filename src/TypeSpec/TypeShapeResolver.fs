
module TypeShapeResolver

open System
open System.Collections.Generic
open System.Runtime.Serialization
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

open TypeShape.Core
open TypeShape.Core.Core

open TypeSystem.Types

    //| Constant of Type * Instance
    //| Reference of Named * ReferenceSpec
    //| Named of NamedType


type RecordShapeGroup =
    | FSharpRecord of IShapeFSharpRecord
    | CliMutable of IShapeCliMutable

let private SomeU = Some() // avoid allocating all the time

let (|Binary|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.UTCDateTime(spec), Shape.ByteArray -> Some(spec)
    | _,_ -> None

let (|DateTime|_|) (t : Type, s : TypeShape ) =
    match t,s with
    | Type.UTCDateTime(spec), :? TypeShape<DateTime> -> Some(spec)
    | _,_ -> None

let (|Byte|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Byte(spec), :? TypeShape<uint8> -> Some(spec)
    | _,_ -> None

let (|Decimal|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Decimal(spec), :? TypeShape<decimal> -> Some(spec)
    | _,_ -> None

let (|Double|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Double(spec), :? TypeShape<float> -> Some(spec)
    | _,_ -> None

let (|UInt64|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.UInt64(spec), :? TypeShape<uint64> -> Some(spec)
    | _,_ -> None

let (|Int64|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Int64(spec), :? TypeShape<int64> -> Some(spec)
    | _,_ -> None

let (|Int|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Int(spec), :? TypeShape<int> -> Some(spec)
    | _,_ -> None

let (|String|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.String(spec), :? TypeShape<string> -> Some(spec)
    | _,_ -> None

let (|Bool|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Bool, :? TypeShape<bool> -> SomeU
    | _,_ -> None


let (|Guid|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Guid, :? TypeShape<Guid> -> SomeU
    | _,_ -> None

let (|Unit|_|) (t : Type, s : TypeShape) =
    match t,s with
    | Type.Unit, :? TypeShape<unit> -> SomeU
    | _,_ -> None

let (|Array|_|) (t : Type, s : TypeShape) =
    match t, s with
    | Type.Array(t), Shape.Array(opt) when opt.Rank = 1 -> Some(t,opt)
    | _,_ ->  None

let (|Union|_|) (t : Type, s : TypeShape) =
    match t, s with
    | Type.Sum(spec), Shape.FSharpUnion(opt) -> Some(spec, opt)
    | _,_ -> None

let (|Record|_|) (t : Type, s : TypeShape) =
    match t with
    | Type.Product(spec) ->
        match s with
        | Shape.FSharpRecord(opt) -> Some(spec,RecordShapeGroup.FSharpRecord opt)
        | Shape.CliMutable(opt) -> Some(spec,RecordShapeGroup.CliMutable opt)
        | _ -> None
    | _ -> None

