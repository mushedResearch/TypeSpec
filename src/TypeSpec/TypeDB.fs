module TypeDB

open TypeSpec.Types
open System
open System.Collections.Generic

type TypeSelector =
    { Name : string
      Version : string}

type ITypeDB = 
    abstract Get : string -> Type
    abstract GetRaw : string -> TypeRaw
    abstract Put : TypeRaw -> unit

type MemoryDB() =
    let cache = new Dictionary<TypeSelector,TypeRaw>()

    interface ITypeDB with
        member this.Get(arg1: string): Type = 
            failwith "Not Implemented"
        member this.GetRaw(arg1: string): TypeRaw = 
            failwith "Not Implemented"
        member this.Put(arg1: TypeRaw): unit = 
            failwith "Not Implemented"

      