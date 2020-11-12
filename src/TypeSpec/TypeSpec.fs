namespace TypeSpec


module Types =
  open System 
  open System.Text.RegularExpressions

  //Spec i.e. Dependent Types
  type RangeSpec< ^T >  when ^T : (static member ( >= ) : ^T -> ^T -> bool)  and ^T : comparison  =
    { Min : ^T option
      Max : ^T option }
    with 
    static member inline Validate (spec : RangeSpec< ^T > when ^T : (static member ( >= ) : ^T -> ^T -> bool)) (value : ^T) =
      match spec.Min, spec.Max with
      | Some(min),Some(max) -> max >= value && value >= min
      | Some(min),None -> min >= value
      | None,Some(max) -> max >= value
      | None,None -> true
    static member inline Default () =
      { Min = None
        Max = None }

  and LengthSpec< ^T > when ^T : ( member Length :  int) = 
    { Range : RangeSpec<int> }
    with 
    static member inline Validate (spec : LengthSpec< ^T > when ^T : (member Length : int)) (value : ^T) =
      RangeSpec<int>.Validate spec.Range (^T : ( member Length :  int) value)
    static member inline Default () =
      { Range = RangeSpec<int>.Default() }

  and RegexSpec =
    { Regex : string option}
    with 
    static member inline Validate (spec : RegexSpec) (value : string) =
      match spec.Regex with
      | Some(regex) -> 
        if String.IsNullOrWhiteSpace value 
          then false
          else
              let s' = value.Trim()
              Regex(regex).IsMatch s'
      | None -> true
    static member inline Default () =
      { Regex = None }

  and ContainsSpec< ^T > when ^T : equality =
    { Items : ^T array}
    with
    static member inline Validate (spec : ContainsSpec< ^T > ) (values : ^T array) : bool =
      Array.forall (fun x -> Array.contains x spec.Items) values
    static member inline Default () =
      { Items = Array.empty }

  and StringSpec =
    { Length : RangeSpec<int> option
      Regex : RegexSpec option}
    with
    static member inline Validate (spec : StringSpec) (value : string) =
      match spec.Length, spec.Regex with
      | Some(len),Some(regex) -> RangeSpec<int>.Validate len value.Length && RegexSpec.Validate regex value
      | Some(len),None -> RangeSpec<int>.Validate len value.Length
      | None,Some(regex) -> RegexSpec.Validate regex value
      | None,None -> true
    static member Default () =
      { Length = None
        Regex = None }

  and ArraySpec< ^T> when ^T : equality = 
    { Length : LengthSpec< ^T array> option 
      Contains : ContainsSpec< ^T> option }
    static member inline Validate (spec : ArraySpec< ^T>) (value : ^T array) =
      match spec.Length, spec.Contains with
      | Some(x), Some(y) -> LengthSpec< ^T array>.Validate x value && ContainsSpec< ^T >.Validate y value
      | Some(x), None -> LengthSpec< ^T array>.Validate x value
      | None, Some(y) -> ContainsSpec< ^T >.Validate y value
      | None, None -> true
    static member inline Default () =
      { Length = None
        Contains = None }

  and DateTimeSpec =
    { UTC : bool
      Between : RangeSpec<DateTime> option }
    static member inline Validate (spec : DateTimeSpec) (value : DateTime) =
      if spec.UTC && (value.ToUniversalTime().ToBinary() <> value.ToBinary()) then false
      else
        match spec.Between with
        | Some(b) -> RangeSpec<DateTime>.Validate b value
        | None -> true
    static member inline Default () =
      { UTC = true
        Between = None }

  and ReferenceSpec =
    { DocumentType : string option }
    with
    static member inline Validate (spec : ReferenceSpec) (value : Reference) =
      match spec.DocumentType with
      | Some(str) -> str = (^T : (member DocumentType : string) value)
      | None -> false
    static member inline Default () =
      { DocumentType = None }

  and ProductSpec = 
    { Items : ContainsSpec<string> option
      Query : ContainsSpec<string * Instance> option}
    with
    static member inline Validate (spec : ProductSpec) (value : (string * Instance) []) =
      match spec.Items, spec.Query with
      | Some(i), Some(q) -> ContainsSpec<string>.Validate i (value |> Array.map fst) && ContainsSpec<string * Instance>.Validate q value
      | Some(i), None -> ContainsSpec<string>.Validate i (value |> Array.map fst)
      | None, Some(q) -> ContainsSpec<string * Instance>.Validate q value
      | None, None -> true
    static member inline Default () =
      { Items = None
        Query = None }

  and SumSpec = 
    { Subset : string [] option }
    with
    static member inline Validate (spec : SumSpec) (value : (string * Instance)) =
      if spec.Subset.IsSome then 
        spec.Subset.Value |> Array.contains (fst value)
      else 
        true
    static member inline Default () =
      { Subset = None }
      
        
 
  and TransactionSig = 
    { Instant : DateTime
      Id : uint64
      Authority : string }

  and CreateTypeTransaction =
    { Transaction : TransactionSig }

  and CreateOfTypeTransaction = 
    { OfType : TypeRaw 
      Transaction : TransactionSig }
    
  and DepricateTypeTransaction =
    { Transaction : TransactionSig }  

  and AddMemberTypeTransaction =
    { Name : string
      OfType : TypeRaw
      Transaction : TransactionSig } 

  and RenameMemberTypeTransaction =
    { NewName : string
      OldName : string
      Transaction : TransactionSig }

  and SpecTransaction<'spec> =     
    { Spec : 'spec
      Transaction : TransactionSig }       

  and SimpleTypeEvent<'spec> = 
     | Create of CreateTypeTransaction
     | Depricate of DepricateTypeTransaction
     | Spec of SpecTransaction<'spec>

  and SimpleTypeEvent = 
     | Create of CreateTypeTransaction
     | Depricate of DepricateTypeTransaction

  and AbstractTypeEvent<'t,'spec> = 
     | Create of CreateTypeTransaction
     | Depricate of DepricateTypeTransaction
     | Spec of SpecTransaction<'spec>

  and NamedTypeEvent =
     | Create of CreateTypeTransaction * string
     | Depricate of DepricateTypeTransaction
     | Spec of SpecTransaction<'spec>

  and ConstantTypeEvent =
     | Create
     | UpdateConstant
     | Depricate

  and ReferenceTypeEvent =
     | Create
     | UpdateReference 
     | Depricate
     | Spec of SpecTransaction<ReferenceSpec>     
  // and Action<'t,'spec> =
  //   | Create of 't
  //   | AddMember of string * 't
  //   | Depricate
  //   | AddSpec of 'spec

  // and Event<'t,'spec> =
  //   { Instant : DateTime
  //     Id : uint64
  //     Event : Action<'t,'spec>
  //     Authority : string}

  and TypeRaw =
    | String of SimpleTypeEvent<StringSpec> []
    | Int of SimpleTypeEvent<RangeSpec<int>> []
    | Byte of SimpleTypeEvent<RangeSpec<uint8>> []
    | Int64 of SimpleTypeEvent<RangeSpec<int64>> []
    | UInt64 of SimpleTypeEvent<RangeSpec<uint64>> []
    | Double of SimpleTypeEvent<RangeSpec<float>> []
    | Decimal of SimpleTypeEvent<RangeSpec<decimal>> []
    | Guid of SimpleTypeEvent []
    | Bool of SimpleTypeEvent []
    | UTCDateTime of SimpleTypeEvent<DateTimeSpec> []
    | Unit
    | Binary of SimpleTypeEvent<ArraySpec<byte>> []
    | Array of AbstractTypeEvent<TypeRaw,ArraySpec<Instance>> []
    | Constant of ConstantTypeEvent []
    | Product of AbstractTypeEvent<TypeRaw,ProductSpec> []
    | Sum of AbstractTypeEvent<TypeRaw,SumSpec> []
    | Reference of ReferenceTypeEvent []
    | Named of NamedTypeEvent []

  and Type =
    | String of StringSpec
    | Int of RangeSpec<int>
    | Byte of RangeSpec<uint8>
    | Int64 of RangeSpec<int64>
    | UInt64 of RangeSpec<uint64>
    | Double of RangeSpec<float>
    | Decimal of RangeSpec<decimal>
    | Bool
    | Guid
    | UTCDateTime of DateTimeSpec
    | Unit
    | Binary of ArraySpec<byte>
    | Array of ArraySpec<Instance>
    | Constant of Type * Instance
    | Product of ProductType
    | Sum of SumType
    | Reference of Named * ReferenceSpec
    | Named of NamedType

  and ProductType =
    { Members : (string * Type) []
      Spec: ProductSpec }

  and SumType =
    { Members : (string * Type) []
      Spec: SumSpec }

  and Named =
    { Name : string
      Namespace : string }
  
  and NamedType =
    { Name : string
      Namespace : string
      Type : Type}

 // Instance
  and Instance =
    | Int of int
    | Byte of uint8
    | Int64 of int64
    | UInt64 of uint64
    | Double of float
    | Decimal of decimal
    | String of string
    | Binary of byte []
    | Guid of Guid
    | UTCDateTime of DateTime
    | Bool
    | Unit
    | SumMember of string * Instance
    | Product of (string * Instance) []
    | Array of Instance []
    | Constant of Instance
    | Reference of Reference * Instance
    | Named of Instance

  and Reference =
    {
      Id : string
      DocumentType : string
    }

  let rec validate (typ : Type) (instance : Instance) =
    match typ, instance with
    | Type.Int(spec), Int(i) -> RangeSpec.Validate spec i
    | Type.Int64(spec), Int64(i) -> RangeSpec.Validate spec i
    | Type.Byte(spec), Byte(i) -> RangeSpec.Validate spec i
    | Type.UInt64(spec), UInt64(i) -> RangeSpec.Validate spec i
    | Type.Double(spec), Double(i) -> RangeSpec.Validate spec i
    | Type.Decimal(spec), Decimal(i) -> RangeSpec.Validate spec i
    | Type.String(spec), String(i) -> StringSpec.Validate spec i
    | Type.Binary(spec), Binary(i) -> ArraySpec.Validate spec i
    | Type.Guid, Guid(i) -> true
    | Type.UTCDateTime(spec), UTCDateTime(i) -> DateTimeSpec.Validate spec i
    | Type.Unit, Unit -> true
    | Type.Sum(spec), SumMember(s,i) -> 
      let vals = spec.Members |> Array.choose (fun (name,t) -> if name = s then Some(t) else None)
      if Array.isEmpty vals then false else
        validate vals.[0] i && SumSpec.Validate spec.Spec (s,i)
    | Type.Product(spec), Product(i) -> 
      if ProductSpec.Validate spec.Spec i then
        let m = i |> Map.ofArray
        spec.Members |> Array.forall (fun (name,t) ->
          if m.ContainsKey(name) then
            validate t m.[name]
          else false
        )
      else false
    | Type.Array(spec), Array(i) -> ArraySpec.Validate spec i
    | Type.Constant(_,i), Constant(ins) -> i = ins
    | Type.Reference(n,spec), Reference(r,i) -> 
      ReferenceSpec.Validate spec r
      //&& validate n.Type i
    | Type.Named(n), _ -> validate n.Type instance
    | _,_ -> false

  let inline getSpec (stream : Event<'t,^spec> [] when ^spec : (static member Default : unit -> ^spec)) =
      let headSpec =
        stream
        |> Array.filter (fun x -> match x.Event with AddSpec(spec) -> true | _ -> false)
        |> Array.sortBy (fun x -> x.Instant)
        
      if headSpec.Length > 0 then
        match headSpec.[0].Event with
        | AddSpec(spec) -> spec
        | _ -> failwith "Array filter error"
      else
        (^spec : (static member Default : unit -> ^spec) ())
        
  let inline getCreate (stream : Event<'t,'spec> []) =
    Array.pick (fun x -> match x.Event with | Create(t) -> Some(t)) stream
  
  let rec fromRaw (typ : TypeRaw) =
    match typ with
    | TypeRaw.String evts -> Type.String (getSpec evts)
    | TypeRaw.Int evts -> Type.Int (getSpec evts )
    | TypeRaw.Int64 evts -> Type.Int64 (getSpec evts)
    | TypeRaw.UInt64 evts -> Type.UInt64 (getSpec evts)
    | TypeRaw.Decimal evts -> Type.Decimal (getSpec evts)
    | TypeRaw.Double evts -> Type.Double (getSpec evts)
    | TypeRaw.Unit -> Type.Unit
    | TypeRaw.Bool evts -> Type.Bool
    | TypeRaw.Byte evts -> Type.Byte (getSpec evts)
    | TypeRaw.Binary evts -> Type.Binary (getSpec evts)
    | TypeRaw.Guid evts -> Type.Guid
    | TypeRaw.UTCDateTime evts -> Type.UTCDateTime (getSpec evts)
    | TypeRaw.Sum evts ->
        let spec = getSpec evts
        let members =
            evts
            |> Array.sortBy (fun x -> x.Instant)
            |> Array.fold (fun acc x ->
              match x.Event with
              | AddMember(name,t) -> (name, (fromRaw t)) :: acc
              | _ -> acc) List.empty
        Type.Sum({Spec = spec; Members = members |> List.toArray})     
    | TypeRaw.Product evts ->
        let spec = getSpec evts
        let members =
            evts
            |> Array.sortBy (fun x -> x.Instant)
            |> Array.fold (fun acc x ->
              match x.Event with
              | AddMember(name,t) -> (name, (fromRaw t)) :: acc
              | _ -> acc) List.empty
        Type.Product({Spec = spec; Members = members |> List.toArray})
        
    | TypeRaw.Array evts -> Type.Array (getSpec evts)
    | TypeRaw.Constant evts ->
        let typ,ins = getCreate evts  //failwith uninitialized
        Type.Constant ((fromRaw typ),ins)
    | TypeRaw.Reference evts ->
        let named = getCreate evts
        let spec = getSpec evts
        Type.Reference (named,spec)
    | TypeRaw.Named evts ->
        let named,typ = getCreate evts
        Type.Named {Namespace = named.Namespace; Name = named.Name; Type = (fromRaw typ)}
    
    
    
    
  

