[<AutoOpen>]
module Common

[<AutoOpen>]
module Strings =
    
    type IString = 
        abstract Value : string

    let create canonicalize isValid ctor (s:string) =
        if s = null then None
        else 
            let s' = canonicalize s
            if isValid s' then Some (ctor s')
            else None

    let apply f (s:IString) = s.Value |> f

    let value s = apply id s

    let equals left right = (value left) = (value right)

    let compareTo left right = (value left).CompareTo (value right)

    let singleLineTrimmed s = System.Text.RegularExpressions.Regex.Replace(s, "\s", "").Trim()

    let lengthValidator len (s:string) = s.Length <= len


    type String100 = String100 of string with
        interface IString with
            member this.Value = let (String100 s) = this in s

    let string100 = create singleLineTrimmed (lengthValidator 100) String100   
    
    let convertTo100 s = apply string100 s


    type NonEmptyString = NonEmptyString of string with
        interface IString with
            member this.Value = let (NonEmptyString s) = this in s

    let nonEmptyString = create id (fun s -> s <> null && s.Length > 0) NonEmptyString

    let convertToNonEmpty s = apply nonEmptyString s




type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure



// TODO: support all control flow constructs such as try/catch, do/while, etc.
type ActionBuilder() =
        
    member x.Bind(expr, func) =
        match expr with
        | Success r -> func r
        | Failure e -> e |> Failure

    member x.Return(value) = value |> Success

    member x.ReturnFrom(value) = value

    member x.Zero() = Success ()

    member x.Run(expr) = expr

    member x.Delay(func) = 
        match func() with
        | Success r -> r |> Success
        | Failure e -> e |> Failure

let action = new ActionBuilder()



let inline flip f a b = f b a


[<AutoOpen>]
module Validator =

    let validator predicate error x =
        if predicate x then Success x
        else Failure error

    let (==) = LanguagePrimitives.PhysicalEquality
    let inline (!=) a b = not (a == b)
    let notNull e = validator ((!=) null) e
    let notEmptyString e = validator (fun (s:string) -> s.Length > 0) e

    let puree = Success

    let apply f x =
        match f,x with
        | Success f, Success x   -> Success (f x)
        | Failure e, Success x   -> Failure e
        | Success f, Failure e   -> Failure e
        | Failure e1, Failure e2 -> Failure (e1 @ e2)

    let (<*>) = apply

    let map f x =
        match x with
        | Success x -> f x |> puree
        | Failure x -> x |> Failure

    let inline (<!>) f x = map f x

    let inline (<|>) x f = map f x

    let inline (<**>) x y = 
        match x with
        | Success x -> (x,y) |> Success
        | Failure e -> e |> Failure

    let inline lift2 f a b = f <!> a <*> b

    let inline ( *>) a b = lift2 (fun _ z -> z) a b

    let inline ( <*) a b = lift2 (fun z _ -> z) a b

    let inline (<?>) a b = lift2 (fun _ z -> z) a (Success b)

    let inline (|?>) a b = lift2 (fun z _ -> z) (Success a) b





module Aggregate = 

    type Aggregate<'TState, 'TCommand, 'TEvent> = {   
        zero  : 'TState;
        apply : 'TState -> 'TEvent -> 'TState;
        exec  : 'TState -> 'TCommand -> Result<'TEvent list, string list>;
    }

    type Type = System.Type

    type Id = System.Guid * int

    let makeHandler (aggregate:Aggregate<'TState, 'TCommand, 'TEvent>) (load:Type * Id -> obj seq, commit:Id -> obj seq -> unit) =
        fun id command ->
            let events = load (typeof<'TEvent>,id) |> Seq.cast :> 'TEvent seq
            let state = events |> Seq.fold aggregate.apply aggregate.zero 
            let uncommitted = command |> aggregate.exec state 
            match uncommitted with
            | Success events -> events |> Seq.cast |> commit id |> Success
            | Failure errors -> errors |> Failure