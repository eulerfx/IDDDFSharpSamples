[<AutoOpen>]
module Common

open System

type DateTime = System.DateTime

type Guid = System.Guid

type TenantId = TenantId of string

type EmailAddress = EmailAddress of string

type Telephone = Telephone of string

type PostalAddress = {
    street : string;
    city : string;
    state : string;
    zip : string;
    countryCode : string;
};

type FullName = {
    firstName : string;
    lastName : string;
};


type Duration = Duration of DateTime * DateTime with 

    member x.IsAvailableOn(dt) =        
        if Duration.OpenEnded = x then true
        else 
            let now = dt
            let (Duration(startingOn,until)) = x            
            if now >= startingOn && now <= until then true
            else false

    member x.IsAvailable = x.IsAvailableOn(DateTime.Now)

    member x.StartingOn(startingOn) = 
        let (Duration(_,until)) = x
        Duration(startingOn, until) 

    member x.Until(until) = 
        let (Duration(startingOn,_)) = x
        Duration(startingOn, until)

    static member OpenEnded = Duration(DateTime.MinValue,DateTime.MinValue)

    static member create (startinOn,until) = Duration(startinOn,until)





type ActionBuilder() =
        
    member x.Bind(expr, func) =
        match expr with
        | Choice1Of2 r -> func r
        | Choice2Of2 e -> e |> Choice2Of2

    member x.Return(value) = value |> Choice1Of2

    member x.ReturnFrom(value) = value

    member x.Zero() = Choice1Of2 ()

    member x.Run(expr) = expr

    member x.Delay(func) = 
        match func() with
        | Choice1Of2 r -> r |> Choice1Of2
        | Choice2Of2 e -> e |> Choice2Of2

let action = new ActionBuilder()




[<AutoOpen>]
module Validator =

    let validator predicate error x =
        if predicate x then Choice1Of2 x
        else Choice2Of2 error

    let (==) = LanguagePrimitives.PhysicalEquality
    let inline (!=) a b = not (a == b)
    let notNull e = validator ((!=) null) e
    let notEmptyString e = validator (fun (s:string) -> s.Length > 0) e

    let puree = Choice1Of2

    let apply f x =
        match f,x with
        | Choice1Of2 f, Choice1Of2 x   -> Choice1Of2 (f x)
        | Choice2Of2 e, Choice1Of2 x   -> Choice2Of2 e
        | Choice1Of2 f, Choice2Of2 e   -> Choice2Of2 e
        | Choice2Of2 e1, Choice2Of2 e2 -> Choice2Of2 (e1 @ e2)

    let (<*>) = apply

    let map f o =
        match o with
        | Choice1Of2 x -> f x |> puree
        | Choice2Of2 x -> Choice2Of2 x

    let inline (<!>) f x = map f x

    let inline lift2 f a b = f <!> a <*> b

    let inline ( *>) a b = lift2 (fun _ z -> z) a b

    let inline ( <*) a b = lift2 (fun z _ -> z) a b

    let inline (<?>) a b = lift2 (fun _ z -> z) a              (Choice1Of2 b)

    let inline (|?>) a b = lift2 (fun z _ -> z) (Choice1Of2 a) b