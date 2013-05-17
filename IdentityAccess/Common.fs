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

type Duration = 
    | Duration of DateTime * DateTime
    | OpenEnded

    member x.IsAvailableOn(dt) =
        match x with
        | OpenEnded -> true
        | Duration (startingOn,until) when dt >= startingOn && dt <= until -> true
        | _ -> false

    member x.IsAvailable = x.IsAvailableOn(DateTime.Now)




type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure



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

    let map f o =
        match o with
        | Success x -> f x |> puree
        | Failure x -> Failure x

    let inline (<!>) f x = map f x

    let inline lift2 f a b = f <!> a <*> b

    let inline ( *>) a b = lift2 (fun _ z -> z) a b

    let inline ( <*) a b = lift2 (fun z _ -> z) a b

    let inline (<?>) a b = lift2 (fun _ z -> z) a (Success b)

    let inline (|?>) a b = lift2 (fun z _ -> z) (Success a) b