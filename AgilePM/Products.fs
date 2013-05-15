module Products

type ProductId = ProductId of string

type ProductOwnerId = ProductOwnerId of string

type TenantId = TenantId of string

type BacklogItemId = BacklogItemId of string


type Product = {
    description : string;
    discussion : ProductDiscussion;
    discussionInitiationId : string;
    name : string;
    productId : ProductId;
    productOwnerId : ProductOwnerId;
    tenantId : TenantId;

} 

and ProductDiscussion = {
    availability : DiscussionAvailability;
    descriptor : DiscussionDescriptor;
}

and DiscussionAvailability = AddOnNotEnabled | Failed | NotRequested | Requested | Ready

and DiscussionDescriptor = Undefined | Id of string

and ProductBacklogItem = {
    backlogItemId : BacklogItemId;
    ordering : int;
    productId : ProductId;
    tenantId : TenantId;
}


// TODO: make all error cases explicit.
let toReadyDiscussion discussion descriptor = 
    match descriptor,discussion.availability with
    | Id _,Requested -> { availability = Ready; descriptor = descriptor; } |> Choice1Of2 
    | _ -> ["Invalid state."] |> Choice2Of2


let discussionFromAvailability a =
    match a with
    | Ready -> ["Cannot be created ready."] |> Choice2Of2
    | _ -> { availability = a; descriptor = Undefined; } |> Choice1Of2  
