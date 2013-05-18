module Product

open Teams


type ProductId = ProductId of string

type BacklogItemId = BacklogItemId of string

type ReleaseId = ReleaseId of string

type SprintId = SprintId of string



type Product = {
    description : string;
    discussion : ProductDiscussion;
    discussionInitiationId : string;
    name : string;
    productId : ProductId;
    productOwnerId : ProductOwnerId;
    tenantId : TenantId;
    backlogItems : ProductBacklogItem Set;
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



type Sprint = {
    tenantId : TenantId;
    productId : ProductId;
    sprintId : SprintId;        
    duration : Duration;
    name : string;
    goals : string;
    backlogItems : CommittedBacklogItem Set;       
}

and CommittedBacklogItem = {
    tenantId : TenantId;
    sprintId : SprintId;
    backlogItemId : BacklogItemId;
    ordering : int;
}



type Release = {
    tenantId : TenantId;
    productId : ProductId;
    releaseId : ReleaseId;
    name : string;
    description : string;
    duration : Duration;
    backlogItems : ScheduledBacklogItem Set;
}

and ScheduledBacklogItem = {
    tenantId : TenantId;
    releaseId : ReleaseId;
    backlogItemId : BacklogItemId;
}



type BacklogItem = {

    tenantId : TenantId;
    productId : ProductId;
    backlogItemd : BacklogItemId;
    summary : string;
    category : string;
    itemType : BacklogItemType;
    itemStatus : BacklogItemStatus;
    storyPoints : StoryPoints;
}

and BacklogItemType = Feature | Enhancement | Defect | Foundation | Integration

and BacklogItemStatus = Planned | Scheduled | Committed | Done | Removed

and StoryPoints = Zero | One | Two | Three | Five | Eight | Thirteen | Twenty | Forty | OneHundred




type Event = 
    | Created of TenantId * ProductId * ProductOwnerId * string * string * DiscussionAvailability
    | DiscussionRequested of TenantId * ProductId * ProductOwnerId * string * string * bool
    | DiscussionInitiated of TenantId * ProductId * ProductDiscussion
    | ReleaseScheduled of TenantId * ProductId * ReleaseId * string * string * Duration
    | SprintScheduled of TenantId * ProductId * SprintId * string * string * Duration



let toReadyDiscussion discussion descriptor = 
    match descriptor,discussion.availability with
    | Id _,Requested -> { availability = Ready; descriptor = descriptor; } |> Success 
    | _ -> ["Invalid state."] |> Failure


let discussionFromAvailability a =
    match a with
    | Ready -> ["Cannot be created ready."] |> Failure
    | _ -> { availability = a; descriptor = Undefined; } |> Success  
