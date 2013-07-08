module Forums


type ForumId = ForumId of string

type DiscussionId = DiscussionId of string

type PostId = PostId of string

type ExclusiveOwner = string option


module Forum = 
    
    open Collaborators

    type Id = Tenant * ForumId

    type State = {
        id : Id;
        creator : Collaborator; 
        moderator : Collaborator;
        subject : NonEmptyString;
        description : NonEmptyString;
        exclusiveOwner : ExclusiveOwner;
        closed : bool; }

    type Command = 
        | Start of Id * Collaborator * Collaborator * NonEmptyString * NonEmptyString * ExclusiveOwner
        | AssignModerator of Collaborator
        | ChangeDescription of NonEmptyString
        | ChangeSubject of NonEmptyString
        | Close
        | ReOpen
        
    type Event =
        | Started of Id * Collaborator * Collaborator * NonEmptyString * NonEmptyString * ExclusiveOwner
        | ModeratorChanged of Id * Collaborator * ExclusiveOwner
        | DescriptionChanged of Id * NonEmptyString * ExclusiveOwner
        | SubjectChanged of Id * NonEmptyString * ExclusiveOwner
        | Closed of Id * ExclusiveOwner
        | ReOpened of Id * ExclusiveOwner
        

    let apply state = function
        | Started(id,creator,moderator,subject,desc,exclusiveOwner) -> 
            { id = id; creator = creator; moderator = moderator; subject = subject; description = desc; exclusiveOwner = exclusiveOwner; closed = false; }        
        | ModeratorChanged (_,moderator,_) -> { state with moderator = moderator }
        | DescriptionChanged (_,desc,_)    -> { state with description = desc }
        | SubjectChanged (_,subject,_)     -> { state with subject = subject }
        | Closed _                         -> { state with closed = true }
        | ReOpened _                       -> { state with closed = false }


    let exec state =     
        let assertOpen = validator (fun _ -> state.closed <> true) ["Forum must be open."] state
        let assertClosed = validator (fun _ -> state.closed) ["Forum must be closed."] state
        function
        | Start (id,creator,moderator,subject,desc,exclusiveOwner) ->
            Started (id,creator,moderator,subject,desc,exclusiveOwner) |> Success
        | AssignModerator moderator -> assertOpen <?> ModeratorChanged(state.id,moderator,state.exclusiveOwner)
        | ChangeDescription desc    -> assertOpen <?> DescriptionChanged(state.id,desc,state.exclusiveOwner)
        | ChangeSubject subject     -> assertOpen <?> SubjectChanged(state.id,subject,state.exclusiveOwner)
        | Close                     -> assertOpen <?> Closed(state.id,state.exclusiveOwner)
        | ReOpen                    -> assertClosed <?> ReOpened(state.id,state.exclusiveOwner)
        

module Discussion =    

    open Collaborators

    type Id = Tenant * ForumId * DiscussionId          

    type State = {    
        id : Id;
        author : Collaborator;
        subject : NonEmptyString;
        exclusiveOwner : ExclusiveOwner;
        closed : bool; }       

    type Event = 
        | Started of Id * Collaborator * NonEmptyString * ExclusiveOwner
        | Closed of Id * ExclusiveOwner
        | ReOpened of Id * ExclusiveOwner

    type Command = 
        | Start of Id * Collaborator * NonEmptyString * ExclusiveOwner
        | Close
        | ReOpen

    
    let apply state = function
        | Started (id,author,subject,exclusiveOwner) 
            -> { id = id; author = author; subject = subject; exclusiveOwner = exclusiveOwner; closed = false; }
        | Closed _   -> { state with closed = true }
        | ReOpened _ -> { state with closed = false }

    let exec state = 
        let assertOpen = validator (fun s -> s.closed |> not) ["Discussion must be open."] state
        let assertClosed = validator (fun s -> s.closed) ["Discussion must be closed."] state
        function
        | Start (id,author,subject,exclusiveOwner) 
            -> Started(id,author,subject,exclusiveOwner) |> Success
        | Close   -> assertOpen <?> Closed(state.id,state.exclusiveOwner)
        | ReOpen  -> assertClosed <?> ReOpened(state.id,state.exclusiveOwner)


module Post =
    
    open Collaborators

    type Id = Tenant * ForumId * DiscussionId * PostId

    type State = {
        id : Id;
        author : Collaborator;
        subject : NonEmptyString;
        body : NonEmptyString;
        replyToPostId : PostId option; }

    type Event = 
        | Posted of Id * Collaborator * NonEmptyString * NonEmptyString * PostId option
        | ContentAltered of Id * NonEmptyString * NonEmptyString

    type Command =
        | Post of Id * Collaborator * NonEmptyString * NonEmptyString * PostId option
        | AlterContent of NonEmptyString * NonEmptyString

    let apply state = function
        | Posted(id,author,subject,body,replyToPostId) -> { id = id; author = author; subject = subject; body = body; replyToPostId = replyToPostId; }
        | ContentAltered(id,subject,body) -> { state with subject = subject; body = body; }

    let exec state = function
        | Post(id,author,subject,body,replyToPostId) -> Posted(id,author,subject,body,replyToPostId) |> Success
        | AlterContent(subject,body) -> ContentAltered(state.id,subject,body) |> Success
    