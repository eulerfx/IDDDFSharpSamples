module Calendars

type CalendarId = CalendarId of string

type CalendarEntryId = CalendarEntryId of string


module Calendar = 

    open Collaborators

    type Id = Tenant * CalendarId

    type State = {
        id : Id;
        name : NonEmptyString;
        description : NonEmptyString;
        sharedWith : CalendarSharer Set; } 

    and CalendarSharer = { participant : Collaborator; }

    type Command =
        | Create of Id * NonEmptyString * NonEmptyString * Collaborator * CalendarSharer Set
        | ChangeDescription of NonEmptyString
        | Rename of NonEmptyString
        | ShareCalendarWith of CalendarSharer
        | UnshareCalendarWith of CalendarSharer

    type Event = 
        | Created of Id * NonEmptyString * NonEmptyString * Collaborator * CalendarSharer Set
        | DescriptionChanged of Id * NonEmptyString
        | Renamed of Id * NonEmptyString
        | CalendarShared of Id * NonEmptyString * CalendarSharer
        | CalendarUnshared of Id * NonEmptyString * CalendarSharer 


    let apply state = function
        | Created (id,name,desc,owner,sharedWith) -> { id = id; name = name; description = desc; sharedWith = sharedWith }
        | DescriptionChanged (_,desc)             -> { state with description = desc }
        | Renamed (_,name)                        -> { state with name = name }
        | CalendarShared (_,name,sharedWith)      -> { state with sharedWith = state.sharedWith |> Set.add sharedWith }
        | CalendarUnshared (_,name,unsharedWith)  -> { state with sharedWith = state.sharedWith |> Set.remove unsharedWith }


    let exec state = function
        | Create (id,name,desc,owner,sharedWith) -> [Created(id,name,desc,owner,sharedWith)] |> Success
        | ChangeDescription desc                 -> [DescriptionChanged (state.id,desc)] |> Success
        | Rename name                            -> [Renamed (state.id,name)] |> Success
        | ShareCalendarWith sharer               -> [CalendarShared (state.id,state.name,sharer)] |> Success
        | UnshareCalendarWith sharer             -> [CalendarUnshared (state.id,state.name,sharer)] |> Success


    

module CalendarEntry =

    open System
    open Collaborators

    type Id = Tenant * CalendarId * CalendarEntryId

    type State = {
        id : Id;
        description : NonEmptyString;
        location : NonEmptyString;
        owner : Collaborator;
        timeSpan : DateRange;
        repetition : Repetition;
        alarm : Alarm;
        invitees : Collaborator Set; }    

    and Alarm = {
        alarmUnits : int;
        alarmUnitsType : AlarmUnitsType; } 
    
    and AlarmUnitsType = Days | Hours | Minutes
    
    and DateRange = { begins : DateTime; ends : DateTime } with 
        static member create (begins,ends) = { begins = begins; ends = ends }

    and Repetition = Repetition of RepeatType * DateTime option with 
        static member doesNotRepeat ends = Repetition(DoesNotRepeat,Some ends)
        static member repeatsIndefinitely repeatType = Repetition(repeatType,None)

    and RepeatType = DoesNotRepeat | Daily | Weekly | Monthly | Yearly


    type Command = 
        | Schedule of Id * NonEmptyString * NonEmptyString * Collaborator * DateRange * Repetition * Alarm * Collaborator Set
        | ChangeDescription of NonEmptyString
        | Invite of Collaborator
        | Uninvite of Collaborator
        | Relocate of NonEmptyString
        | Reschedule of NonEmptyString * NonEmptyString * DateRange * Repetition * Alarm

    type Event = 
        | Scheduled of Id * NonEmptyString * NonEmptyString * Collaborator * DateRange * Repetition * Alarm * Collaborator Set
        | DescriptionChanged of Id * NonEmptyString
        | ParticipantInvited of Id * Collaborator
        | ParticipantUninvited of Id * Collaborator
        | Relocated of Id * NonEmptyString
        | Rescheduled of Id * DateRange * Repetition * Alarm


    let apply state = function
        | Scheduled(id,desc,location,owner,timeSpan,repetition,alarm,invitees) 
            -> { id = id; description = desc; location = location; owner = owner; timeSpan = timeSpan; repetition = repetition; alarm = alarm; invitees = invitees }
        | DescriptionChanged(_,desc)          -> { state with description = desc }
        | ParticipantInvited(_,participant)   -> { state with invitees = state.invitees |> Set.add participant }
        | ParticipantUninvited(_,participant) -> { state with invitees = state.invitees |> Set.remove participant }
        | Relocated(_,location)               -> { state with location = location }
        | Rescheduled(_,timeSpan,repetition,alarm) 
            -> { state with timeSpan = timeSpan; repetition = repetition; alarm = alarm }


    let exec (state:State) = function
        | Schedule(id,desc,location,owner,timeSpan,repetition,alarm,invitees) -> 
            let repetition = match repetition with Repetition(DoesNotRepeat,_) -> Repetition.doesNotRepeat (timeSpan.ends) | _ -> repetition
            [Scheduled(id,desc,location,owner,timeSpan,repetition,alarm,invitees)] |> Success        
        | Reschedule (desc,location,timeSpan,repetition,alarm) -> 
            [Rescheduled(state.id,timeSpan,repetition,alarm)]  |> Success
        | ChangeDescription desc -> [DescriptionChanged (state.id,desc)] |> Success        
        | Invite(participant)    -> [ParticipantInvited(state.id,participant)] |> Success        
        | Uninvite(participant)  -> [ParticipantUninvited(state.id,participant)] |> Success        
        | Relocate location      -> [Relocated(state.id,location)] |> Success          