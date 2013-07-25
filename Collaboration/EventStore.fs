/// Integration with EventStore.
[<RequireQualifiedAccess>]
module EventStoreModule

[<AutoOpen>]
module EventSourcing =

    type EventStreamId = {
        name    : string;
        version : int; }

    type EventStream = {
        events  : obj seq;
        version : int; }

    type DispatchableDomainEvent = {
        eventId    : string;
        version    : int;
        occurredOn : System.DateTime;
        event      : obj; }

    


open System
open System.Net
open EventStore.ClientAPI

/// Creates and opens an EventStore connection.
let conn endPoint =   
    let conn = EventStoreConnection.Create() 
    conn.Connect(endPoint)
    conn


let private allEventsForward (conn:EventStoreConnection) (lastReceivedEvent:int64) = seq {    
    let batchSize = 1000
    let isEnd = ref false
    let position = ref (Position(lastReceivedEvent,lastReceivedEvent))
    while !isEnd do
        let slice = conn.ReadAllEventsForward(!position,batchSize,false)
        yield! slice.Events
        isEnd := slice.IsEndOfStream
        position := slice.NextPosition
}    

let private allStreamEventsForward (conn:EventStoreConnection) stream resolveLinks = seq {    
    let batchSize = 1000
    let isEnd = ref false
    let start = ref 1
    while !isEnd do
        let slice = conn.ReadStreamEventsForward(stream,!start,batchSize,resolveLinks)
        yield! slice.Events
        isEnd := slice.IsEndOfStream
        start := slice.NextEventNumber
}


/// Creates event store based repository.
let makeRepository (conn:EventStoreConnection) (category:string) (serialize:obj -> string * byte array, deserialize: Type * string * byte array -> obj) =

    let streamId (id:Guid) = category + "-" + id.ToString("N").ToLower()

    let load (t,id) = 
        allStreamEventsForward conn (streamId id) false
        |> Seq.map (fun e -> deserialize(t, e.Event.EventType, e.Event.Data))

    let commit (id,expectedVersion) e =
        let streamId = streamId id
        let eventType,data = serialize e
        let metaData = [||] : byte array
        let eventData = new EventData(Guid.NewGuid(), eventType, true, data, metaData)
        if expectedVersion = 0 
            then conn.CreateStream(streamId, Guid.NewGuid(), true, metaData)
        conn.AppendToStream(streamId, expectedVersion, eventData)

    load,commit



let eventsSince (conn:EventStoreConnection) (lastReceivedEvent:int64) =
    allEventsForward conn lastReceivedEvent
    |> Seq.map (fun e -> 
        { eventId = e.Event.EventId.ToString(); 
          version = e.Event.EventNumber; 
          occurredOn = DateTime.UtcNow; // TODO: extract from metadata
          event = e.Event.Data })

    