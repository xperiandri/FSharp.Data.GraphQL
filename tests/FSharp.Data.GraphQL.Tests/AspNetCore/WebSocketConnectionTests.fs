module FSharp.Data.GraphQL.Tests.AspNetCore.WebSocketConnectionTests

open System
open System.Net.WebSockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging.Abstractions
open Microsoft.Extensions.Options
open Xunit

open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Shared.WebSockets

// Drives GraphQLWebSocketConnection over a fake socket: the protocol handshake, close codes, queries, request
// errors, subscriptions, client-side completion, incremental delivery, the message size limit and the bounded
// inbox, without a hosted server.

let private timeout = TimeSpan.FromSeconds 10.0

/// <summary>
/// A socket whose client side is scripted by the test: text frames and a close frame are queued for the server to
/// receive, every frame the server sends is recorded, and a server-initiated close is answered by the client the way
/// a managed socket would, by completing the pending receive with a close frame.
/// </summary>
type private FakeWebSocket () =
    inherit WebSocket ()

    let incoming = Channel.CreateUnbounded<byte[] voption> ()
    let sent = Channel.CreateUnbounded<string> ()
    let mutable state = WebSocketState.Open
    let mutable serverCloseStatus = ValueNone
    let mutable remainder = ReadOnlyMemory<byte>.Empty
    // Written by the server's reader, read by the test
    let mutable deliveredBytes = 0L
    let mutable deliveredMessages = 0

    member _.EnqueueText (json : string) = incoming.Writer.TryWrite (ValueSome (Encoding.UTF8.GetBytes json)) |> ignore
    member _.EnqueueClose () = incoming.Writer.TryWrite ValueNone |> ignore
    member _.Sent = sent.Reader
    member _.ServerCloseStatus : WebSocketCloseStatus voption = serverCloseStatus
    /// How many bytes of text frames the server has received so far
    member _.DeliveredBytes = Interlocked.Read &deliveredBytes
    /// How many whole text messages the server has received so far
    member _.DeliveredMessages = Volatile.Read &deliveredMessages

    override _.CloseStatus = serverCloseStatus |> ValueOption.toNullable
    override _.CloseStatusDescription = null
    override _.State = state
    override _.SubProtocol = GraphQLTransportWS.SubProtocol

    override _.Abort () =
        state <- WebSocketState.Aborted
        incoming.Writer.TryComplete () |> ignore

    override _.CloseAsync (status, _, _) =
        serverCloseStatus <- ValueSome status
        match state with
        | WebSocketState.CloseReceived -> state <- WebSocketState.Closed
        | _ ->
            state <- WebSocketState.Closed
            // The client answers the close handshake
            incoming.Writer.TryWrite ValueNone |> ignore
        Task.CompletedTask

    override _.CloseOutputAsync (status, _, _) =
        serverCloseStatus <- ValueSome status
        state <- WebSocketState.CloseSent
        Task.CompletedTask

    override _.Dispose () = ()

    override _.ReceiveAsync (buffer : ArraySegment<byte>, _ : CancellationToken) : Task<WebSocketReceiveResult> = task {
        let deliver (bytes : ReadOnlyMemory<byte>) =
            let count = min bytes.Length buffer.Count
            bytes.Slice(0, count).CopyTo (Memory<byte> (buffer.Array, buffer.Offset, count))
            remainder <- bytes.Slice count
            Interlocked.Add (&deliveredBytes, int64 count) |> ignore
            if remainder.IsEmpty then
                Interlocked.Increment &deliveredMessages |> ignore
            WebSocketReceiveResult (count, WebSocketMessageType.Text, remainder.IsEmpty)

        if not remainder.IsEmpty then
            return deliver remainder
        else
            // Throws once the socket was aborted, as a real receive does
            match! incoming.Reader.ReadAsync () with
            | ValueNone ->
                if state = WebSocketState.Open then
                    state <- WebSocketState.CloseReceived
                return WebSocketReceiveResult (0, WebSocketMessageType.Close, true, Nullable WebSocketCloseStatus.NormalClosure, "closed")
            | ValueSome bytes -> return deliver (ReadOnlyMemory bytes)
    }

    override _.SendAsync (buffer : ArraySegment<byte>, _ : WebSocketMessageType, _ : bool, _ : CancellationToken) : Task =
        sent.Writer.TryWrite (Encoding.UTF8.GetString (buffer.Array, buffer.Offset, buffer.Count)) |> ignore
        Task.CompletedTask

type private Session = {
    Socket : FakeWebSocket
    Options : GraphQLOptions<Root>
    Run : Task
    Scope : IDisposable
}

let private startConnection (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (TestSchema.executor, (fun _ -> { RequestId = "test" })) |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider
    let httpContext = DefaultHttpContext (RequestServices = serviceProvider)
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- httpContext
    let options = serviceProvider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value |> configure
    let socket = new FakeWebSocket ()
    let connection = GraphQLWebSocketConnection<Root> (httpContext, socket, options, serviceProvider, NullLogger.Instance, CancellationToken.None)
    { Socket = socket; Options = options; Run = connection.RunAsync (); Scope = scope }

let private start () = startConnection id

let private receive (session : Session) : Task<JsonDocument> = task {
    use cancellation = new CancellationTokenSource (timeout)
    let! json = session.Socket.Sent.ReadAsync cancellation.Token
    return JsonDocument.Parse json
}

let private typeOf (message : JsonDocument) = message.RootElement.GetProperty("type").GetString ()
let private idOf (message : JsonDocument) = message.RootElement.GetProperty("id").GetString ()
let private payloadOf (message : JsonDocument) = message.RootElement.GetProperty "payload"

let private hasProperty (name : string) (element : JsonElement) =
    let mutable ignored = Unchecked.defaultof<JsonElement>
    element.TryGetProperty (name, &ignored)

let private subscribe (session : Session) (id : string) (query : string) =
    session.Socket.EnqueueText (JsonSerializer.Serialize {| id = id; ``type`` = "subscribe"; payload = {| query = query |} |})

let private initialize (session : Session) : Task = task {
    session.Socket.EnqueueText """{"type":"connection_init"}"""
    use! ack = receive session
    typeOf ack |> equals "connection_ack"
}

let private closeFromClient (session : Session) : Task = task {
    session.Socket.EnqueueClose ()
    do! waitForTask timeout "The connection did not end after the client closed" session.Run
    do! session.Run
}

/// Receives the messages of one subscription up to and including its complete or error message
let private receiveUntilTerminal (session : Session) (id : string) : Task<JsonDocument list> = task {
    let received = ResizeArray<JsonDocument> ()
    let mutable terminal = false
    while not terminal do
        let! message = receive session
        idOf message |> equals id
        received.Add message
        match typeOf message with
        | "complete"
        | "error" -> terminal <- true
        | _ -> ()
    return List.ofSeq received
}

/// A message of the given type and of exactly the given size in bytes, padded by its payload
let private paddedMessage (messageType : string) (size : int) =
    let envelope = $$$"""{"type":"{{{messageType}}}","payload":{"pad":""}}"""
    // Pads between the quotes of the empty "pad" string
    envelope.Insert (envelope.Length - 3, String ('x', size - envelope.Length))

/// Polls the condition until it holds, failing with the message once the timeout has passed
let private waitUntil (message : unit -> string) (condition : unit -> bool) : Task = task {
    let started = Diagnostics.Stopwatch.StartNew ()
    while not (condition ()) do
        if started.Elapsed > timeout then
            fail (message ())
        do! Task.Delay 10
}

/// A ping handler that answers no ping until the returned gate is opened, keeping the control loop busy
let private gatedPingHandler () =
    // Opening the gate must not run the control loop on the test's thread
    let gate = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
    let handler : PingHandler = fun _ payload -> task {
        do! gate.Task
        return payload
    }
    struct (gate, handler)

[<Fact>]
let ``Connection is closed with 4408 when connection_init does not arrive in time`` () : Task = task {
    let session =
        startConnection (fun options -> {
            options with
                WebsocketOptions = { options.WebsocketOptions with ConnectionInitTimeout = TimeSpan.FromMilliseconds 100.0 }
        })
    use _ = session.Scope
    do! waitForTask timeout "The connection did not end after the initialization timeout" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4408))
}

[<Fact>]
let ``Subscribe before connection_init closes the connection with 4401`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    do! waitForTask timeout "The connection did not end after the unauthorized subscribe" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4401))
}

[<Fact>]
let ``A second connection_init closes the connection with 4429`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    session.Socket.EnqueueText """{"type":"connection_init"}"""
    do! waitForTask timeout "The connection did not end after the second connection_init" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4429))
}

[<Fact>]
let ``Client close ends the connection with a normal closure`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

[<Fact>]
let ``Ping is answered with pong`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    session.Socket.EnqueueText """{"type":"ping"}"""
    use! pong = receive session
    typeOf pong |> equals "pong"
    do! closeFromClient session
}

[<Fact>]
let ``A query is answered with next and complete`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "complete" ]
    (payloadOf messages.Head).GetProperty("data").GetProperty("hero").GetProperty("name").GetString ()
    |> equals "Luke Skywalker"
    do! closeFromClient session
}

[<Fact>]
let ``A request error is sent as an error message`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { nope } }"""
    let! messages = receiveUntilTerminal session "1"
    let error = List.exactlyOne messages
    typeOf error |> equals "error"
    let payload = payloadOf error
    payload.ValueKind |> equals JsonValueKind.Array
    Assert.Contains ("nope", payload[0].GetProperty("message").GetString ())
    do! closeFromClient session
}

[<Fact>]
let ``A duplicate subscription id closes the connection with 4409`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """subscription { watchMoon(id: "1") { id isMoon } }"""
    subscribe session "1" """subscription { watchMoon(id: "2") { id isMoon } }"""
    do! waitForTask timeout "The connection did not end after the duplicate subscription id" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4409))
}

[<Fact>]
let ``Client complete cancels the subscription and frees its id`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """subscription { watchMoon(id: "1") { id isMoon } }"""
    session.Socket.EnqueueText """{"id":"1","type":"complete"}"""
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "complete" ]
    do! closeFromClient session
}

[<Fact>]
let ``A query with defer delivers the incremental payloads then complete`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name homePlanet @defer } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "next"; "next"; "next"; "complete" ]
    let payloads = messages |> List.filter (fun message -> typeOf message = "next") |> List.map payloadOf
    let initial = List.head payloads
    Assert.True (initial.GetProperty("hasNext").GetBoolean (), "The initial payload must have hasNext true")
    initial.GetProperty("data").GetProperty("hero").GetProperty("homePlanet").ValueKind |> equals JsonValueKind.Null
    let final = List.last payloads
    Assert.False (final.GetProperty("hasNext").GetBoolean (), "The final payload must have hasNext false")
    Assert.False (hasProperty "data" final, "The final payload must not carry data")
    let delivered = payloads |> List.find (hasProperty "incremental")
    Assert.Contains ("Tatooine", (delivered.GetProperty("incremental")[0]).GetProperty("data").GetRawText ())
    do! closeFromClient session
}

[<Fact>]
let ``Closing the connection cancels a running subscription`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """subscription { watchMoon(id: "1") { id isMoon } }"""
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

/// Options with a size limit small enough to fall inside a message read in several frames
let private withSizeLimit (limit : int) (options : GraphQLOptions<Root>) = {
    options with
        ReadBufferSize = 256
        WebsocketOptions = { options.WebsocketOptions with MaxReceiveMessageSize = limit }
}

let private withPingHandler (handler : PingHandler) (options : GraphQLOptions<Root>) = {
    options with
        WebsocketOptions = { options.WebsocketOptions with CustomPingHandler = ValueSome handler }
}

let private smallSizeLimit = 1024

[<Theory>]
[<InlineData 1>]
[<InlineData 0>]
let ``A message up to the maximum size is processed`` (bytesBelowLimit : int) : Task = task {
    let session = startConnection (withSizeLimit smallSizeLimit)
    use _ = session.Scope
    do! initialize session
    let ping = paddedMessage "ping" (smallSizeLimit - bytesBelowLimit)
    Encoding.UTF8.GetByteCount ping |> equals (smallSizeLimit - bytesBelowLimit)
    session.Socket.EnqueueText ping
    use! pong = receive session
    typeOf pong |> equals "pong"
    use sentPing = JsonDocument.Parse ping
    (payloadOf pong).GetProperty("pad").GetString ()
    |> equals ((payloadOf sentPing).GetProperty("pad").GetString ())
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

[<Fact>]
let ``A message over the maximum size closes the connection with 1009 without being read in full`` () : Task = task {
    let session = startConnection (withSizeLimit smallSizeLimit)
    use _ = session.Scope
    do! initialize session
    let deliveredBefore = session.Socket.DeliveredBytes
    let size = 64 * 1024
    session.Socket.EnqueueText (paddedMessage "ping" size)
    do! waitForTask timeout "The connection did not end after a message over the size limit" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.MessageTooBig)
    let read = session.Socket.DeliveredBytes - deliveredBefore
    Assert.True (
        read <= int64 smallSizeLimit + 1L,
        $"The server read %d{read} of the %d{size} bytes of a message over the limit of %d{smallSizeLimit} bytes instead of stopping one byte past it"
    )
    match session.Socket.Sent.TryRead () with
    | true, answer -> fail $"The message over the size limit was answered with %s{answer}"
    | false, _ -> ()
}

[<Fact>]
let ``A message over the default maximum size of 4 MiB closes the connection with 1009 without being read in full`` () : Task = task {
    let limit = 4 * 1024 * 1024
    let session = start ()
    use _ = session.Scope
    session.Options.WebsocketOptions.MaxReceiveMessageSize |> equals limit
    do! initialize session
    let deliveredBefore = session.Socket.DeliveredBytes
    session.Socket.EnqueueText (paddedMessage "ping" (limit + 4096))
    do! waitForTask timeout "The connection did not end after a message over the size limit" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.MessageTooBig)
    let read = session.Socket.DeliveredBytes - deliveredBefore
    Assert.True (read <= int64 limit + 1L, $"The server read %d{read} bytes of a message over the limit of %d{limit} bytes instead of stopping one byte past it")
}

[<Fact>]
let ``A connection_init over the maximum size closes the connection with 1009`` () : Task = task {
    let session = startConnection (withSizeLimit smallSizeLimit)
    use _ = session.Scope
    session.Socket.EnqueueText (paddedMessage "connection_init" (smallSizeLimit + 1))
    do! waitForTask timeout "The connection did not end after a connection_init over the size limit" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.MessageTooBig)
}

[<Theory>]
[<InlineData 0>]
[<InlineData(-1)>]
let ``A maximum message size that is not positive is rejected`` (maxMessageSize : int) =
    use socket = new FakeWebSocket ()
    let ex =
        Assert.Throws<ArgumentOutOfRangeException> (fun () ->
            WebSocketMessageReader (socket, serializerOptions, GraphQLOptionsDefaults.ReadBufferSize, maxMessageSize, NullLogger.Instance)
            |> ignore
        )
    ex.ParamName |> equals "maxMessageSize"

[<Fact>]
let ``The reader stops reading from the socket while the inbox is full`` () : Task = task {
    let capacity = WebSocketConnectionLimits.IncomingMessageQueueCapacity
    let struct (gate, handler) = gatedPingHandler ()
    let session = startConnection (withPingHandler handler)
    use _ = session.Scope
    do! initialize session
    let sent = capacity * 4
    for _ in 1..sent do
        session.Socket.EnqueueText """{"type":"ping"}"""
    // connection_init, the ping the control loop is busy with, the pings filling the inbox, and the one the reader waits to queue
    let readerWaiting = 1 + 1 + capacity + 1
    do!
        (fun () -> session.Socket.DeliveredMessages >= readerWaiting)
        |> waitUntil (fun () -> $"The server read only %d{session.Socket.DeliveredMessages} messages, fewer than the %d{readerWaiting} it can hold")
    // Whatever more the reader would read, it reads in this time: every message the client sent is already in the socket
    do! Task.Delay 200
    let read = session.Socket.DeliveredMessages
    Assert.True (
        (read = readerWaiting),
        $"The server read %d{read} of the %d{sent + 1} messages the client sent while its control loop was busy, instead of stopping at %d{readerWaiting}"
    )
    // Once the control loop catches up, every message is read and answered: none was dropped
    gate.SetResult ()
    for _ in 1..sent do
        use! pong = receive session
        typeOf pong |> equals "pong"
    do! closeFromClient session
}

[<Fact>]
let ``The connection ends when its control loop stops while the reader waits for room in the inbox`` () : Task = task {
    let capacity = WebSocketConnectionLimits.IncomingMessageQueueCapacity
    let struct (gate, handler) = gatedPingHandler ()
    let session = startConnection (withPingHandler handler)
    use _ = session.Scope
    do! initialize session
    session.Socket.EnqueueText """{"type":"ping"}"""
    // Ends the control loop with 4429 once it gets to it, while the reader waits for room behind it
    session.Socket.EnqueueText """{"type":"connection_init"}"""
    for _ in 1 .. capacity * 2 do
        session.Socket.EnqueueText """{"type":"ping"}"""
    let readerWaiting = 1 + 1 + capacity + 1
    do!
        (fun () -> session.Socket.DeliveredMessages >= readerWaiting)
        |> waitUntil (fun () -> $"The server read only %d{session.Socket.DeliveredMessages} messages, fewer than the %d{readerWaiting} it can hold")
    gate.SetResult ()
    do! waitForTask timeout "The connection did not end after its control loop stopped while the reader was waiting for room in the inbox" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4429))
}

// ---- Review experiments (not part of the PR) ----

type private RealSession = {
    Server : WebSocket
    Client : WebSocket
    RealRun : Task
    RealScope : IDisposable
    Tcp : IDisposable list
}

let private startReal (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) : Task<RealSession> = task {
    let listener = new Net.Sockets.TcpListener (Net.IPAddress.Loopback, 0)
    listener.Start ()
    let port = (listener.LocalEndpoint :?> Net.IPEndPoint).Port
    let client = new Net.Sockets.TcpClient ()
    let acceptTask = listener.AcceptTcpClientAsync ()
    do! client.ConnectAsync (Net.IPAddress.Loopback, port)
    let! server = acceptTask
    listener.Stop ()
    let serverSocket =
        WebSocket.CreateFromStream (server.GetStream (), WebSocketCreationOptions (IsServer = true, SubProtocol = GraphQLTransportWS.SubProtocol, KeepAliveInterval = TimeSpan.Zero))
    let clientSocket =
        WebSocket.CreateFromStream (client.GetStream (), WebSocketCreationOptions (IsServer = false, SubProtocol = GraphQLTransportWS.SubProtocol, KeepAliveInterval = TimeSpan.Zero))
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (TestSchema.executor, (fun _ -> { RequestId = "test" })) |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider
    let httpContext = DefaultHttpContext (RequestServices = serviceProvider)
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- httpContext
    let options = serviceProvider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value |> configure
    let connection = GraphQLWebSocketConnection<Root> (httpContext, serverSocket, options, serviceProvider, NullLogger.Instance, CancellationToken.None)
    return { Server = serverSocket; Client = clientSocket; RealRun = connection.RunAsync (); RealScope = scope; Tcp = [ client :> IDisposable; server :> IDisposable ] }
}

let private clientSend (s : RealSession) (text : string) : Task =
    s.Client.SendAsync (ArraySegment (Encoding.UTF8.GetBytes text), WebSocketMessageType.Text, true, CancellationToken.None)

let private clientReceive (s : RealSession) : Task<struct (WebSocketReceiveResult * string)> = task {
    let buffer = Array.zeroCreate<byte> 65536
    use cts = new CancellationTokenSource (timeout)
    let! r = s.Client.ReceiveAsync (ArraySegment buffer, cts.Token)
    return struct (r, Encoding.UTF8.GetString (buffer, 0, r.Count))
}

let private realInit (s : RealSession) : Task = task {
    do! clientSend s """{"type":"connection_init"}"""
    let! struct (_, text) = clientReceive s
    Assert.Contains ("connection_ack", text)
}

[<Theory>]
[<InlineData(65536)>]
[<InlineData(67108864)>]
let ``REVIEW real socket over limit closes with 1009 gracefully`` (size : int) : Task = task {
    let! s = startReal (withSizeLimit smallSizeLimit)
    use _ = s.RealScope
    do! realInit s
    let sw = Diagnostics.Stopwatch.StartNew ()
    let rcv = clientReceive s
    let sendTask = clientSend s (paddedMessage "ping" size)
    let! struct (r, text) = rcv
    Assert.True ((r.MessageType = WebSocketMessageType.Close), $"Expected close, got %A{r.MessageType} %s{text}")
    r.CloseStatus |> equals (Nullable WebSocketCloseStatus.MessageTooBig)
    try do! sendTask with ex -> printfn "send failed: %s" ex.Message
    try do! s.Client.CloseOutputAsync (WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) with ex -> printfn "close output failed: %s" ex.Message
    do! waitForTask timeout "server did not end" s.RealRun
    do! s.RealRun
    let elapsed = sw.Elapsed
    Assert.True (elapsed < TimeSpan.FromSeconds 4.0, $"took %A{elapsed}; server state %A{s.Server.State}")
    s.Server.State |> equals WebSocketState.Closed
    for d in s.Tcp do d.Dispose ()
}

[<Fact>]
let ``REVIEW real socket invalid json closes with 4400 gracefully`` () : Task = task {
    let! s = startReal id
    use _ = s.RealScope
    do! realInit s
    let sw = Diagnostics.Stopwatch.StartNew ()
    let rcv = clientReceive s
    do! clientSend s "{ not json"
    let! struct (r, text) = rcv
    Assert.True ((r.MessageType = WebSocketMessageType.Close), $"Expected close, got %A{r.MessageType} %s{text}")
    r.CloseStatus |> equals (Nullable (enum<WebSocketCloseStatus> 4400))
    do! s.Client.CloseOutputAsync (WebSocketCloseStatus.NormalClosure, "", CancellationToken.None)
    do! waitForTask timeout "server did not end" s.RealRun
    do! s.RealRun
    Assert.True (sw.Elapsed < TimeSpan.FromSeconds 4.0, $"took %A{sw.Elapsed}; server state %A{s.Server.State}")
    s.Server.State |> equals WebSocketState.Closed
    for d in s.Tcp do d.Dispose ()
}

[<Fact>]
let ``REVIEW real socket connection_init over limit closes with 1009`` () : Task = task {
    let! s = startReal (withSizeLimit smallSizeLimit)
    use _ = s.RealScope
    let rcv = clientReceive s
    let sendTask = clientSend s (paddedMessage "connection_init" (1024 * 1024))
    let! struct (r, text) = rcv
    Assert.True ((r.MessageType = WebSocketMessageType.Close), $"Expected close, got %A{r.MessageType} %s{text}")
    r.CloseStatus |> equals (Nullable WebSocketCloseStatus.MessageTooBig)
    try do! sendTask with ex -> printfn "send failed: %s" ex.Message
    try do! s.Client.CloseOutputAsync (WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) with ex -> printfn "close output failed: %s" ex.Message
    do! waitForTask timeout "server did not end" s.RealRun
    do! s.RealRun
    for d in s.Tcp do d.Dispose ()
}

[<Fact>]
let ``REVIEW real socket backpressure then client close`` () : Task = task {
    let struct (gate, handler) = gatedPingHandler ()
    let! s = startReal (withPingHandler handler)
    use _ = s.RealScope
    do! realInit s
    for _ in 1..40 do
        do! clientSend s """{"type":"ping"}"""
    do! Task.Delay 200
    gate.SetResult ()
    let mutable pongs = 0
    while pongs < 40 do
        let! struct (r, text) = clientReceive s
        Assert.Contains ("pong", text)
        pongs <- pongs + 1
    do! s.Client.CloseOutputAsync (WebSocketCloseStatus.NormalClosure, "", CancellationToken.None)
    do! waitForTask timeout "server did not end" s.RealRun
    do! s.RealRun
    for d in s.Tcp do d.Dispose ()
}

type private CountingStream (inner : IO.Stream) =
    inherit IO.Stream ()
    let mutable read = 0L
    member _.BytesRead = Interlocked.Read &read
    override _.CanRead = inner.CanRead
    override _.CanWrite = inner.CanWrite
    override _.CanSeek = false
    override _.Length = raise (NotSupportedException ())
    override _.Position with get () = raise (NotSupportedException ()) and set _ = raise (NotSupportedException ())
    override _.Flush () = inner.Flush ()
    override _.FlushAsync (ct : CancellationToken) = inner.FlushAsync ct
    override _.Seek (_, _) = raise (NotSupportedException ())
    override _.SetLength _ = raise (NotSupportedException ())
    override _.Read (buffer : byte[], offset : int, count : int) =
        let n = inner.Read (buffer, offset, count)
        Interlocked.Add (&read, int64 n) |> ignore
        n
    override _.ReadAsync (buffer : byte[], offset : int, count : int, ct : CancellationToken) : Task<int> = task {
        let! n = inner.ReadAsync (buffer, offset, count, ct)
        Interlocked.Add (&read, int64 n) |> ignore
        return n
    }
    override _.ReadAsync (buffer : Memory<byte>, ct : CancellationToken) : ValueTask<int> =
        let t = task {
            let! n = inner.ReadAsync (buffer, ct)
            Interlocked.Add (&read, int64 n) |> ignore
            return n
        }
        ValueTask<int> t
    override _.Write (buffer : byte[], offset : int, count : int) = inner.Write (buffer, offset, count)
    override _.WriteAsync (buffer : byte[], offset : int, count : int, ct : CancellationToken) = inner.WriteAsync (buffer, offset, count, ct)
    override _.WriteAsync (buffer : ReadOnlyMemory<byte>, ct : CancellationToken) = inner.WriteAsync (buffer, ct)
    override _.Dispose (disposing : bool) = if disposing then inner.Dispose ()

[<Theory>]
[<InlineData(67108864)>]
[<InlineData(1073741824)>]
let ``REVIEW2 bytes the server reads of an oversized message`` (size : int) : Task = task {
    let listener = new Net.Sockets.TcpListener (Net.IPAddress.Loopback, 0)
    listener.Start ()
    let port = (listener.LocalEndpoint :?> Net.IPEndPoint).Port
    let client = new Net.Sockets.TcpClient ()
    let acceptTask = listener.AcceptTcpClientAsync ()
    do! client.ConnectAsync (Net.IPAddress.Loopback, port)
    let! server = acceptTask
    listener.Stop ()
    let counting = new CountingStream (server.GetStream ())
    let serverSocket =
        WebSocket.CreateFromStream (counting, WebSocketCreationOptions (IsServer = true, SubProtocol = GraphQLTransportWS.SubProtocol, KeepAliveInterval = TimeSpan.Zero))
    let clientSocket =
        WebSocket.CreateFromStream (client.GetStream (), WebSocketCreationOptions (IsServer = false, SubProtocol = GraphQLTransportWS.SubProtocol, KeepAliveInterval = TimeSpan.Zero))
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (TestSchema.executor, (fun _ -> { RequestId = "test" })) |> ignore
    use scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider
    let httpContext = DefaultHttpContext (RequestServices = serviceProvider)
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- httpContext
    let options = serviceProvider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value
    let connection = GraphQLWebSocketConnection<Root> (httpContext, serverSocket, options, serviceProvider, NullLogger.Instance, CancellationToken.None)
    let run = connection.RunAsync ()
    do! clientSocket.SendAsync (ArraySegment (Encoding.UTF8.GetBytes """{"type":"connection_init"}"""), WebSocketMessageType.Text, true, CancellationToken.None)
    let buffer = Array.zeroCreate<byte> 65536
    let! _ = clientSocket.ReceiveAsync (ArraySegment buffer, CancellationToken.None)
    let readBefore = counting.BytesRead
    let rcv = clientSocket.ReceiveAsync (ArraySegment buffer, CancellationToken.None)
    let payload = Encoding.UTF8.GetBytes (paddedMessage "ping" size)
    let sw = Diagnostics.Stopwatch.StartNew ()
    // Sent in 1 MiB fragments so that the client stops sending once the connection is gone
    let sendTask = task {
        try
            let mutable offset = 0
            while offset < payload.Length do
                let n = min (1024 * 1024) (payload.Length - offset)
                do! clientSocket.SendAsync (ArraySegment (payload, offset, n), WebSocketMessageType.Text, (offset + n = payload.Length), CancellationToken.None)
                offset <- offset + n
        with ex -> printfn "client send ended: %s" ex.Message
    }
    let! r = rcv
    let closeSeenAt = sw.Elapsed
    let readAtClose = counting.BytesRead - readBefore
    let! _ = Task.WhenAny (run, Task.Delay (TimeSpan.FromSeconds 15.0))
    let runEndedAt = sw.Elapsed
    let readTotal = counting.BytesRead - readBefore
    client.Dispose ()
    server.Dispose ()
    let! _ = Task.WhenAny (sendTask, Task.Delay 2000)
    fail $"REPORT size=%d{size} closeStatus=%A{r.CloseStatus} closeSeenAt=%A{closeSeenAt} readWhenCloseSeen=%d{readAtClose} runCompleted=%b{run.IsCompleted} runEndedAt=%A{runEndedAt} serverReadTotal=%d{readTotal} serverState=%A{serverSocket.State}"
}
