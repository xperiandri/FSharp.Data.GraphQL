module FSharp.Data.GraphQL.Tests.ReviewSharedSchemaTests

open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Types

type RItem = { Name : string }
type RRoot = { Items : RItem list }

let private itemType = Define.Object<RItem> ("RItem", [ Define.Field ("name", StringType, fun _ (i : RItem) -> i.Name) ])

let private rootType =
    Define.Object<RRoot> ("Query", [ Define.Field ("items", ListOf itemType, fun _ (r : RRoot) -> r.Items) ])

[<Fact>]
let ``Review: second executor with a compile middleware on a shared schema`` () =
    let schema = Schema (rootType)
    let _plain = Executor (schema)
    let filtered = Executor (schema, [ Define.ObjectListFilterMiddleware<RRoot, RItem> () ])
    let result = filtered.CreateExecutionPlan (parse """{ items(filter: { name_starts_with: "a" }) { name } }""")
    match result with
    | Ok _ -> ()
    | Error struct (_, errors) -> failwith $"Rejected: %A{errors}"

[<Fact>]
let ``Review: single executor with a compile middleware`` () =
    let schema = Schema (rootType)
    let filtered = Executor (schema, [ Define.ObjectListFilterMiddleware<RRoot, RItem> () ])
    let result = filtered.CreateExecutionPlan (parse """{ items(filter: { name_starts_with: "a" }) { name } }""")
    match result with
    | Ok _ -> ()
    | Error struct (_, errors) -> failwith $"Rejected: %A{errors}"
