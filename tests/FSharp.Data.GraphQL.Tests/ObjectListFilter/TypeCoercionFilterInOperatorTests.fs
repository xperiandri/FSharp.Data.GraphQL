[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
[<Xunit.Trait (Tests.TraitType.ObjectListFilterOperator, "in")>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.TypeCoercion.InOperatorTests

open Xunit
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Tests.ObjectListFilter.TypeCoercion.Common

// ──────────────────────────────────────────────────────────────────────────────
// In operator test cases
// Covers all coercion types: CLR enum, DU-as-enum, Guid, single-case DU, and primitives
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``In operator coerces string primitives`` () =
    let filter = In { FieldName = "name"; Value = [ box "Alice"; box "Bob" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``In operator coerces int primitives`` () =
    let filter = In { FieldName = "id"; Value = [ box 1; box 3 ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Id) |> List.sort |> equals [ 1; 3 ]

[<Fact>]
let ``In operator coerces CLR enum`` () =
    let filter = In { FieldName = "color"; Value = [ box "Red"; box "Blue" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces DU-as-enum`` () =
    let filter = In { FieldName = "status"; Value = [ box "Active"; box "Pending" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces Guid`` () =
    let filter =
        In {
            FieldName = "guidField"
            Value = [
                box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                box "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
            ]
        }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``In operator coerces single-case DU wrapping string`` () =
    let filter = In { FieldName = "wrappedName"; Value = [ box "Alice"; box "Charlie" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces single-case DU wrapping int`` () =
    let filter = In { FieldName = "wrappedScore"; Value = [ box 10; box 30 ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``In operator coerces single-case DU wrapping Guid`` () =
    let filter =
        In {
            FieldName = "wrappedGuid"
            Value = [ box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"; box "cccccccc-cccc-cccc-cccc-cccccccccccc" ]
        }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]
