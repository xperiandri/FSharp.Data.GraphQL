[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.Linq)>]
[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.ComparerMapping.Tests

open System
open System.Collections
open Xunit
open FSharp.Data.GraphQL.Server.Middleware

[<Fact>]
let ``comparerToStringComparison maps well-known StringComparer instances`` () =
    let testCases =
        [
            (StringComparer.OrdinalIgnoreCase :> IComparer, StringComparison.OrdinalIgnoreCase)
            (StringComparer.InvariantCultureIgnoreCase :> IComparer, StringComparison.InvariantCultureIgnoreCase)
            (StringComparer.CurrentCultureIgnoreCase :> IComparer, StringComparison.CurrentCultureIgnoreCase)
            (StringComparer.Ordinal :> IComparer, StringComparison.Ordinal)
            (StringComparer.InvariantCulture :> IComparer, StringComparison.InvariantCulture)
            (StringComparer.CurrentCulture :> IComparer, StringComparison.CurrentCulture)
        ]

    for comparer, expected in testCases do
        let actual = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
        actual |> equals expected

[<Fact>]
let ``comparerToStringComparison returns ValueNone for unsupported comparers`` () =
    let customComparer =
        { new IComparer with
            member _.Compare (_, _) = 0
        }

    ObjectListFilter.comparerToStringComparison null |> wantValueNone
    ObjectListFilter.comparerToStringComparison customComparer |> wantValueNone
