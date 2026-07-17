[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.Linq)>]
[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.ComparerMapping.Tests

open System
open System.Collections
open System.Globalization
open Xunit
open FSharp.Data.GraphQL.Server.Middleware

// ─────────────────────────────────────────────────────────────────────────────
// Singleton reference-equality branch
// ─────────────────────────────────────────────────────────────────────────────

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

// ─────────────────────────────────────────────────────────────────────────────
// Each singleton must map to a distinct StringComparison value
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison singleton mappings are all distinct`` () =
    let singletons : IComparer list =
        [
            StringComparer.OrdinalIgnoreCase
            StringComparer.InvariantCultureIgnoreCase
            StringComparer.CurrentCultureIgnoreCase
            StringComparer.Ordinal
            StringComparer.InvariantCulture
            StringComparer.CurrentCulture
        ]

    let results =
        singletons
        |> List.map (fun c -> ObjectListFilter.comparerToStringComparison c |> wantValueSome)

    let distinct = results |> List.distinct
    List.length distinct |> equals (List.length results)

// ─────────────────────────────────────────────────────────────────────────────
// IsWellKnownCultureAwareComparer fallback path
// StringComparer.Create produces a non-singleton comparer that is still
// well-known, so it falls through the ReferenceEquals branch and hits the
// IsWellKnownCultureAwareComparer fallback.
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison maps non-singleton InvariantCulture comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.InvariantCulture, false) :> IComparer
    // must NOT be the same object as the singleton
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.InvariantCulture :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    result |> equals StringComparison.InvariantCulture

[<Fact>]
let ``comparerToStringComparison maps non-singleton InvariantCultureIgnoreCase comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.InvariantCulture, true) :> IComparer
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.InvariantCultureIgnoreCase :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    result |> equals StringComparison.InvariantCultureIgnoreCase

[<Fact>]
let ``comparerToStringComparison maps non-singleton CurrentCulture comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.CurrentCulture, false) :> IComparer
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.CurrentCulture :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    result |> equals StringComparison.CurrentCulture

[<Fact>]
let ``comparerToStringComparison maps non-singleton CurrentCultureIgnoreCase comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.CurrentCulture, true) :> IComparer
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.CurrentCultureIgnoreCase :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    result |> equals StringComparison.CurrentCultureIgnoreCase

// ─────────────────────────────────────────────────────────────────────────────
// Unknown / unsupported cases → ValueNone
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison returns ValueNone for null`` () =
    ObjectListFilter.comparerToStringComparison null |> wantValueNone

[<Fact>]
let ``comparerToStringComparison returns ValueNone for non-StringComparer IComparer`` () =
    let customComparer =
        { new IComparer with
            member _.Compare (_, _) = 0
        }
    ObjectListFilter.comparerToStringComparison customComparer |> wantValueNone

[<Fact>]
let ``comparerToStringComparison returns ValueNone for non-standard culture comparer`` () =
    // A comparer for a specific non-current, non-invariant culture — the
    // IsWellKnownCultureAwareComparer fallback cannot map it to any of the six
    // StringComparison values, so it must return ValueNone.
    let trCulture = CultureInfo.GetCultureInfo "tr-TR"
    // Only run this test when the test host is not Turkish (otherwise CurrentCulture == tr-TR
    // and the result would legitimately be CurrentCulture).
    if not (CultureInfo.CurrentCulture.Name.StartsWith "tr") then
        let comparer = StringComparer.Create (trCulture, false) :> IComparer
        ObjectListFilter.comparerToStringComparison comparer |> wantValueNone

// ─────────────────────────────────────────────────────────────────────────────
// Determinism: calling comparerToStringComparison twice on the same instance
// must return the same result
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison is deterministic for singletons`` () =
    let singletons : IComparer list =
        [
            StringComparer.OrdinalIgnoreCase
            StringComparer.InvariantCultureIgnoreCase
            StringComparer.CurrentCultureIgnoreCase
            StringComparer.Ordinal
            StringComparer.InvariantCulture
            StringComparer.CurrentCulture
        ]

    for comparer in singletons do
        let first  = ObjectListFilter.comparerToStringComparison comparer
        let second = ObjectListFilter.comparerToStringComparison comparer
        first |> equals second
