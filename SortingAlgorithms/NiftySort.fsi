namespace SortingAlgorithms

[<NoEquality>]
[<NoComparison>]
type NiftySortStats =
    {
        SortSection : int -> int -> unit
        Swap        : int -> int -> unit
    }


[<RequireQualifiedAccess>]
module NiftySort =

    val sortWithStats<'a when 'a : comparison> : NiftySortStats -> 'a array -> unit

    val sort<'a when 'a : comparison> : 'a array -> unit
