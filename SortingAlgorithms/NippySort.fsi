namespace SortingAlgorithms

[<NoEquality>]
[<NoComparison>]
type NippySortStats =
    {
        Comparison : int -> int -> unit
        Swap       : int -> int -> unit
        SortUp     : int -> int -> int -> int -> unit
        SortDown   : int -> int -> int -> int -> unit
        Reverse    : int -> int -> unit
    }

[<RequireQualifiedAccess>]
module NippySort =

    val sortWithStats<'a when 'a : comparison> : NippySortStats -> 'a array -> unit

    val sort<'a when 'a : comparison> : 'a array -> unit
