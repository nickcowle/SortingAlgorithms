namespace NippySort

[<RequireQualifiedAccess>]
module NippySort =

    val sort<'a when 'a : comparison> : 'a array -> unit
