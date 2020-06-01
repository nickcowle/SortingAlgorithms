namespace SortingAlgorithms.Test

open FsCheck
open SortingAlgorithms
open Xunit

type NippySortEvent =
| Comparison of int * int
| Swap       of int * int
| SortUp     of int * int * int * int
| SortDown   of int * int * int * int
| Reverse    of int * int

[<RequireQualifiedAccess>]
module TestNippySort =

    let config = { Config.QuickThrowOnFailure with MaxTest = 100_000 }

    let check prop = Check.One(config, prop)

    [<Fact>]
    let ``NippySort sorts the elements of the array in place correctly`` () =

        let prop (xs : int list) =
            let expected = xs |> List.sort |> Array.ofList
            let ys = xs |> Array.ofList
            NippySort.sort ys
            ys = expected

        check prop

    let trace (xs : int list) : NippySortEvent list =

        let events = ResizeArray ()

        let stats =
            {
                Comparison = fun i j                       -> Comparison (i, j)                       |> events.Add
                Swap       = fun i j                       -> Swap (i, j)                             |> events.Add
                SortUp     = fun lower upper pivot smaller -> SortUp (lower, upper, pivot, smaller)   |> events.Add
                SortDown   = fun lower upper pivot greater -> SortDown (lower, upper, pivot, greater) |> events.Add
                Reverse    = fun i j                       -> Reverse (i, j)                          |> events.Add
            }

        xs |> Array.ofList |> NippySort.sortWithStats stats
        events |> List.ofSeq

    [<Fact>]
    let ``NippySort does not swap any elements of a sorted array`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let events = trace sorted
            let isSwapEvent = function Swap (_,_) -> true | _ -> false
            events |> List.filter isSwapEvent = []

        check prop

    [<Fact>]
    let ``NippySort just reverses a sorted but reversed list`` () =

        let prop (xs : int list) =
            let reversed = xs |> List.sort |> List.rev
            let events = trace reversed

            let expected =
                if xs |> List.distinct |> List.length >= 2 then
                    [ Reverse (0, reversed |> List.length) ]
                else
                    []

            expected = events

        check prop
