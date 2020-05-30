namespace NippySort.Test

open FsCheck
open NippySort
open Xunit

type NiftySortEvent =
| SortSection of int * int
| Swap        of int * int

[<RequireQualifiedAccess>]
module TestNiftySort =

    let config = { Config.QuickThrowOnFailure with MaxTest = 100_000 }

    let check prop = Check.One(config, prop)

    let trace (xs : int list) : NiftySortEvent list =

        let events = ResizeArray ()

        let stats =
            {
                SortSection = fun i j -> SortSection (i, j) |> events.Add
                Swap        = fun i j -> Swap        (i, j) |> events.Add
            }

        xs |> Array.ofList |> NiftySort.sortWithStats stats
        events |> List.ofSeq

    let isSwapEvent = function Swap (_,_) -> true | _ -> false

    [<Fact>]
    let ``NiftySort sorts the elements of the array in place correctly`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let input = xs |> Array.ofList
            NiftySort.sort input
            input |> Array.toList = sorted

        check prop

    [<Fact>]
    let ``NiftySort sorts sorted arrays correctly`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let input = sorted |> Array.ofList
            NiftySort.sort input
            input |> Array.toList = sorted

        check prop

    [<Fact>]
    let ``NiftySort doesn't swap any elements of a sorted array`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let events = trace sorted
            events |> List.filter isSwapEvent |> List.isEmpty

        check prop

    [<Fact>]
    let ``NiftySort sorts reversed arrays correctly`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let reversed = sorted |> List.rev
            let input = reversed |> Array.ofList
            NiftySort.sort input
            input |> Array.toList = sorted

        check prop

    [<Fact>]
    let ``NiftySort performs at most n/2 swaps on a reversed array`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let reversed = sorted |> List.rev
            let events = trace reversed
            events |> List.filter isSwapEvent |> List.length <= (List.length xs / 2)

        check prop

    [<Fact>]
    let ``NiftySort sorts constant arrays correctly`` () =

        let prop (x : int) (NonNegativeInt count) =
            let xs = List.replicate count x
            let input = xs |> Array.ofList
            NiftySort.sort input
            input |> Array.toList = xs

        check prop

    [<Fact>]
    let ``NiftySort doesn't swap any elements of a constant array`` () =

        let prop (x : int) (NonNegativeInt count) =
            let xs = List.replicate count x
            let events = trace xs
            events |> List.filter isSwapEvent |> List.isEmpty

        check prop
