namespace NippySort.Test

open FsCheck
open NippySort
open Xunit

[<RequireQualifiedAccess>]
module TestNiftySort =

    let config = { Config.QuickThrowOnFailure with MaxTest = 100_000 }

    let check prop = Check.One(config, prop)

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
    let ``NiftySort sorts reversed arrays correctly`` () =

        let prop (xs : int list) =
            let sorted = xs |> List.sort
            let reversed = sorted |> List.rev
            let input = reversed |> Array.ofList
            NiftySort.sort input
            input |> Array.toList = sorted

        check prop

    [<Fact>]
    let ``NiftySort sorts constant arrays correctly`` () =

        let prop (x : int) (NonNegativeInt count) =
            let xs = List.replicate count x
            let input = xs |> Array.ofList
            NiftySort.sort input
            input |> Array.toList = xs

        check prop
