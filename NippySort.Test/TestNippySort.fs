namespace NippySort.Test

open FsCheck
open NippySort
open Xunit

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

    [<Fact>]
    let ``NippySort does not swap any elements of a sorted array`` () =

        let prop (xs : int list) =

            let swapCount = ref 0

            let stats =
                {
                    Comparison = fun _ _ -> ()
                    Swap = fun _ _ -> incr swapCount
                    SortUp = fun _ _ _ _ -> ()
                    SortDown = fun _ _ _ _ -> ()
                }

            let sorted = xs |> List.sort |> Array.ofList
            NippySort.sortWithStats stats sorted
            swapCount.Value = 0

        check prop
