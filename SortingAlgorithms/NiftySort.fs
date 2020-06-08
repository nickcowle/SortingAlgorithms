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

    let inline swap (stats : NiftySortStats) (xs : 'a array) (i : int) (j : int) =
        stats.Swap i j
        let x = xs.[i]
        xs.[i] <- xs.[j]
        xs.[j] <- x


    let rec sortInner
        (stats : NiftySortStats) (xs : 'a array)
        (lower : int) (upper : int) (innerLeft : int) (innerRight : int)
        (maxLower : 'a) (minUpper : 'a)
        (leftSorted : bool) (rightSorted : bool) =

        let sortInner = sortInner stats xs lower upper
        let swap = swap stats xs

        let left  = xs.[innerLeft]
        let right = xs.[innerRight - 1]

        let leftIsSmall  = left  < maxLower
        let leftIsBig    = left  > minUpper
        let rightIsSmall = right < maxLower
        let rightIsBig   = right > minUpper

        match innerRight - innerLeft with
        | 0 ->
            if not leftSorted then sortSection stats xs lower innerLeft
            if not rightSorted then sortSection stats xs innerRight upper
        | 1 ->
            if leftIsBig then
                sortInner innerLeft (innerRight - 1) minUpper maxLower leftSorted false
            else
                let maxLower, leftSorted = if leftIsSmall then maxLower, false else left, leftSorted
                sortInner (innerLeft + 1) innerRight left maxLower leftSorted rightSorted
        | _ ->
            if leftIsSmall && rightIsSmall then
                swap (innerLeft + 1) (innerRight - 1)
                sortInner (innerLeft + 2) innerRight maxLower minUpper false rightSorted
            elif leftIsBig && rightIsBig then
                swap innerLeft (innerRight - 2)
                sortInner innerLeft (innerRight - 2) maxLower minUpper leftSorted false
            elif leftIsBig || rightIsSmall || right < left then
                swap innerLeft (innerRight - 1)
                sortInner innerLeft innerRight maxLower minUpper leftSorted rightSorted
            else
                let maxLower, leftSorted = if leftIsSmall then maxLower, false else left, leftSorted
                let minUpper, rightSorted = if rightIsBig then minUpper, false else right, rightSorted
                sortInner (innerLeft + 1) (innerRight - 1) maxLower minUpper leftSorted rightSorted


    and sortSection (stats : NiftySortStats) (xs : 'a array) (lower : int) (upper : int) =
        stats.SortSection lower upper
        match upper - lower with
        | 0
        | 1 -> ()
        | _ ->
            if xs.[lower] > xs.[upper - 1] then swap stats xs lower (upper - 1)
            sortInner stats xs lower upper (lower + 1) (upper - 1) (xs.[lower]) (xs.[upper - 1]) true true


    let sortWithStats (stats : NiftySortStats) (xs : 'a array) : unit =
        sortSection stats xs 0 xs.Length


    let sort (xs : 'a array) =

        let dummyStats =
            {
                SortSection = fun _ _ -> ()
                Swap        = fun _ _ -> ()
            }

        sortWithStats dummyStats xs
