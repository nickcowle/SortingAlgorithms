namespace NippySort

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
        let inline getLeft () = xs.[innerLeft]
        let inline getRight () = xs.[innerRight - 1]

        match innerRight - innerLeft with
        | 0 ->
            // We've successfully partitioned the values - now recurse and sort these if necessary
            if not leftSorted then sortSection stats xs lower innerLeft
            if not rightSorted then sortSection stats xs innerRight upper
        | 1 ->
            let x = getLeft ()
            if x > minUpper then
                sortInner innerLeft (innerRight - 1) maxLower minUpper leftSorted false
            else
                let leftSorted = if x < maxLower then false else leftSorted
                sortInner (innerLeft + 1) innerRight (max maxLower x) minUpper leftSorted rightSorted
        | n ->
            let left = getLeft ()
            let right = getRight ()
            if left < maxLower then
                // We're not going to move the left point. Left is now definitely not sorted
                if right < maxLower then
                    // Right point needs to be moved left
                    if n > 2 then swap stats xs (innerLeft + 1) (innerRight - 1)
                    sortInner (innerLeft + 2) innerRight maxLower minUpper false rightSorted
                else
                    // Right can stay right
                    let minUpper, rightSorted = if right > minUpper then minUpper, false else right, rightSorted
                    sortInner (innerLeft + 1) (innerRight - 1) maxLower minUpper false rightSorted
            elif right > minUpper then
                // We're not going to move the right point. Right is now definitely not sorted.
                if left > minUpper then
                    // Left point needs to be moved right
                    if n > 2 then swap stats xs innerLeft (innerRight - 2)
                    sortInner innerLeft (innerRight - 2) maxLower minUpper leftSorted false
                else
                    // Left can stay left
                    let maxLower, leftSorted = if left < maxLower then maxLower, false else left, leftSorted
                    sortInner (innerLeft + 1) (innerRight - 1) maxLower minUpper leftSorted false
            else
                // left >= maxLower, right <= minUpper, but we don't know that left <= right
                if max maxLower left > min minUpper right then
                    swap stats xs innerLeft (innerRight - 1)

                let left = getLeft ()
                let right = getRight ()
                let maxLower, leftSorted  = if left  < maxLower then maxLower, false else left,  leftSorted
                let minUpper, rightSorted = if right > minUpper then minUpper, false else right, rightSorted
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
