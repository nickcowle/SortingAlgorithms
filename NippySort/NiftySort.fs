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
        // TOD eliminate cases where we i = j
        let x = xs.[i]
        xs.[i] <- xs.[j]
        xs.[j] <- x


    let rec sortInner (stats : NiftySortStats) (xs : 'a array) (lower : int) (upper : int) (innerLeft : int) (innerRight : int) (maxLower : 'a) (minUpper : 'a) =

        match innerRight - innerLeft with
        | 0 ->
            // We've successfully partitioned the values - now recurse and sort these
            // TODO: Keep track of whether these parts are sorted or not!
            sortSection stats xs lower innerLeft
            sortSection stats xs innerRight upper
        | 1 ->
            let x = xs.[innerLeft]
            if x >= minUpper then
                sortInner stats xs lower upper innerLeft (innerRight - 1) maxLower minUpper
            else
                sortInner stats xs lower upper (innerLeft + 1) innerRight (max maxLower x) minUpper
        | _ ->
            let testLeft = max maxLower xs.[innerLeft]
            let testRight = min minUpper xs.[innerRight - 1]

            if testLeft <= testRight then
                sortInner stats xs lower upper (innerLeft + 1) (innerRight - 1) testLeft testRight
            else
                let testLeft = max maxLower xs.[innerRight - 1]
                let testRight = min minUpper xs.[innerLeft]
                if testLeft <= testRight then
                    swap stats xs innerLeft (innerRight - 1)
                    sortInner stats xs lower upper (innerLeft + 1) (innerRight - 1) testLeft testRight
                elif xs.[innerLeft] <= maxLower && xs.[innerRight - 1] <= maxLower then
                    swap stats xs (innerLeft + 1) (innerRight - 1)
                    sortInner stats xs lower upper (innerLeft + 2) innerRight maxLower minUpper
                elif xs.[innerLeft] >= minUpper && xs.[innerRight - 1] >= minUpper then
                    swap stats xs innerLeft (innerRight - 2)
                    sortInner stats xs lower upper innerLeft (innerRight - 2) maxLower minUpper
                else
                    failwith "Unreachable"


    and sortSection (stats : NiftySortStats) (xs : 'a array) (lower : int) (upper : int) =
        stats.SortSection lower upper
        match upper - lower with
        | 0
        | 1 -> ()
        | _ ->
            if xs.[lower] > xs.[upper - 1] then swap stats xs lower (upper - 1)
            sortInner stats xs lower upper (lower + 1) (upper - 1) (xs.[lower]) (xs.[upper - 1])


    let sortWithStats (stats : NiftySortStats) (xs : 'a array) : unit =
        sortSection stats xs 0 xs.Length


    let sort (xs : 'a array) =

        let dummyStats =
            {
                SortSection = fun _ _     -> ()
                Swap        = fun _ _     -> ()
            }

        sortWithStats dummyStats xs
