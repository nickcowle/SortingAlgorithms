namespace NippySort

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

    let inline compare (stats : NippySortStats) (xs : 'a array) (op : 'a -> 'a -> bool) (i : int) (j : int) =
        stats.Comparison i j
        op xs.[i] xs.[j]


    let inline swap (stats : NippySortStats) (xs : 'a array) (i : int) (j : int) =
        stats.Swap i j
        let x = xs.[i]
        xs.[i] <- xs.[j]
        xs.[j] <- x


    let inline reverse (stats : NippySortStats) (xs : 'a array) (i : int) (j : int) =
        stats.Reverse i j
        let mutable i = i
        let mutable j = j - 1
        while i < j do
            let v = xs.[i]
            xs.[i] <- xs.[j]
            xs.[j] <- v
            i <- i + 1
            j <- j - 1


    let rec sortUp (stats : NippySortStats) (xs : 'a array) (lower : int) (upper : int) (pivot : int) (smaller : int) =
        stats.SortUp lower upper pivot smaller
        let mutable smaller = smaller
        while smaller < pivot && (compare stats xs (<=) smaller pivot) do smaller <- smaller + 1
        if smaller = pivot then
            // We've made it all the way up to the pivot - sort either side
            sortSection stats xs lower pivot
            sortSection stats xs (pivot + 1) upper
        else
            // We've hit an element to the left of the pivot that is larger than the pivot.
            // Swap the pivot and the element, then call sortDown.
            swap stats xs smaller pivot
            sortDown stats xs lower upper smaller pivot


    and sortDown (stats : NippySortStats) (xs : 'a array) (lower : int) (upper : int) (pivot : int) (greater : int) =
        stats.SortDown lower upper pivot greater
        let mutable greater = greater
        while greater > pivot + 1 && (compare stats xs (>=) (greater - 1) pivot) do greater <- greater - 1
        if greater = pivot + 1 then
            // We've made it all the way down to the pivot - sort either side
            sortSection stats xs lower pivot
            sortSection stats xs (pivot + 1) upper
        else
            // We've hit an element to the right of the pivot that is smaller than the pivot.
            // Swap the pivot and the element, then call sortUp.
            swap stats xs pivot (greater - 1)
            sortUp stats xs lower upper (greater - 1) (pivot + 1)


    and sortSection (stats : NippySortStats) (xs : 'a array) (lower : int) (upper : int) =

        // do a pass to see if we're in order or reversed
        let mutable ascends = false
        let mutable descends = false
        let mutable i = lower
        while i < upper - 1 do
            if xs.[i + 1] > xs.[i] then
                ascends <- true
            else if xs.[i + 1] < xs.[i] then
                descends <- true
            i <- i + 1

        if ascends && descends then
            sortDown stats xs lower upper lower upper
        else if descends then
            // The list is sorted descending - reverse it
            reverse stats xs lower upper


    let sortWithStats (stats : NippySortStats) (xs : 'a array) : unit =
        sortSection stats xs 0 xs.Length


    let sort (xs : 'a array) =

        let dummyStats =
            {
                Comparison = fun _ _     -> ()
                Swap       = fun _ _     -> ()
                SortUp     = fun _ _ _ _ -> ()
                SortDown   = fun _ _ _ _ -> ()
                Reverse    = fun _ _     -> ()
            }

        sortSection dummyStats xs 0 xs.Length
