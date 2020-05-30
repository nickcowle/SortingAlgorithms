namespace NippySort

[<RequireQualifiedAccess>]
module NippySort =

    let rec sortUp (xs : 'a array) (lower : int) (upper : int) (pivot : int) (smaller : int) =
        let x = xs.[pivot]
        let mutable smaller = smaller
        while smaller < pivot && xs.[smaller] < x do smaller <- smaller + 1
        if smaller = pivot then
            // We've made it all the way up to the pivot - sort either side
            sortSection xs lower pivot
            sortSection xs (pivot + 1) upper
        else
            // We've hit an element to the left of the pivot that is larger than the pivot.
            // Swap the pivot and the element, then call sortDown.
            let v = xs.[smaller]
            xs.[smaller] <- x
            xs.[pivot] <- v
            sortDown xs lower upper smaller pivot

    and sortDown (xs : 'a array) (lower : int) (upper : int) (pivot : int) (greater : int) =
        let x = xs.[pivot]
        let mutable greater = greater
        while greater > pivot + 1 && xs.[greater - 1] > x do greater <- greater - 1
        if greater = pivot + 1 then
            // We've made it all the way down to the pivot - sort either side
            sortSection xs lower pivot
            sortSection xs (pivot + 1) upper
        else
            // We've hit an element to the right of the pivot that is smaller than the pivot.
            // Swap the pivot and the element, then call sortUp.
            let v = xs.[greater - 1]
            xs.[greater - 1] <- x
            xs.[pivot] <- v
            sortUp xs lower upper (greater - 1) (pivot + 1)

    and sortSection (xs : 'a array) (lower : int) (upper : int) =
        if upper > lower + 1 then
            sortDown xs lower upper lower upper

    let sort (xs : 'a array) : unit =
        sortSection xs 0 xs.Length
