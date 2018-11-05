﻿module TowerOfHanoi

let rec TowerOfHanoi f x t n =
    if n > 0 then
        TowerOfHanoi f t x (n - 1)
        printfn "Move disc from %c to %c" f t
        TowerOfHanoi x f t (n - 1)

let rec hanoi n o d t =
    if n = 0 then [] else
        (hanoi (n - 1) o t d) @ [(o, d)] @ (hanoi (n - 1) t d o)

let rec TowerOfHanoiRec n s f =
    match n with
    | 0 -> []
    | _ -> 
        let t = (6 - s - f)
        (TowerOfHanoiRec (n - 1) s t) @ [s, f] @ (TowerOfHanoiRec(n - 1) t f)