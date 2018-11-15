module TowerOfHanoi

let rec TowerOfHanoi from middle destiny number =
    if number > 0 then
        TowerOfHanoi from destiny middle (number - 1)
        printfn "Move disc %d from %c to %c" (number - 1) from destiny
        TowerOfHanoi middle from destiny (number - 1)

let rec hanoi number origin destination extra =
    if number = 0 then [] else
        (hanoi (number - 1) origin extra destination) @ [(origin, destination)] @ (hanoi (number - 1) extra destination origin)

let rec TowerOfHanoiRec number start final =
    match number with
    | 0 -> []
    | _ -> 
        let trail = (6 - start - final)
        let list = (TowerOfHanoiRec (number - 1) start trail) @ [start, final] @ (TowerOfHanoiRec(number - 1) trail final)
        //printfn "%O" list
        list

(TowerOfHanoiRec 3 1 3) |> List.iter (fun (x, y) -> printfn "Move the disc from %A to %A" x y);;