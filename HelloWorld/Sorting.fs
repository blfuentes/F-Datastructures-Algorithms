module Sorting

open System.Linq.Expressions

let rec quickSort = function
    | [] -> []
    | n::ns ->  let lessthan, greaterEqual = List.partition((>) n) ns
                quickSort lessthan @ n :: quickSort greaterEqual

let rand = new System.Random()
let data = List.init 10 (fun _ -> rand.Next())
let result = quickSort data
let result2 = List.sort data

let rec quickSort_func (pxs:seq<_>) =
    seq {
        match Seq.toList pxs with
        | p::xs ->  let lessthan, greaterEqual = List.partition ((>=) p) xs
                    yield! quickSort_func lessthan; yield p; yield! quickSort_func greaterEqual
        | _ -> ()
    }