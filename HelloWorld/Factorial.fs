module Factorial

// iterative
let factorialIterative x =
    let mutable n = x
    let mutable returnVal = 1
    while n >= 1 do
        returnVal <- returnVal * n
        n <- (n - 1)
    returnVal

// recursive
let rec factorial n =
    if n < 1 then 1
    else n * factorial (n - 1)

// recursive with pattern matching
let rec factorial_PatternMatching n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n - 1)

// tail recursive
let tail_factorial n =
    let rec tailRecFact n accum =
        if n <= 1 then
            accum
        else
            tailRecFact (n - 1) (accum*n)
    tailRecFact n 1

// continuation recursive
let cont_recursive n =
    let rec contTailRecFact n f =
        if n <= 1 then
            f()
        else
            contTailRecFact (n - 1) (fun () -> n * f())
    contTailRecFact n (fun () -> 1)

