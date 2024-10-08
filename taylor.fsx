#load "utility.fsx"

open Utility

let f x = x / (9.0 + pown x 2)
let term x n = (pown -1.0 n) * (pown x (2*n+1)) / (pown 9.0 (n+1))

let naiven x n =
    FOR
        (0, x)
        (fun (i, _) -> i <= n)
        (fun (i, x) -> (i+1, x))
        (fun (i, x) -> fun res -> res + (term x i))
        0.0

let naive x eps =
    FOR
        (0, term x 0)
        (fun (_, xi) -> abs ((f x) - xi) > eps)
        (fun (i, _) -> (i+1, naiven x i))
        (fun (i, xi) -> fun _ -> (xi, i+1))
        (0.0, 0)
    
let smart x eps =
    FOR
        (0, 0.0)
        (fun (i, xi) -> abs ((f x) - xi) > eps && i < 10)
        (fun (i, xi) -> (i+1, xi + (term x i)))
        (fun (i, xi) -> fun _ -> (xi, i+1))
        (0.0, 0)

let main a b n eps =
    let step = (b - a) / float (n - 1)
    printfn "| x     | Builtin     | Smart Taylor | # terms | Dumb Taylor | # terms |"
    printfn "|-------|-------------|--------------|---------|-------------|---------|"
    FOR
        (1, a, (f a), (smart a eps), (naive a eps))
        (fun (i, _, _, _, _) -> i <= n)
        (fun (i, x, _, _, _) -> (i+1, (a+step*float i), (f x), (smart x eps), (naive x eps)))
        (fun (_, x, builtin, (smartResult, smartTerms), (dumbResult, dumbTerms)) -> fun _ -> 
            printfn "| %5.1f | %11.5f | %12.5f | %7d | %11.5f | %7d |" x builtin smartResult smartTerms dumbResult dumbTerms)
        ()


main -1.0 1.0 21 1e-5