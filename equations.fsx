#load "utility.fsx"

open Utility
open System

let eps = 1e-10

let dichotomy f a b = 
    FOR
        (a, b, (a+b)/2.0)
        (fun (_, _, c) -> abs (f c) > eps)
        (fun (a, b, c) ->
            if f a * f c < 0.0 then (a, c, (a+c)/2.0)
            else (c, b, (c+b)/2.0))
        (fun (_, _, c) -> fun _ -> c)
        0.0


let iterations phi x0 = 
    FOR
        (x0, phi x0)
        (fun (x, x1) -> abs (x1 - x) > eps)
        (fun (_, x1) -> (x1, phi x1))
        (fun (x, _) -> fun _ -> x)
        0.0


let newthon f f' x0 = 
    let phi x = x - (f x) / (f' x) in
    iterations phi x0

// // Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = Math.Exp(x) + Math.Log(x) - 10.0 * x
let f2 x = Math.Cos(x) - Math.Exp(-Math.Pow(x, 2)/2.0) + x - 1.0
let f3 x = 1.0 - x + Math.Sin(x) - Math.Log(1.0+x)

let f1' x = Math.Exp(x) + (1.0/x) - 10.0
let f2' x = -Math.Sin(x) + (x*Math.Exp(-Math.Pow(x, 2)/2.0)) + 1.0
let f3' x = Math.Cos(x) - (1.0 / (x+1.0)) - 1.0

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 3. 4.) (iterations phi1 3.5) (newthon f1 f1' 3.5)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 1. 2.) (iterations phi2 1.5) (newthon f2 f2' 1.5)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 1. 1.5) (iterations phi3 1.25) (newthon f3 f3' 1.25)

