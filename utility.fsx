let rec FOR init cond iter func args =
    match cond init with
    | true -> FOR (iter init) cond iter func (func init args)
    | false -> args

let pown x n =
    FOR
        0
        (fun i -> i < n)
        (fun i -> i + 1)
        (fun _ -> fun res -> res * x)
        1.0

let abs x =
    if x < 0.0 then -x
    else x