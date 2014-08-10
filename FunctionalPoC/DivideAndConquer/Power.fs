namespace FunctionalPoC.DivideAndConquer
open FunctionalPoC.DivideAndConquer.DivideAndConquer

//n^m with logaritmic complexity
module Power =

    type TProblem = int * int
    type TSolution = int
    
    let isTrivial (m:int, n:int): bool = (n=0) || (n=1)
    
    let solve (m:int, n:int): int = match n with 
                                    | 0 -> 1
                                    | _ -> m

    let divide (m:int, n:int): TProblem list = match (n % 2) with
                                               | 0 -> [ (m,n/2) ; (m,n/2) ] 
                                               | _ -> [ (m,1) ; (m,n-1)]

    let combine (listSolutions :TSolution list) :TSolution =
        listSolutions |> List.reduce (fun acc x -> x*acc)
                                    

    let power m n  = divideAndConquer isTrivial solve divide combine (m,n) 
