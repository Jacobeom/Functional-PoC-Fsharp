namespace FunctionalPoC.DivideAndConquer
open FunctionalPoC.DivideAndConquer.DivideAndConquer
// QuickSort algo (http://en.wikipedia.org/wiki/Quicksort)
//           using first element list as pivot   
module QuickSort = 
    
    type TElement<'a> = 'a
    type TProblem<'a> = TElement<'a> List
    type TSolution<'a> = TElement<'a> List


    let isTrivial (problem:TProblem<'a>): bool = 
        match problem with
            | [] -> true
            | a::[] -> true
            | a::b::[] -> true
            | _ -> false
   
    let solve (problem:TProblem<'a>): TSolution<'a> =
        match problem with
            | [] -> []
            | a::[] -> [a]
            | a::b::[] -> if (a<b) then [a;b] else[b;a]
         // | _ = (impossible case)
    
    let split (list:TProblem<'a>) (pivot:TElement<'a>) =
            (
             list |> List.filter(fun x->x<=pivot), 
             list |> List.filter(fun x->x>pivot)
            )

    let divide (first::second::tail :TProblem<'a>): TProblem<'a> List =
        let (lowersThan, biggersThan) = split (second::tail) first
        [lowersThan]@[[first]]@[biggersThan]
    //let divide (_ :TProblem): TProblem List = (impossible case)
    
    let combine (solutions:TSolution<'a> List) :TSolution<'a> =
        solutions |> List.fold (fun acc x -> acc@x) []

    let quickSort (list:TProblem<'a>) = divideAndConquer isTrivial solve divide combine list