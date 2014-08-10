namespace FunctionalPoC

module MergeSort =

let rec split = function
    | [] -> ([], [])
    | [a] -> ([a], []) 
    | h1::h2::tail ->  let (atail,btail)  = split tail 
                       (h1::atail, h2::btail)

let rec merge = function
    | (f, [], ys) -> ys
    | (f, xs, []) -> xs
    | (f, x::xs, y::ys) -> if f x y then x::merge( f, xs, y::ys)
                           else y::merge (f, x::xs, ys)


let rec mergesort = function
    | (f, []) -> []
    | (f, [x])-> [x]
    | (f, list) -> let (a, b) = split list
                   merge (f, mergesort(f, a), mergesort(f, b)) 
        


