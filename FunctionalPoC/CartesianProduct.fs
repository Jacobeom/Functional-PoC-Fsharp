namespace FunctionalPoC


module CartesianProduct =
    let cartesian listA listB = let allPairs elt list  = list |> List.map( fun x -> (elt, x) )
                                listA |> List.map( fun eltA -> allPairs eltA listB ) |> List.reduce (fun xs ys -> xs @ ys)


