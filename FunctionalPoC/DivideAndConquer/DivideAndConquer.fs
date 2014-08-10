namespace FunctionalPoC.DivideAndConquer

module DivideAndConquer =
    let rec divideAndConquer isTrivial solve divide combine problem =
        match (isTrivial problem) with
            | true -> solve problem
            | _    -> combine ( (divide problem) |> List.map (fun x -> divideAndConquer isTrivial solve divide combine x)  )

       
                    
    

