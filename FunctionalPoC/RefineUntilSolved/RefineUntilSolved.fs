namespace FunctionalPoC.RefineUntilSolved

module RefineUntilSolved = 

    let rec refineUntilSolved  isTrivial solve refine problem= 
     match (isTrivial problem) with 
        | true -> solve problem
        | _ -> refineUntilSolved isTrivial solve refine (refine problem) 



