namespace FunctionalPoC.Backtrack

module BacktringOptimalSol = 

    let pickTheBest  (partialSolution:'tPartialSolution) 
                     (solbenefits :('tSolution * 'tBenefit) List)
                     (estimate: 'tPartialSolution -> 'tBenefit)  
                     (convert: 'tPartialSolution -> 'tSolution)
                     : ('tSolution * 'tBenefit) List  =
                     match solbenefits with 
                        | [] -> [(convert partialSolution,estimate partialSolution)] 
                        | [(solution, benefit)] -> if (estimate partialSolution) > benefit then
                                                      [(convert partialSolution, estimate partialSolution)]
                                                   else
                                                       [(solution, benefit)]
    

    let  canBeImproved  (partialSolution:'tPartialSolution) 
                        (solbenefits :('tSolution * 'tBenefit) List)
                        (estimate: 'tPartialSolution -> 'tBenefit): bool =        
                match solbenefits with     
                        | [] -> true
                        | [(solution, benefit)] -> (estimate partialSolution) > benefit

        
    let rec backtringOptimalSolAux ( solbenefits :('tSolution * 'tBenefit) List)
                                   ( estimate:  'tPartialSolution -> 'tBenefit)
                                   ( isSolution:'tPartialSolution -> bool)
                                   ( convert: 'tPartialSolution -> 'tSolution)
                                   ( expand:'tPartialSolution -> 'tPartialSolution List)
                                   ( isValid: 'tPartialSolution -> bool)
                                   ( partialSol: 'tPartialSolution) : ('tSolution * 'tBenefit) List =
          if (isSolution partialSol) then
            pickTheBest partialSol solbenefits  estimate  convert
          else
            if (canBeImproved partialSol solbenefits estimate) then
               (expand partialSol |> List.filter(isValid) )  |>  
                        List.fold  (fun acc node -> if (canBeImproved node acc estimate) then
                                                        backtringOptimalSolAux acc estimate isSolution convert expand isValid node 
                                                    else
                                                        acc)
                                    solbenefits     
            else 
               solbenefits          


    let backtringOptimalSol ( estimate: 'tPartialSolution -> 'tBenefit)
                            ( isSolution:'tPartialSolution -> bool)
                            ( convert: 'tPartialSolution -> 'tSolution)
                            ( expand:'tPartialSolution -> 'tPartialSolution List)
                            ( isValid: 'tPartialSolution -> bool)
                            ( initialState: 'tPartialSolution) : ('tSolution * 'tBenefit) List  =
        backtringOptimalSolAux [] estimate isSolution convert expand isValid initialState

    