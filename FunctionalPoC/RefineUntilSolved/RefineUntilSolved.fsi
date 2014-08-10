namespace FunctionalPoC.RefineUntilSolved

module RefineUntilSolved  =
                            
    val refineUntilSolved : ('tProblem->bool) ->  //isTrivial
                            ('tProblem->'tSolution) ->  //solve
                            ('tProblem ->'tProblem) -> //refine
                            'tProblem -> 
                            'tSolution