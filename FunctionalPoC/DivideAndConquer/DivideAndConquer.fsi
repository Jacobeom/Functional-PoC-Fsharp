namespace FunctionalPoC.DivideAndConquer

module DivideAndConquer  =
                            
    val divideAndConquer : ('tProblem->bool) ->  //isTrivial
                           ('tProblem->'tSolution) ->  //solve
                           ('tProblem ->'tProblem list) -> //divide
                           ('tSolution list -> 'tSolution ) -> //combine
                           'tProblem -> 
                           'tSolution
