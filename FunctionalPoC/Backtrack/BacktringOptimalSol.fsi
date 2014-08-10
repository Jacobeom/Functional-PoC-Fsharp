namespace FunctionalPoC.Backtrack

module BacktringOptimalSol = 
    
    
    val backtringOptimalSol : 
        ('tPartialSolution -> 'tBenefit) -> //Estimate 
        ('tPartialSolution -> bool) -> //isSolution
        ('tPartialSolution -> 'tSolution) -> //convert
        ('tPartialSolution -> 'tPartialSolution List) -> //expand
        ('tPartialSolution -> bool) -> //isValid
        'tPartialSolution -> //problem
        ('tSolution * 'tBenefit) List
             when 'tBenefit : comparison

    val backtringOptimalSolAux : //inmersioned  backtringOptimalSol
        ('tSolution * 'tBenefit) List -> //Better Solution & Benefit SO FAR
        ('tPartialSolution -> 'tBenefit) -> //Estimate 
        ('tPartialSolution -> bool) -> //isSolution
        ('tPartialSolution -> 'tSolution) -> //convert
        ('tPartialSolution -> 'tPartialSolution List) -> //expand
        ('tPartialSolution -> bool) -> //isValid
        'tPartialSolution -> //problem
        ('tSolution * 'tBenefit) List
             when 'tBenefit : comparison

    val pickTheBest :
         'tPartialSolution -> 
         ('tSolution * 'tBenefit) List ->
         ('tPartialSolution -> 'tBenefit)  -> //Estimate 
         ('tPartialSolution -> 'tSolution) -> //convert 
         ('tSolution * 'tBenefit) List
            when 'tBenefit : comparison

    val canBeImproved:
          'tPartialSolution ->
          ('tSolution * 'tBenefit) List ->
          ('tPartialSolution -> 'tBenefit)  // Estimate
           -> bool  
            when 'tBenefit : comparison

