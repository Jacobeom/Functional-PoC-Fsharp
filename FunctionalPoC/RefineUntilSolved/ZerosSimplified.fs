namespace FunctionalPoC.RefineUntilSolved
open FunctionalPoC.RefineUntilSolved.RefineUntilSolved

module ZerosSimplified =

     type TProblem  = float * //[Lower Range,
                      float * // Upper Range]
                      (float->float) * // f(x)
                      (float->bool) // y = 0?, 0.1=0?

     type TSolution = NoSolution | Solution of float

     let isTrivial ((lR, uR, f,isZero): TProblem) : bool = 
         match isZero((uR-lR)) with
                | true -> true
                | _    ->  isZero (f ((uR+lR)/2.0) ) 
    
                
     let solve ((lR, uR, f, isZero): TProblem) : TSolution = 
            match isZero((uR-lR)) with
                | true -> NoSolution
                | _    -> Solution((uR+lR)/(2.0)) 
      
     let refine ((lR, uR, f, isZero): TProblem): TProblem  = 
                                 if ((f (lR) * f((lR+uR)/2.0))< 0.0) then
                                     (lR, ((lR+uR)/2.0), f, isZero)
                                 else
                                     (((lR+uR)/2.0), uR, f, isZero)

     let zerosSimplified (problem:TProblem) : TSolution = 
         refineUntilSolved isTrivial solve refine problem