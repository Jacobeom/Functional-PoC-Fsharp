namespace FunctionalPoC.DivideAndConquer
open FunctionalPoC.DivideAndConquer.DivideAndConquer

//Being f(x)=t a continuos function (http://en.wikipedia.org/wiki/Continuous_function)
//            find x value where f(x) = 0 (in case it exists)   
module Zeros =

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

    let divide ((lR, uR, f, isZero): TProblem): TProblem List = 
                                     if ((f (lR) * f((lR+uR)/2.0))< 0.0) then
                                         [(lR, ((lR+uR)/2.0), f, isZero)] 
                                     else
                                         [(((lR+uR)/2.0), uR, f, isZero)]

    let combine ([x]:TSolution List) : TSolution = x

    let zero (isZero : float->bool) (lR: float) (uR: float) (f: float->float) =
     divideAndConquer isTrivial solve divide combine  (lR, uR, f, isZero )
