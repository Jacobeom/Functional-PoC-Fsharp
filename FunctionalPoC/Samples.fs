namespace FunctionalPoC

module Samples =

        let rec combineWith (element:'a) (list: 'a List) : (('a List) List) =
            match list with 
            | [] -> [[element]]
            | (head::tail) -> (element::head::tail) :: (combineWith element tail)
    
        let rec sublists (list: 'a List): ('a List) List =
           match list with 
            | [] -> [[]]
            | head::tail -> (combineWith head tail) @ sublists tail 
        

        
      