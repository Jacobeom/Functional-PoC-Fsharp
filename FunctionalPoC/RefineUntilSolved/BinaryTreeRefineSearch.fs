namespace FunctionalPoC.RefineUntilSolved
open FunctionalPoC.RefineUntilSolved.RefineUntilSolved

module BinaryTreeRefineSearch =
    type Tree<'a> = Empty | Node of Tree<'a> * 'a  * Tree<'a>
    type TProblem<'a> = Tree<'a> * 'a 
    type TSolution = bool


    let isTrivial ((node, value):TProblem<'a>) = 
        match node with
             | Empty -> true
             | Node (_,nodeValue,_) -> nodeValue.Equals(value)

    let solve ((node, value):TProblem<'a>) =
        match node with
             | Empty -> false
             | _     -> true
    
    let refine ((node, value):TProblem<'a>): TProblem<'a> =
        match node with
            | Empty -> (Empty,value)
            | Node(nodeleft, nodeVal, nodeRight) -> if (value <nodeVal) then
                                                         (nodeleft,value)
                                                    else 
                                                         (nodeRight,value)
  
    let binaryTreeRefineSearch (problem: TProblem<'a>) :TSolution = 
        refineUntilSolved isTrivial solve refine problem