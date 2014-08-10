namespace FunctionalPoC.DivideAndConquer
open FunctionalPoC.DivideAndConquer.DivideAndConquer

module BinaryTreeSearch =
     
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
    
    let divide ((node, value):TProblem<'a>): TProblem<'a> List =
        match node with
            | Empty -> []
            | Node(nodeleft, nodeVal, nodeRight) -> if (value <nodeVal) then
                                                         [(nodeleft,value)]
                                                    else 
                                                         [(nodeRight,value)]

    let combine (listSolution:TSolution List): TSolution =
        match listSolution with
            | [x] -> x
            |  _  -> false
        //Should be as above but we do know that divide only returns one element list!!!      
        //listSolution |> List.fold (fun acc x -> acc|| x) false


    let binariTreeSearch tree value = divideAndConquer isTrivial solve divide combine (tree, value)
    
