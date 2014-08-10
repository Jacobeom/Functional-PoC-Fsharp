namespace FunctionalPoC.DivideAndConquer
open FunctionalPoC.DivideAndConquer.DivideAndConquer
// MergeSort algo (http://en.wikipedia.org/wiki/MergeSort)
 
module MergeSortDC  =
    

    type TElement<'a> = 'a
    type TProblem<'a> = TElement<'a> List
    type TSolution<'a> = TElement<'a> List

   
    let isTrivial (list: TProblem<'a>) :bool =
        match (list) with 
        | [] -> true
        | x::[] -> true
        | _ -> false

    let solve (list: TProblem<'a>) :TSolution<'a> = list
       


    //Split list in odd/even placed elements [x1,x2,x3,x4] = [[x1;x3];[x2:x4]]
    let rec divide (list: TProblem<'a>) :TSolution<'a> List = 
        match (list) with
            | [] -> [[]; []]
            | a::[] -> [[a]; []]
            | x1::x2::tail -> let [listA; listB] = divide tail
                              [x1::listA;x2::listB] 
             
    let rec combine (problemList: TProblem<'a> List) :TSolution<'a>  =
        match (problemList)  with
         | [[];[]] -> []
         | [list;[]] -> list
         | [[];list] -> list
         | [hA::tailA;hB::tailB] -> if (hA<hB) then 
                                       hA::combine [tailA;hB::tailB] 
                                    else
                                        hB::combine [hA::tailA;tailB]


    let mergeSortDc (list: TProblem<'a>) : TSolution<'a> =
        divideAndConquer isTrivial solve divide combine list
