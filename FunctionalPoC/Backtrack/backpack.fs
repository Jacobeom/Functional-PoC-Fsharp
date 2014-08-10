namespace FunctionalPoC.Backtrack
open FunctionalPoC.Backtrack.BacktringOptimalSol

module Backpack =
 
    type TPartialSol<'a> = ('a * int * int) List *  //List of items (name * weight * value) to be placed in the backpack
                           int *                    //weigth available in the backpack
                           'a List *                //List of items (name) placed in the backpack
                           int                      //Value in the backpack Sum(value placed items)
    type TBenefit = int

    type TFinal<'a> = 'a List 

    let isSolution ((availableList,backPackWeight,placedList,totalValue):TPartialSol<'a>):bool =
        availableList = [] || backPackWeight =0

    let convert ((_,_,placedList,_):TPartialSol<'a>):TFinal<'a> =
        placedList

    let rec expand ((availableList,backPackWeight,placedList,totalValue):TPartialSol<'a>): TPartialSol<'a> List =
        match availableList with 
             | (elt,weight,value)::tail -> if (weight<=backPackWeight) then
                                                [(tail,backPackWeight-weight,elt::placedList,totalValue+value);
                                                 (tail,backPackWeight,placedList,totalValue)]
                                           else
                                                expand (tail,backPackWeight,placedList,totalValue)
             | _ -> []
     
    let isValid ((_,_,_,_):TPartialSol<'a>):bool = true

    let estimate ((availableList,backPackWeight,_,totalValue):TPartialSol<'a>):TBenefit =
     match availableList with 
            | (elt,weight,value)::tail -> totalValue + (backPackWeight*value/weight)
            | _ -> totalValue

    let backpack (problem:('a * int * int) List) (backPackWeight:int) : (TFinal<'a> * TBenefit) List  = 
            let orderedItems= problem |> List.sortWith 
                                        (fun (_,weightx,valuex) (_,weighty,valuey) -> ((valuey |> double) / (weighty |> double)).CompareTo((valuex |> double) /(weightx |> double))) 
            backtringOptimalSol estimate isSolution convert expand isValid  (orderedItems,backPackWeight,[],0)