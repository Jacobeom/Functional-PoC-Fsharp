module DivideAndRuleTest

open NUnit.Framework
open FunctionalPoC.DivideAndConquer.DivideAndConquer
open FunctionalPoC.DivideAndConquer.Zeros
open FunctionalPoC.DivideAndConquer.BinaryTreeSearch
open FunctionalPoC.DivideAndConquer.QuickSort
open FunctionalPoC.DivideAndConquer.MergeSortDC


[<TestFixture>]
type DivideAndRuleTest() = 
   
    [<Test>]
    member x.Zeros () = 
       let f (x:float) : float = x+5.0
       let isZero (x:float):bool = (abs x)<0.01
       let result = FunctionalPoC.DivideAndConquer.Zeros.zero isZero -100.0 +100.0 f      
      
       let test element  =
         match element with
            | FunctionalPoC.DivideAndConquer.Zeros.Solution x -> Assert.IsTrue(isZero(f(x)))
            | FunctionalPoC.DivideAndConquer.Zeros.NoSolution -> Assert.IsTrue(false)

       test result
       
       
    [<Test>]
    member x.ZerosNoSol () = 
       let f (x:float) : float = x+5.0
       let isZero (x:float):bool = (abs x)<0.01
       let result = FunctionalPoC.DivideAndConquer.Zeros.zero isZero -1.0 +40.0 f

       let test element  =
         match element with
            | FunctionalPoC.DivideAndConquer.Zeros.Solution x -> Assert.IsTrue(false)
            | FunctionalPoC.DivideAndConquer.Zeros.NoSolution -> Assert.IsTrue(true)

       test result

     [<Test>]
     member x.Power () =
        let result = FunctionalPoC.DivideAndConquer.Power.power 2 10

        let rec powerRec x y = match y with
                                | 0 -> 1
                                | 1 -> x
                                | _ -> x* powerRec x (y-1)

        let powerReduce x y = match y with
                           | 0 -> 1
                           | _ -> [1..y] |> List.fold (fun acc yelt -> acc*x ) 1
        
        let resultReduce = powerReduce 2 10
        let resultRec = powerRec 2 10

        Assert.AreEqual(result, resultRec)
                                  

    [<Test>]
    member x.BinaryTreeSearch () =
     
        let sampleTree : Tree<int> =
            Node(
                Node(Node(Empty,1,Empty),3,Node(Empty,4,Empty)),
                5,
                Node(Empty,7,Empty)
                )

        let anySol = binariTreeSearch sampleTree 4

        Assert.IsTrue(anySol)


    [<Test>]
    member x.QuickSort () =

        let list = [1;33;21;2;56;223;2;1;44;37]
        let quickSortList = quickSort list
        let fsharpSort = list |> List.sort

        Assert.AreEqual(quickSortList,fsharpSort)


    [<Test>]
    member x.MergeSort () =

        let list = [1;33;21;2;56;223;2;1;44;37]
        let mergeSortList = mergeSortDc list
        let fsharpSort = list |> List.sort

        Assert.AreEqual(mergeSortList,fsharpSort)
