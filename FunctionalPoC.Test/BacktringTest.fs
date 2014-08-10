module BacktringTest

open NUnit.Framework
open FunctionalPoC.Backtrack.Backpack


[<TestFixture>]
type MergeSortTest() = 
   
    [<Test>]
    member x.BackPack () = 
        let itemsToPlace = [('A', 2, 3); ('B', 3, 5); ('C', 4, 6); ('D', 5, 10)]

        let result = backpack itemsToPlace 8
        Assert.AreEqual(result, [(['B';'D'],15)])
