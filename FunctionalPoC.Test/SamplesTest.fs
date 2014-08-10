namespace FunctionalPoC.Test
open NUnit.Framework
open FunctionalPoC.Samples

[<TestFixture>]
type SamplesTestTest() = 
   
    [<Test>]
    member x.sublists () = 
       let subListResult = sublists [1;2;3;4;5]
       let subListExpectedResult = [[1;2;3;4;5];[1;3;4;5];[1;4;5];[1;5];[1];[2;3;4;5];[2;4;5];[2;5];[2];[3;4;5];[3;5];[3];[4;5];[4];[5];[]]
       
       Assert.AreEqual(subListResult,subListExpectedResult)

       
       
       
    
