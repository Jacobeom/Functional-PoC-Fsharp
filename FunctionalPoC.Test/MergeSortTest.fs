module MergeSortTest

open NUnit.Framework
open FunctionalPoC.MergeSort

[<CustomEquality; CustomComparison>]
//http://blogs.msdn.com/b/dsyme/archive/2009/11/08/equality-and-comparison-constraints-in-f-1-9-7.aspx
type Fraction = {Numerator:int; Denominator:int} 
                override x.Equals(yobj) = 
                    match yobj with
                        | :? Fraction as y -> (x.Numerator = y.Numerator && x.Denominator = y.Denominator )
                        | _ -> false
 
                override x.GetHashCode() = hash x.Numerator + hash x.Denominator
                interface System.IComparable with
                     member x.CompareTo yobj =
                        match yobj with
                        | :? Fraction as y -> compare ((x.Numerator |> double) / (x.Denominator |> double))  
                                                      ((y.Numerator |> double) / (y.Denominator|> double))
                        | _ -> invalidArg "yobj" "cannot compare values of different types"

[<TestFixture>]
type MergeSortTest() = 
   
    [<Test>]
    member x.BasicSort () = 
       Assert.AreEqual(mergesort ( (fun x y -> x < y),  [ 1;10;3;6;7;9;8;5;4;2 ] ), [1..10])


    [<Test>]
    member x.NumericSort () = 
        let list = [1;33;21;2;56;223;2;1;44;37]
        Assert.AreEqual(mergesort ( (fun x y -> x < y), list ), list |> List.sort)


    [<Test>]
    member x.StringSort () = 
        let list = ["Pedro";"Jacobo";"Manuel";"Miguel";"Rosa"]
        Assert.AreEqual(mergesort ( (fun x y -> x < y), list ), list |> List.sort)

    [<Test>]
    member x.CompareFraction () =
        let eq = { Numerator=1; Denominator=2 }.Equals({ Numerator=1; Denominator=2 }) 
        let compareNum = (1 :> System.IComparable).CompareTo(1)
        let compare = ({Numerator=1; Denominator=2} :> System.IComparable).CompareTo({Numerator=1; Denominator=2})

        let compareNum2 = (1 :> System.IComparable).CompareTo(100)
        let compareNum1 = (2 :> System.IComparable).CompareTo(1)
        let compare2 = ({Numerator=1; Denominator=2} :> System.IComparable).CompareTo({Numerator=1; Denominator=20})
        let compare1 = ({Numerator=1; Denominator=1} :> System.IComparable).CompareTo({Numerator=1; Denominator=2})

        Assert.IsTrue(eq)
    
    
    [<Test>]
    member x.FractionSort () =
        let list = [{ Numerator=1; Denominator=2 };{ Numerator=1; Denominator=20 };{ Numerator=1; Denominator=40 };{ Numerator=5; Denominator=2 }]
        Assert.AreEqual(mergesort ( (fun x y -> x < y), list ), list |> List.sort)

   