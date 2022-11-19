module BdTaxCalculator.Library.Tests

open System
open Xunit
open Model
    
type MoneyTestInput = {
    A:        Money
    B:        Money
    Expected: Money
}

type private NonNegativeSubtractionTestData() as this =
    inherit TheoryData<MoneyTestInput>()
    do
        this.Add {
            A = 1m |> Money.fromDecimal
            B = 2m |> Money.fromDecimal
            Expected = 0m |> Money.fromDecimal
        }
        this.Add {
            A = 2m |> Money.fromDecimal
            B = 2m |> Money.fromDecimal
            Expected = 0m |> Money.fromDecimal
        }
        this.Add {
            A = 2m |> Money.fromDecimal
            B = 1m |> Money.fromDecimal
            Expected = 1m |> Money.fromDecimal
        }
    
[<Theory>]
[<ClassData(typeof<NonNegativeSubtractionTestData>)>]
let ``Money Should Subtract in Non-Negative Manner`` (input: MoneyTestInput) =
    Assert.Equal(
        input.Expected,
        input.A |-| input.B
    )
    
type private MoneyAddTestData() as this =
    inherit TheoryData<MoneyTestInput>()
    do
        this.Add {
            A =        12m |> Money.fromDecimal
            B =        24m |> Money.fromDecimal
            Expected = 36m |> Money.fromDecimal
        }
        this.Add {
            A =        12m |> Money.fromDecimal
            B =        00m |> Money.fromDecimal
            Expected = 12m |> Money.fromDecimal
        }
    
[<Theory>]
[<ClassData(typeof<MoneyAddTestData>)>]
let ``Money Should Add``(input: MoneyTestInput) =
    Assert.Equal(
        input.Expected,
        input.A + input.B
    )
    
[<Fact>]
let ``Cannot Create Negative Money``() =
    Assert.Throws<Exception>(
        fun _ -> -1m |> Money.fromDecimal |> ignore    
    )
    
[<Fact>]
let ``Money Should Compare``() =
    Assert.True (
        (1m |> Money.fromDecimal) < (2m |> Money.fromDecimal)     
    )
    Assert.True (
        (2m |> Money.fromDecimal) > (1m |> Money.fromDecimal)     
    )
    Assert.True (
        (2m |> Money.fromDecimal) = (2m |> Money.fromDecimal)     
    )
    Assert.False (
        (2m |> Money.fromDecimal) < (2m |> Money.fromDecimal)     
    )
    Assert.False (
        (2m |> Money.fromDecimal) > (2m |> Money.fromDecimal)     
    )
    Assert.False (
        (1m |> Money.fromDecimal) = (2m |> Money.fromDecimal)     
    )
    
[<Fact>]
let ``Should Create Money Correctly``() =
    Assert.True(
        match
            1m
            |> Money.fromDecimal
            |> fun x -> x.Value
        with
        | 1m -> true
        | _  -> false
    )