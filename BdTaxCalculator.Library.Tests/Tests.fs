module BdTaxCalculator.Library.Tests

open Model
open Xunit
    
type NonNegativeSubtractionTestInput = {
    A:        Money
    B:        Money
    Expected: Money
}

type private NonNegativeSubtractionTestData() as this =
    inherit TheoryData<NonNegativeSubtractionTestInput>()
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
let ``Non-Negative Subtraction Works`` (input: NonNegativeSubtractionTestInput) =
    Assert.Equal(
        input.Expected,
        input.A |-| input.B
    )