module Tests

open BdTaxCalculator.Library.Money
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
            A = 1m |> money
            B = 2m |> money
            Expected = 0m |> money
        }
        this.Add {
            A = 2m |> money
            B = 2m |> money
            Expected = 0m |> money
        }
        this.Add {
            A = 2m |> money
            B = 1m |> money
            Expected = 1m |> money
        }
    
[<Theory>]
[<ClassData(typeof<NonNegativeSubtractionTestData>)>]
let ``Non-Negative Subtraction Works`` (input: NonNegativeSubtractionTestInput) =
    Assert.Equal(
        input.Expected,
        input.A - input.B
    )