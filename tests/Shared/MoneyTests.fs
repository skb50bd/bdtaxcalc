module Shared.Tests.Money

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif
open Shared.Model

let testParamsMany label xs tests =
  xs
  |> List.collect (fun x -> tests|> List.map (fun test -> test x))
  |> testList label

let testParams label xs test =
  testParamsMany label xs [ test ]

let moneyOperations =
    testList "Money Operations" [
        testParams
            "Addition"
            [
                {|
                    Left =     12m |> Money.fromDecimal
                    Right =    24m |> Money.fromDecimal
                    Expected = 36m |> Money.fromDecimal
                |}
                {|
                    Left =     12m |> Money.fromDecimal
                    Right =    00m |> Money.fromDecimal
                    Expected = 12m |> Money.fromDecimal
                |}
            ]
            (fun input ->
                testCase
                    $"{input.Left} + {input.Right} = {input.Expected}"
                    <| fun _ ->
                        Expect.equal
                            (input.Left + input.Right)
                            (input.Expected)
                            "Should be Equal"
            )

        testParams
            "Subtraction"
            [
                {|
                     Left =     4m |> Money.fromDecimal
                     Right =    2m |> Money.fromDecimal
                     Expected = 2m
                |}
                {|
                     Left =     4m |> Money.fromDecimal
                     Right =    8m |> Money.fromDecimal
                     Expected = -4m
                |}
                {|
                     Left =     4m |> Money.fromDecimal
                     Right =    4m |> Money.fromDecimal
                     Expected = 0m
                |}
            ]
            (fun input ->
                testCase
                    $"{input.Left} - {input.Right} = {input.Expected}"
                    <| fun _ ->
                        Expect.equal
                            (input.Left - input.Right).Value
                            (input.Expected)
                            "Should be Equal"
            )

        testParams
            "Non-Negative Subtraction"
            [
                {|
                     Left =     4m |> Money.fromDecimal
                     Right =    2m |> Money.fromDecimal
                     Expected = 2m |> Money.fromDecimal
                |}
                {|
                     Left =     4m |> Money.fromDecimal
                     Right =    8m |> Money.fromDecimal
                     Expected = 0m |> Money.fromDecimal
                |}
                {|
                     Left =     4m |> Money.fromDecimal
                     Right =    4m |> Money.fromDecimal
                     Expected = 0m |> Money.fromDecimal
                |}
            ]
            (fun input ->
                testCase
                    $"{input.Left} |-| {input.Right} = {input.Expected}"
                    <| fun _ ->
                        Expect.equal
                            (input.Left |-| input.Right)
                            (input.Expected)
                            "Should be Equal"
            )

        testParams
            "Percentage"
             [
                {|
                     Amount =     50m |> Money.fromDecimal
                     Percentage = 5m
                     Expected =   2.5m |> Money.fromDecimal
                |}
                {|
                    Amount =     100m |> Money.fromDecimal
                    Percentage = 21m
                    Expected =   21m |> Money.fromDecimal
                |}
                {|
                     Amount =     0.5m |> Money.fromDecimal
                     Percentage = 25m
                     Expected =   0.125m |> Money.fromDecimal
                |}
            ]
            (fun input ->
                testCase
                    $"{input.Percentage}%% of {input.Amount} = {input.Expected}"
                    <| fun _ ->
                        Expect.equal
                            (input.Percentage %% input.Amount)
                            (input.Expected)
                            "Should be Equal"
            )

        testCase
            "Cannot Create Negative Money"
            <| fun _ ->
                Expect.throws
                    (fun _ -> -1m |> Money.fromDecimal |> ignore)
                    "Should Throw"

        testCase
            "Create Money with Correct Value"
            <| fun _ ->
                Expect.equal
                    (11.123m |> Money.fromDecimal |> fun x -> x.Value)
                    (11.123m)
                    "Should be Equal"

                Expect.equal
                    (11m |> Money.fromDecimal |> fun x -> x.Value)
                    (11m)
                    "Should be Equal"

        testCase
            "Comparison"
            <| fun _ ->
                Expect.isTrue
                    ((2m |> Money.fromDecimal) > (1m |> Money.fromDecimal))
                    "Greater Than"

                Expect.isTrue
                    ((2m |> Money.fromDecimal) < (3m |> Money.fromDecimal))
                    "Less Than"

                Expect.equal
                    (2m |> Money.fromDecimal)
                    (2m |> Money.fromDecimal)
                    "Equal"
    ]