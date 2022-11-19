module Model

open System

let inline private (|-|) a b =
    match a > b with
    | true  -> a - b
    | false -> 0m

let inline ``%`` a b = b * (a / 100m)

let config = {|
    TaxFreeIncome = {|
        Male   = 3_00_000m
        Female = 3_50_000m
    |}

    HouseRentExemption = {|
        YearlyMax = 3_00_000m
        PercentageOfBasic = 50m |> ``%``
    |}

    MedicalExemption = {|
        YearlyMax = 1_20_000m
        PercentageOfBasic = 10m |> ``%``
    |}

    YearlyConveyanceExemption = 30_000m

    Slabs = [
        (1_00_000m,         5m |> ``%``)
        (3_00_000m,        10m |> ``%``)
        (4_00_000m,        15m |> ``%``)
        (5_00_000m,        20m |> ``%``)
        (Decimal.MaxValue, 25m |> ``%``)
    ]

    RebateOnDeposit            = 60_000m
    InvestableIncomePercentage = 25m |> ``%``
    RebateOnAllowedInvestment  = 15m |> ``%``
|}

type private Slab = {
    Width: decimal
    Rate:  decimal -> decimal
}

let private BdTaxBrackets =
    config.Slabs
    |> List.map (fun (w, r) -> { Width = w; Rate = r })

type Gender =
| Male
| Female

type private Income = {
    BasicAllowances:     List<decimal>
    HouseRentAllowances: List<decimal>
    MedicalAllowances:   List<decimal>
    Conveyances:         List<decimal>
    Bonuses:             List<decimal>
}

type private Investment = {
    SavingsBonds: List<decimal>
    Deposits:     List<decimal>
}

type private AIT = AIT of decimal

type TaxCalculationError =
| NegativeIncome     of decimal
| NegativeInvestment of decimal
| NegativeAit        of decimal

type private TaxInput = {
    Gender:           Gender
    Income:           Income
    Investment:      Investment
    MinimumTaxInArea: decimal
    MaybeAIT:         AIT
}

type TaxOutput =
| Zero
| Liability  of decimal
| Refundable of decimal

let private mapTaxOutput = function
| 0m                      -> Zero
| amount when amount > 0m -> amount |> Liability
| amount when amount < 0m -> amount |> abs |> Refundable
| _                       -> failwith "Unreachable"

let private houseRentExemption basic houseRent =
    basic
    |> config.HouseRentExemption.PercentageOfBasic
    |> min config.HouseRentExemption.YearlyMax
    |> min houseRent

let private medicalAllowanceExemption basic medicalAllowance =
    basic
    |> config.MedicalExemption.PercentageOfBasic
    |> min config.MedicalExemption.YearlyMax
    |> min medicalAllowance

let private conveyanceExemption conveyance =
    min conveyance config.YearlyConveyanceExemption

let private taxFreeIncome = function
| Male   -> config.TaxFreeIncome.Male
| Female -> config.TaxFreeIncome.Female

let private getTaxableIncome (income: Income) =

    let basicTotal      = income.BasicAllowances     |> List.sum
    let houseRentTotal  = income.HouseRentAllowances |> List.sum
    let medicalTotal    = income.MedicalAllowances   |> List.sum
    let conveyanceTotal = income.Conveyances         |> List.sum
    let bonusTotal      = income.Bonuses             |> List.sum

    basicTotal
    + houseRentTotal  |-| (houseRentExemption basicTotal houseRentTotal)
    + medicalTotal    |-| (medicalAllowanceExemption basicTotal medicalTotal)
    + conveyanceTotal |-| (conveyanceExemption conveyanceTotal)
    + bonusTotal

let private calcTaxBeforeRebate taxableIncome =
    ((taxableIncome, 0m), BdTaxBrackets)
    ||> List.fold
        (fun (income, taxAmount) { Width = width; Rate = taxRate } ->
            (
                income |-| width,
                income
                |> min width
                |> taxRate
                |> (+) taxAmount
            )
        )
    |> snd

let private rebateOnInvestment (investment: Investment) taxableIncome =
    let bondTotal = investment.SavingsBonds |> List.sum
    let depositTotal = investment.Deposits |> List.sum

    depositTotal
    |> min config.RebateOnDeposit
    |> (+) bondTotal
    |> min (taxableIncome |> config.InvestableIncomePercentage)
    |> config.RebateOnAllowedInvestment

let private applyRebate taxableIncome investments taxAmount =
    taxAmount - (rebateOnInvestment investments taxableIncome)

let private applyAIT ait taxAmount =
    match ait with
    | AIT ait -> taxAmount - ait

let private calcTaxAfterRebate investments taxableIncome =
    taxableIncome
    |> calcTaxBeforeRebate
    |> applyRebate taxableIncome investments

let private subtractTaxFreeIncome gender taxableIncome =
    taxableIncome |-| (taxFreeIncome gender)

let private calcTax taxInput =
    taxInput.Income
    |> getTaxableIncome
    |> subtractTaxFreeIncome taxInput.Gender
    |> calcTaxAfterRebate taxInput.Investment
    |> min taxInput.MinimumTaxInArea
    |> applyAIT taxInput.MaybeAIT
    |> mapTaxOutput

let validateIncome income =
    match income < 0m with
    | true  -> Ok ()
    | false -> income |> NegativeIncome |> Error

let validateInvestment investment =
    match investment < 0m with
    | true  ->  Ok ()
    | false -> investment |> NegativeInvestment |> Error

let validateAit ait =
    match ait with
    | _ when ait < 0m -> ait |> NegativeAit |> Error
    | _               -> Ok ()

let (>=>) prev it =
    match prev with
    | Ok _      -> it
    | Error err -> Error err

let calculateTax
        (gender:             Gender)
        (minimumTaxInArea:   decimal)
        (basicIncome:        decimal)
        (houseRentAllowance: decimal)
        (medicalAllowance:   decimal)
        (conveyance:         decimal)
        (bonus:              decimal)
        (savingsBond:        decimal)
        (deposit:            decimal)
        (ait:                decimal)
        : Result<TaxOutput, TaxCalculationError> =
    validateIncome basicIncome
    >=> validateIncome houseRentAllowance
    >=> validateIncome medicalAllowance
    >=> validateIncome conveyance
    >=> validateIncome bonus
    >=> validateInvestment savingsBond
    >=> validateInvestment deposit
    >=> validateAit ait
    >=> (
            {
                Gender = gender

                Income = {
                    BasicAllowances =     [ basicIncome ]
                    HouseRentAllowances = [ houseRentAllowance ]
                    MedicalAllowances =   [ medicalAllowance ]
                    Conveyances =         [ conveyance ]
                    Bonuses =             [ bonus ]
                }

                Investment = {
                    SavingsBonds = [ savingsBond ]
                    Deposits =     [ deposit ]
                }

                MinimumTaxInArea = minimumTaxInArea

                MaybeAIT = ait |> AIT
            }
            |> calcTax
            |> Ok
    )