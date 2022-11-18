module Model

open System

let inline private (|-|) a b =
    match a > b with
    | true -> a - b
    | false -> 0m

let ``%`` (a: decimal) (b: decimal) = b * (a / 100m)

let config = {|
    TaxFreeIncome                      = {|
        Male   = 3_00_000m
        Female = 4_00_000m
    |}
    MaxHouseRentExemptionPerYear       = 3_00_000m
    MaxMedicalExemptionPerYear         = 1_20_000m
    MaxConveyanceExemptionPerYear      = 30_000m
    TaxBracketWidths                   = {|
        First  = 1_00_000m
        Second = 3_00_000m
        Third  = 4_00_000m
        Forth  = 5_00_000m
        Fifth  = Decimal.MaxValue
    |}
    MaxRebateOnDeposit           = 60_000m
    InvestableIncomePercentage   = 30m |> ``%``
    RebateOnMaxAllowedInvestment = 15m |> ``%``
|}

type Gender =
| Male
| Female

type private IncomeType =
| Basic
| MedicalAllowance
| HouseRentAllowance
| Conveyance
| Bonus

type private Income = Income of Amount: decimal * IncomeType

type private InvestmentType =
| SavingsBond
| Deposit

type private Investment = Investment of Amount: decimal * InvestmentType

type private AIT =
| AIT of decimal

type private TaxInput = {
    Gender:           Gender
    Income:           list<Income>
    Investments:      list<Investment>
    MinimumTaxInArea: decimal
    MaybeAIT:         option<AIT>
}

type TaxOutput =
| Zero
| Liability  of decimal
| Refundable of decimal

let private mapTaxOutput taxAmount =
    match taxAmount with
    | 0m -> Zero
    | amount when amount > 0m -> taxAmount |> Liability
    | amount when amount < 0m -> taxAmount |> Refundable
    | _ -> failwith "Unreachable"

let private houseRentExemption basic houseRent =
    basic
    |> (50m |> ``%``)
    |> min config.MaxHouseRentExemptionPerYear
    |> min houseRent

let private medicalAllowanceExemption basic medicalAllowance =
    basic
    |> (10m |> ``%``)
    |> min config.MaxMedicalExemptionPerYear
    |> min medicalAllowance

let private conveyanceExemption conveyance = min conveyance config.MaxConveyanceExemptionPerYear

let private taxFreeIncome = function
| Male   -> config.TaxFreeIncome.Male
| Female -> config.TaxFreeIncome.Female

let private getTaxableIncome (income: List<Income>) =
    let basic, houseRent, medical, conveyance, bonus =
        ((0m, 0m, 0m, 0m, 0m), income)
        ||> List.fold
            (fun (basic, houseRent, medical, conveyance, bonus) ->
                function
                | Income (amt, Basic) ->
                    (basic + amt, houseRent, medical, conveyance, bonus)

                | Income (amt, HouseRentAllowance) ->
                    (basic, houseRent + amt, medical, conveyance, bonus)

                | Income (amt, MedicalAllowance) ->
                    (basic, houseRent, medical + amt, conveyance, bonus)

                | Income (amt, Conveyance) ->
                    (basic, houseRent, medical, conveyance + amt, bonus)

                | Income (amt, Bonus) ->
                    (basic, houseRent, medical, conveyance, bonus + amt)
            )

    basic
    + houseRent  |-| (houseRentExemption basic houseRent)
    + medical    |-| (medicalAllowanceExemption basic medical)
    + conveyance |-| (conveyanceExemption conveyance)
    + bonus

type private TaxBracket =
| TaxBracket of bracketWidth: decimal * percentFunc: (decimal -> decimal)

let private BdTaxBrackets =
    [
        (config.TaxBracketWidths.First,  5m |> ``%``)
        (config.TaxBracketWidths.Second, 10m |> ``%``)
        (config.TaxBracketWidths.Third,  15m |> ``%``)
        (config.TaxBracketWidths.Forth,  20m |> ``%``)
        (config.TaxBracketWidths.Fifth,  25m |> ``%``)
    ]
    |> List.map TaxBracket

let private calcTaxBeforeRebate taxableIncome =
    ((taxableIncome, 0m), BdTaxBrackets)
    ||> List.fold
        (fun (income, taxAmount) ->
            function
            | TaxBracket (width, percentFunc) ->
                (
                    income |-| width,
                    income
                    |> min width
                    |> percentFunc
                    |> (+) taxAmount
                )
        )
    |> snd

let private rebateOnInvestment (investments: List<Investment>) taxableIncome =
    ((0m, 0m), investments)
    ||> List.fold
        (fun (bond, deposit) ->
            function
            | Investment (amt, SavingsBond) -> (bond + amt, deposit)
            | Investment (amt, Deposit)     -> (bond, deposit + amt)
        )
    |> fun (bond, deposits) ->
        deposits
        |> min config.MaxRebateOnDeposit
        |> (+) bond
    |> min (taxableIncome |> config.InvestableIncomePercentage)
    |> config.RebateOnMaxAllowedInvestment

let private applyRebate taxableIncome (investments: List<Investment>) taxAmount =
    taxAmount - (rebateOnInvestment investments taxableIncome)

let private applyAIT maybeAIT taxAmount =
    match maybeAIT with
    | Some (AIT ait) -> taxAmount - ait
    | None           -> taxAmount

let private calcTaxAfterRebate (investments: List<Investment>) taxableIncome =
    taxableIncome
    |> calcTaxBeforeRebate
    |> applyRebate taxableIncome investments

let private subtractTaxFreeIncome gender taxableIncome =
    taxableIncome |-| (taxFreeIncome gender)

let private calcTax taxInput =
    taxInput.Income
    |> getTaxableIncome
    |> subtractTaxFreeIncome taxInput.Gender
    |> calcTaxAfterRebate taxInput.Investments
    |> applyAIT taxInput.MaybeAIT
    |> mapTaxOutput

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
        (ait:                decimal) =
    calcTax {
        Gender = gender

        Income = [
            Income (basicIncome, Basic)
            Income (houseRentAllowance, HouseRentAllowance)
            Income (medicalAllowance, MedicalAllowance)
            Income (conveyance, Conveyance)
            Income (bonus, Bonus)
        ]

        Investments = [
            Investment (savingsBond, SavingsBond)
            Investment (deposit, Deposit)
        ]

        MinimumTaxInArea = minimumTaxInArea

        MaybeAIT =
            match ait with
            | 0m                  -> None
            | ait when ait > 0m -> ait |> AIT |> Some
            | _                   -> failwith $"Incorrect AIT Amount {ait}"
    }