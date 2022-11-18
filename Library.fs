module Model

open System

let inline (|-|) a b =
    if a > b then a - b
    else          0m

let nPercent n = n / 100m
let nPercentOf n value = value * nPercent n
let ``05%`` = nPercentOf 5m
let ``10%`` = nPercentOf 10m
let ``15%`` = nPercentOf 15m
let ``20%`` = nPercentOf 20m
let ``25%`` = nPercentOf 25m
let ``30%`` = nPercentOf 30m
let ``50%`` = nPercentOf 50m

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
    MaxRebateOnDeposit                 = 60_000m
    InvestableIncomePercentage         = ``30%``
    RebateOnMaxAllowedInvestment = ``15%``
|}

type Gender =
| Male
| Female

type Income =
| Basic              of decimal
| MedicalAllowance   of decimal
| HouseRentAllowance of decimal
| Conveyance         of decimal
| Bonus              of decimal

type Investment =
| SavingsBond of decimal
| Deposit     of decimal

type AIT =
| AIT of decimal

type TaxInput = {
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

let mapTaxOutput taxAmount =
    if taxAmount = 0m   then Zero
    elif taxAmount > 0m then taxAmount |> Liability
    else                     taxAmount |> abs |> Refundable

let houseRentExemption basic houseRent =
    basic
    |> ``50%``
    |> min config.MaxHouseRentExemptionPerYear
    |> min houseRent

let medicalAllowanceExemption basic medicalAllowance =
    basic
    |> ``10%``
    |> min config.MaxMedicalExemptionPerYear
    |> min medicalAllowance

let conveyanceExemption conveyance = min conveyance config.MaxConveyanceExemptionPerYear

let taxFreeIncome = function
| Male   -> config.TaxFreeIncome.Male
| Female -> config.TaxFreeIncome.Female

let taxableIncome income =
    let (basic, houseRent, medical, conveyance, bonus) =
        ((0m, 0m, 0m, 0m, 0m), income)
        ||> List.fold
            (fun (basic, houseRent, medical, conveyance, bonus) ->
                function
                | Basic b ->
                    (basic + b, houseRent, medical, conveyance, bonus)

                | HouseRentAllowance hr ->
                    (basic, houseRent + hr, medical, conveyance, bonus)

                | MedicalAllowance m ->
                    (basic, houseRent, medical + m, conveyance, bonus)

                | Conveyance c ->
                    (basic, houseRent, medical, conveyance + c, bonus)

                | Bonus b ->
                    (basic, houseRent, medical, conveyance, bonus + b)
            )

    basic
    + houseRent  |-| (houseRentExemption basic houseRent)
    + medical    |-| (medicalAllowanceExemption basic medical)
    + conveyance |-| (conveyanceExemption conveyance)
    + bonus

type TaxBracket =
| TaxBracket of bracketWidth: decimal * percentFunc: (decimal -> decimal)

let BdTaxBrackets =
    [
        (config.TaxBracketWidths.First,  ``05%``)
        (config.TaxBracketWidths.Second, ``10%``)
        (config.TaxBracketWidths.Third,  ``15%``)
        (config.TaxBracketWidths.Forth,  ``20%``)
        (config.TaxBracketWidths.Fifth,  ``25%``)
    ]
    |> List.map TaxBracket

let calcTaxBeforeRebate taxableIncome =
    (((taxableIncome), 0m), BdTaxBrackets)
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

let rebateOnInvestment investments taxableIncome =
    ((0m, 0m), investments)
    ||> List.fold
        (fun (bond, deposit) ->
            function
            | SavingsBond b -> (bond + b, deposit)
            | Deposit d     -> (bond, deposit + d)
        )
    |> fun (bond, deposits) ->
        deposits
        |> min config.MaxRebateOnDeposit
        |> (+) bond
    |> min (taxableIncome |> config.InvestableIncomePercentage)
    |> config.RebateOnMaxAllowedInvestment

let applyRebate taxableIncome investments taxAmount =
    taxAmount - (rebateOnInvestment investments taxableIncome)

let applyAIT maybeAIT taxAmount =
    match maybeAIT with
    | Some (AIT ait) -> taxAmount - ait
    | None           -> taxAmount

let calcTaxAfterRebate investments taxableIncome =
    taxableIncome
    |> calcTaxBeforeRebate
    |> applyRebate taxableIncome investments

let subtractTaxFreeIncome gender taxableIncome =
    taxableIncome |-| (taxFreeIncome gender)

let taxOutput income investments maybeAIT gender =
    income
    |> taxableIncome
    |> subtractTaxFreeIncome gender
    |> calcTaxAfterRebate investments
    |> applyAIT maybeAIT
    |> mapTaxOutput

let calcTax taxInput =
    taxOutput
        taxInput.Income
        taxInput.Investments
        taxInput.MaybeAIT
        taxInput.Gender

let calculateTax
        (gender: string)
        minimumTaxInArea
        basicIncome
        houseRentAllowance
        medicalAllowance
        conveyance
        bonus
        savingsBond
        deposit
        ait =
    calcTax {
        Gender =
            match gender.ToUpper() with
            | "MALE"   | "M" -> Male
            | "FEMALE" | "F" -> Female
            | _              -> failwith $"Unsupported Gender: {gender}"

        Income = [
            basicIncome        |> Basic
            houseRentAllowance |> HouseRentAllowance
            medicalAllowance   |> MedicalAllowance
            conveyance         |> Conveyance
            bonus              |> Bonus
        ]

        Investments = [
            savingsBond |> SavingsBond
            deposit     |> Deposit
        ]

        MinimumTaxInArea = minimumTaxInArea

        MaybeAIT =
            match ait with
            | 0m                  -> None
            | ait' when ait' > 0m -> ait' |> AIT |> Some
            | _                   -> failwith $"Incorrect AIT Amount {ait}"
    }