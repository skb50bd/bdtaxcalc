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

type Gender =
| Male
| Female

type Particular =
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
    Income:           list<Particular>
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

let houseRentExemption basic houseRent=
    basic
    |> ``50%``
    |> min 3_00_000m
    |> min houseRent

let medicalAllowanceExemption basic medicalAllowance=
    basic
    |> ``10%``
    |> min 1_20_000m
    |> min medicalAllowance

let conveyanceExemption conveyance =
    min conveyance 30_000m

let taxFreeIncome = function
| Male   -> 3_00_000m
| Female -> 3_50_000m

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

let BdTaxBrackets = [
    TaxBracket (1_00_000m,        ``05%``)
    TaxBracket (3_00_000m,        ``10%``)
    TaxBracket (4_00_000m,        ``15%``)
    TaxBracket (5_00_000m,        ``20%``)
    TaxBracket (Decimal.MaxValue, ``25%``)
]

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

let rebateOnInvestment
        (investments: list<Investment>)
        (taxableIncome: decimal)
        : decimal =
    ((0m, 0m), investments)
    ||> List.fold
        (fun (bond, deposit) ->
            function
            | SavingsBond b -> (bond + b, deposit)
            | Deposit d -> (bond, deposit + d)
        )
    |> fun (bond, deposits) ->
        deposits |> min 60_000m |> (+) bond
    |> min (taxableIncome * 0.3m)
    |> (*) 0.15m

let applyRebate
        taxableIncome
        investments
        taxAmount =
    taxAmount
    - rebateOnInvestment investments taxableIncome

let applyAIT maybeAIT taxAmount =
    match maybeAIT with
    | Some (AIT ait) -> taxAmount - ait
    | None -> taxAmount

let taxOutput income investments maybeAIT gender =
    (taxableIncome income) |-| (taxFreeIncome gender)
    |> fun taxableIncome ->
        taxableIncome
        |> calcTaxBeforeRebate
        |> applyRebate taxableIncome investments
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
            | "MALE" | "M"   -> Male
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
            | 0m -> None
            | ait' when ait' > 0m -> ait' |> AIT |> Some
            | _ -> failwith $"Incorrect AIT Amount {ait}"
    }

