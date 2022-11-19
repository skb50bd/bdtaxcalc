module BdTaxCalculator.Library.Model

open System
open Money

type Slab = {
    Width: Money
    Rate:  Money -> Money
}

type TaxFreeIncomeConfig = {
    Male:   Money
    Female: Money
}

type TaxExemptionRule = {
    YearlyMax: Money
    FromBasic: Money -> Money
}

type Config = {
    TaxFreeIncome:             TaxFreeIncomeConfig
    HouseRentExemption:        TaxExemptionRule
    MedicalExemption:          TaxExemptionRule
    ConveyanceExemption:       TaxExemptionRule
    RebateOnDeposit:           Money
    InvestableIncome:          Money -> Money
    RebateOnAllowedInvestment: Money -> Money
    Slabs:                     list<Slab>
}

let config = {
    TaxFreeIncome = {
        Male   = 3_00_000m |> money
        Female = 3_50_000m |> money
    }
    
    HouseRentExemption = {
        YearlyMax = 3_00_000m |> money
        FromBasic = 50m |> (%)
    }
    
    MedicalExemption = {
        YearlyMax = 1_20_000m |> money
        FromBasic = 10m |> (%)
    }
    
    ConveyanceExemption = {
        YearlyMax = 30_000m |> money
        FromBasic = 100m |> (%)
    }
    
    Slabs = [
        { Width = 1_00_000m        |> money; Rate =  5m |> (%) }
        { Width = 3_00_000m        |> money; Rate = 10m |> (%) }
        { Width = 4_00_000m        |> money; Rate = 15m |> (%) }
        { Width = 5_00_000m        |> money; Rate = 20m |> (%) }
        { Width = Decimal.MaxValue |> money; Rate = 25m |> (%) }
    ]
    
    RebateOnDeposit            = 60_000m |> money
    InvestableIncome           = 25m |> (%)
    RebateOnAllowedInvestment  = 15m |> (%)
}

type Gender =
| Male
| Female

type private Income = {
    BasicAllowances:     list<Money>
    HouseRentAllowances: list<Money>
    MedicalAllowances:   list<Money>
    Conveyances:         list<Money>
    Bonuses:             list<Money>
}

type private Investment = {
    SavingsBonds: list<Money>
    Deposits:     list<Money>
}

type private AIT =
| AIT of Money

type TaxCalculationError =
| NegativeIncome     of decimal
| NegativeInvestment of decimal
| NegativeAit        of decimal

type private TaxInput = {
    Gender:           Gender
    Income:           Income
    Investments:      Investment
    MinimumTaxInArea: Money
    MaybeAIT:         AIT
}

type TaxOutput =
| Zero
| Liability  of Money
| Refundable of Money

let private houseRentExemption basic houseRent =
    basic
    |> config.HouseRentExemption.FromBasic
    |> min config.HouseRentExemption.YearlyMax
    |> min houseRent

let private medicalAllowanceExemption basic medicalAllowance =
    basic
    |> config.MedicalExemption.FromBasic
    |> min config.MedicalExemption.YearlyMax
    |> min medicalAllowance

let private conveyanceExemption conveyance =
    min conveyance config.ConveyanceExemption.YearlyMax

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
    ((taxableIncome, Money.Zero), config.Slabs)
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
    |> min (taxableIncome |> config.InvestableIncome)
    |> config.RebateOnAllowedInvestment

let private applyRebate taxableIncome investments taxAmount =
    taxAmount |-| (rebateOnInvestment investments taxableIncome)

let private applyAIT ait taxAmount =
    match (ait, taxAmount) with
    | (AIT (Money ait), (Money tax)) ->
        if   ait = tax then Zero
        elif ait < tax then (tax - ait) |> money |> Liability
        else                (ait - tax) |> money |> Refundable
        
let private calcTaxAfterRebate investments taxableIncome =
    taxableIncome
    |> calcTaxBeforeRebate
    |> applyRebate taxableIncome investments

let private subtractTaxFreeIncome gender taxableIncome =
    taxableIncome |-| (taxFreeIncome gender)

let private applyMinimumTax minimumTaxInArea taxAmount =
    if taxAmount > Money.Zero then min minimumTaxInArea taxAmount
    elif taxAmount = Money.Zero then taxAmount
    else failwith "Unreachable!"

let private calcTax taxInput =
    taxInput.Income
    |> getTaxableIncome
    |> subtractTaxFreeIncome taxInput.Gender
    |> calcTaxAfterRebate taxInput.Investments
    |> applyMinimumTax taxInput.MinimumTaxInArea
    |> applyAIT taxInput.MaybeAIT

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
    prev |> Result.bind (fun _ -> it)

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
                    BasicAllowances =     [ basicIncome |> money ]
                    HouseRentAllowances = [ houseRentAllowance |> money ]
                    MedicalAllowances =   [ medicalAllowance |> money ]
                    Conveyances =         [ conveyance |> money ]
                    Bonuses =             [ bonus |> money ]
                }

                Investments = {
                    SavingsBonds = [ savingsBond |> money ]
                    Deposits =     [ deposit |> money ]
                }

                MinimumTaxInArea = money minimumTaxInArea

                MaybeAIT = ait |> money |> AIT
            }
            |> calcTax
            |> Ok
    )