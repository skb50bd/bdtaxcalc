module BdTaxCalculator.Library.Money

open System

[<Struct>]
[<CustomComparison>]
[<CustomEquality>]
type Money =
| Money of decimal
    with
    
    member this.Value =
        match this with Money amt -> amt
        
    override this.GetHashCode () = this.Value.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? Money as other -> this.Value = other.Value
        | _                 -> false

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Money as other -> this.Value.CompareTo other.Value
            | _                 -> -1

    static member (+) (left, right) =
        match (left, right) with
        | (Money l, Money r) -> l + r |> Money

    static member (-) (left, right) =
        match (left, right) with
        | (Money l, Money r) -> l - r
        |> Money
        
    static member (|-|) (left, right) =
        match (left, right) with
        | (Money l, Money r) when l > r -> l - r |> Money
        | _                             -> 0m    |> Money
    
    static member Zero = 0m |> Money
    static member MaxValue = Decimal.MaxValue |> Money
    static member MinValue = Decimal.MinValue |> Money
    
let (%) (percent: decimal) (amount: Money) =
    match amount with
        | Money amt -> (percent * amt) |> Money

let tryMoney value =
    match value < 0m with
    | false -> value |> Money |> Some
    | true  -> None
    
let money value =
    match tryMoney value with
    | Some m -> m
    | None   -> failwith $"Money must be non-negative: {value}"