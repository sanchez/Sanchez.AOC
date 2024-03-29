module Sanchez.AOC2019.Runner.Solutions.Day1

open Sanchez.AOC2019.Runner.Core
open System.IO

let fuelForModule mass =
    (mass / 3) - 2
    
let rec advancedFuelForModule mass =
    let fuelCost = (mass / 3) - 2
    if fuelCost <= 0 then 0
    else fuelCost + advancedFuelForModule fuelCost

let solution () =
    let lines =
        readInputFile 1
        |> Seq.map int
        |> Seq.toList
        
    let basicFuel =
        lines
        |> List.map fuelForModule
        |> List.sum
        
    let advancedFuel =
        lines
        |> List.map advancedFuelForModule
        |> List.sum
        
    sprintf "%d, %d" basicFuel advancedFuel