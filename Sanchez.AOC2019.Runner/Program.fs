﻿// Learn more about F# at http://fsharp.org

open System
open Sanchez.AOC2019.Runner.Core
open Sanchez.AOC2019.Runner.Solutions
open Sanchez.AOC2019.Runner.Solutions

[<EntryPoint>]
let main argv =
    let solutions: SolutionComputer list = [
        Day8.solution
        Day7.solution
        Day6.solution
        Day5.solution
        Day4.solution
        Day3.solution
        Day2.solution
        Day1.solution
    ]
    
    generateAndPrint solutions |> ignore
    0 // return an integer exit code
