module Sanchez.AOC2019.Runner.Core

type SolutionComputer = unit -> string

let generateSolutions (computers: SolutionComputer list) =
    computers |> List.map (fun x -> x())

let rec processAndPrint (computer: SolutionComputer list) (depth: int) =
    if computer.IsEmpty then
        ignore
    else
        let res = computer.Head ()
        printfn "Solution %d: %s" depth res
        processAndPrint computer.Tail (depth + 1)
    
let generateAndPrint computers =
    printfn "Processing solutions:"
    processAndPrint computers 1