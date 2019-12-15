module Sanchez.AOC2019.Runner.Solutions.Day2

open Sanchez.AOC2019.Runner.Core

let fetchCode (codes: int array) (position: int) =
    if position >= codes.Length then
        0
    else
        codes.[position]
        
let pointerValue (codes: int array) (position: int) =
    fetchCode codes position
    |> (fun x -> codes.[x])
        
let rec processOpcode (codes: int array) (position: int) =
    let operation = fetchCode codes position
    if operation = 1 || operation = 2 then
        let in1 = pointerValue codes (position + 1)
        let in2 = pointerValue codes (position + 2)
        let outLoc = fetchCode codes (position + 3)
        let newState = Array.copy codes
    
        if operation = 1 then
            // do the addition here
            let res = in1 + in2
            Array.set newState outLoc res
            processOpcode newState (position + 4)
        else
            // do the multiply here
            let res = in1 * in2
            Array.set newState outLoc res
            processOpcode newState (position + 4)
    else codes
    
let testNounAndVerb noun verb =
    let codes =
        readInputFile 2
        |> Seq.reduce (fun acc x -> acc + x)
        |> (fun x -> x.Split ',')
        |> Array.map int
        
    Array.set codes 1 noun
    Array.set codes 2 verb
    
    let machineState = processOpcode codes 0
    machineState.[0]
    
let getNounAndVerb () =
    let combos = seq {
        for noun in 1..99 do
            for verb in 1..99 do
                yield (noun, verb)
    }
    combos
    |> Seq.tryFind (fun (noun, verb) ->
        let res = testNounAndVerb noun verb
        res = 19690720
        )
        
let solution () =
    let starterQuiz = testNounAndVerb 12 2
    
    let resultPair = getNounAndVerb ()
    if resultPair.IsSome then
        let (noun, verb) = resultPair.Value
        let total = 100 * noun + verb
        sprintf "%d, %d" starterQuiz total
    else
        sprintf "%d" starterQuiz