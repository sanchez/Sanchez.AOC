module Sanchez.AOC2019.Runner.Solutions.Day7

open System
open Sanchez.AOC2019.Runner.Core

let runOnAmplifier (codes: int array) (phaseSetting: int) (lastAmpValue: int) =
    let (outputs, state) = IntCode.executeIntCode (Array.copy codes) [phaseSetting; lastAmpValue]
    List.head outputs
    
let runAllAmplifiers (codes: int array) (phaseA, phaseB, phaseC, phaseD, phaseE) =
    runOnAmplifier codes phaseA 0
    |> runOnAmplifier codes phaseB
    |> runOnAmplifier codes phaseC
    |> runOnAmplifier codes phaseD
    |> runOnAmplifier codes phaseE
    
let hasDuplicated (phaseA, phaseB, phaseC, phaseD, phaseE) =
    let items = [phaseA; phaseB; phaseC; phaseD; phaseE]
    List.distinct items
    |> (fun x -> x.Length = items.Length)
    
let runOnLoopedAmplifier (codes: int array) (phaseSetting: int) (inputs: int list) =
    let (outputs, state) = IntCode.executeIntCode (Array.copy codes) ([phaseSetting] @ inputs)
    outputs
    
let runOnSafeAmplifier (codes: int array) (phaseSetting: int) (inputs: int list) =
    try
        let (outputs, state) = IntCode.executeIntCode (Array.copy codes) ([phaseSetting] @ inputs)
        (true, outputs)
    with
    | IntCode.InternalException outputs -> (false, outputs)
    
let rec runLoopedAmplifiers (codes: int array) (phaseA, phaseB, phaseC, phaseD, phaseE) (inputA, inputB, inputC, inputD, inputE) =
    let (aSuccess, phaseAOut) = runOnSafeAmplifier codes phaseA inputA
    let bInput = inputB @ phaseAOut
    let (bSuccess, phaseBOut) = runOnSafeAmplifier codes phaseB bInput
    let cInput = inputC @ phaseBOut
    let (cSuccess, phaseCOut) = runOnSafeAmplifier codes phaseC cInput
    let dInput = inputD @ phaseCOut
    let (dSuccess, phaseDOut) = runOnSafeAmplifier codes phaseD dInput
    let eInput = inputE @ phaseDOut
    let (eSuccess, phaseEOut) = runOnSafeAmplifier codes phaseE eInput
    
    if eSuccess then
        phaseEOut
        |> List.sortDescending
        |> List.head
    else
        runLoopedAmplifiers codes (phaseA, phaseB, phaseC, phaseD, phaseE) (inputA@phaseEOut, bInput, cInput, dInput, eInput)
        
let solution () =
    let codes =
        readInputFile 7
        |> Seq.reduce (+)
        |> (fun x -> x.Split ',')
        |> Array.map int
    
    let testOne =
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        |> (fun x -> x.Split ',')
        |> Array.map int
        |> (fun x -> runAllAmplifiers x (4, 3, 2, 1, 0))
        |> (=) 43210
        
    let testTwo =
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        |> (fun x -> x.Split ',')
        |> Array.map int
        |> (fun x -> runAllAmplifiers x (1, 0, 4, 3, 2))
        |> (=) 65210
        
    let highestThruster =
        seq {
            for phaseA in 0..4 do
                for phaseB in 0..4 do
                    for phaseC in 0..4 do
                        for phaseD in 0..4 do
                            for phaseE in 0..4 do
                                yield (phaseA, phaseB, phaseC, phaseD, phaseE)
        }
        |> Seq.filter hasDuplicated
        |> Seq.map (runAllAmplifiers codes)
        |> Seq.sortDescending
        |> Seq.head
        
//    let testThree =
//        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
//        |> (fun x -> x.Split ',')
//        |> Array.map int
//        |> (fun x -> runLoopedAmplifiers x (9, 8, 7, 6, 5) ([0], [], [], [], []))
//        
//    let testFour =
//        "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
//        |> (fun x -> x.Split ',')
//        |> Array.map int
//        |> (fun x -> runLoopedAmplifiers x (9, 8, 7, 6, 5) ([0], [], [], [], []))
    
    sprintf "%d" highestThruster