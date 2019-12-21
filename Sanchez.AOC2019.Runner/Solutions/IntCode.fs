module Sanchez.AOC2019.Runner.Solutions.IntCode
open System

type ParamMode =
    | Position
    | Immediate
    
type Operation =
    | Addition
    | Multiplication
    | Input
    | Output
    | JumpTrue
    | JumpFalse
    | LessThan
    | Equals
    | Halt
    
exception InternalException of int list
exception OverflowException
    
let rec fetchCode (codes: int array) (position: int) =
    if position < 0 then
        fetchCode codes (position - 1)
        |> (+) position
        |> Array.get codes
    elif position >= codes.Length then raise OverflowException
    else codes.[position]
    
let pointerValue (codes: int array) (position: int) =
    fetchCode codes position
    |> Array.get codes

let fetchOpCode (codes: int array) (position) =
    let code = fetchCode codes position
    let operation = code % 100
    let op =
        match operation with
        | 1 -> Addition
        | 2 -> Multiplication
        | 3 -> Input
        | 4 -> Output
        | 5 -> JumpTrue
        | 6 -> JumpFalse
        | 7 -> LessThan
        | 8 -> Equals
        | _ -> Halt
        
    let loadParam paramVal =
        match paramVal with
        | 1 -> Immediate
        | _ -> Position
        
    let firstParamMode = loadParam ((code % 1000) / 100)
    let secondParamMode = loadParam ((code % 10000) / 1000)
    let thirdParamMode = loadParam ((code % 100000) / 10000)
    
    (op, (firstParamMode, secondParamMode, thirdParamMode))
    
let fetchValue (codes: int array) (position: int) (paramMode: ParamMode) =
    match paramMode with
    | Immediate -> fetchCode codes position
    | Position -> pointerValue codes position
    
let fetchTwoInOneOut (codes: int array) (position: int) (firstParamMode, secondParamMode, _) =
    let in1 = fetchValue codes position firstParamMode
    let in2 = fetchValue codes (position + 1) secondParamMode
    let out = fetchCode codes (position + 2)
    (in1, in2, out)
    
let fetchTwoIn (codes: int array) (position: int) (firstParamMode, secondParamMode, _) =
    let in1 = fetchValue codes position firstParamMode
    let in2 = fetchValue codes (position + 1) secondParamMode
    (in1, in2)
    
let fetchOneInOneOut (codes: int array) (position: int) (firstParamMode, _, _) =
    let in1 = fetchValue codes position firstParamMode
    let out = fetchCode codes (position + 1)
    (in1, out)
    
let processAddition (codes: int array) (position: int) paramModes input outputs =
    let (in1, in2, outLoc) = fetchTwoInOneOut codes (position + 1) paramModes
    let res = in1 + in2
    let newState = Array.copy codes
    Array.set newState outLoc res
    
    ((position + 4), newState, input, outputs)
    
let processMultiplication (codes: int array) (position: int) paramModes input outputs =
    let (in1, in2, outLoc) = fetchTwoInOneOut codes (position + 1) paramModes
    let res = in1 * in2
    let newState = Array.copy codes
    Array.set newState outLoc res
    
    ((position + 4), newState, input, outputs)
    
let processInput (codes: int array) (position: int) paramModes (inputs: int list) outputs =
    let outLoc = fetchCode codes (position + 1)
    let newState = Array.copy codes
    Array.set newState outLoc inputs.Head
    
    ((position + 2), newState, inputs.Tail, outputs)
    
let processOutput (codes: int array) (position: int) (firstParamMode, _, _) input outputs =
    let output = fetchValue codes (position + 1) firstParamMode
    
    ((position + 2), codes, input, output::outputs)
    
let processJumpTrue (codes: int array) (position: int) paramModes input outputs =
    let (testVal, jmpPos) = fetchTwoIn codes (position + 1) paramModes
    if testVal <> 0 then (jmpPos, codes, input, outputs)
    else (position + 3, codes, input, outputs)
    
let processJumpFalse (codes: int array) (position: int) paramModes input outputs =
    let (testVal, jmpPos) = fetchTwoIn codes (position + 1) paramModes
    if testVal = 0 then (jmpPos, codes, input, outputs)
    else (position + 3, codes, input, outputs)
    
let processLessThan (codes: int array) (position: int) paramModes input outputs =
    let (in1, in2, outPos) = fetchTwoInOneOut codes (position + 1) paramModes
    let res =
        if in1 < in2 then 1
        else 0
    let newState = Array.copy codes
    Array.set newState outPos res
    (position + 4, newState, input, outputs)
    
let processEquals (codes: int array) (position: int) paramModes input outputs =
    let (in1, in2, outPos) = fetchTwoInOneOut codes (position + 1) paramModes
    let res =
        if in1 = in2 then 1
        else 0
    let newState = Array.copy codes
    Array.set newState outPos res
    (position + 4, newState, input, outputs)
    
let rec processOpCode (codes: int array) (position: int) (inputs: int list) (outputs: int list) =
    let (operation, paramModes) = fetchOpCode codes position
//    printfn "Running Operation: %A" operation
    let opFunc =
        match operation with
        | Addition -> processAddition
        | Multiplication -> processMultiplication
        | Input -> processInput
        | Output -> processOutput
        | JumpTrue -> processJumpTrue
        | JumpFalse -> processJumpFalse
        | LessThan -> processLessThan
        | Equals -> processEquals
        | Halt -> (fun _ _ _ _ _ -> (Int32.MaxValue, codes, inputs, outputs))
    try
        let (newPosition, newState, newInputs, newOutputs) = opFunc codes position paramModes inputs outputs
        if newPosition < codes.Length then
            processOpCode newState newPosition newInputs newOutputs
        else (newOutputs, newState)
    with
    | InternalException ex -> raise (InternalException ex)
    | ex -> raise (InternalException outputs)
    
let executeIntCode (codes: int array) (inputs: int list) =
    processOpCode codes 0 inputs []