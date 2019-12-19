module Sanchez.AOC2019.Runner.Solutions.Day5

open System
open Sanchez.AOC2019.Runner.Core
open Sanchez.AOC2019.Runner.Solutions
open System.Collections.Generic

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

let rec fetchCode (codes: int array) (position: int) =
    if position < 0 then
        fetchCode codes (position - 1)
        |> (+) position
        |> Array.get codes
    elif position >= codes.Length then 0
    else codes.[position]
    
let pointerValue (codes: int array) (position: int) =
    fetchCode codes position
    |> Array.get codes
    
let fetchOpCode (codes: int array) (position: int) =
    let code = fetchCode codes position
    let operation = code % 100
    let op =
        if operation = 1 then Addition
        elif operation = 2 then Multiplication
        elif operation = 3 then Input
        elif operation = 4 then Output
        elif operation = 5 then JumpTrue
        elif operation = 6 then JumpFalse
        elif operation = 7 then LessThan
        elif operation = 8 then Equals
        else Halt
    let firstParamMode =
        match (code % 1000) / 100 with
        | 1 -> Immediate
        | _ -> Position
    let secondParamMode =
        match (code % 10000) / 1000 with
        | 1 -> Immediate
        | _ -> Position
    let thirdParamMode =
        match (code % 100000) / 10000 with
        | 1 -> Immediate
        | _ -> Position
    (op, firstParamMode, secondParamMode, thirdParamMode)
    
let fetchValue (codes: int array) (position: int) (paramMode: ParamMode) =
    match paramMode with
    | Immediate -> fetchCode codes position
    | Position -> pointerValue codes position
    
let fetchInsAndOut (codes: int array) (position: int) (firstParamMode, secondParamMode, thirdParamMode) =
    let in1 = fetchValue codes position firstParamMode
    let in2 = fetchValue codes (position + 1) secondParamMode
    let out = fetchCode codes (position + 2)
    (in1, in2, out)
    
let fetchTwoInput (codes: int array) (position: int) (firstParamMode, secondParamMode, thirdParamMode) =
    let in1 = fetchValue codes position firstParamMode
    let in2 = fetchValue codes (position + 1) secondParamMode
    (in1, in2)
    
let fetchSingleInAndOut (codes: int array) (position: int) (firstParamMode, _, _) =
    let in1 = fetchValue codes position firstParamMode
    let out = fetchCode codes (position + 1)
    (in1, out)
    
let fetchSingular codes position paramMode =
    fetchValue codes position paramMode
    
let processAddition (codes: int array) (position: int) paramModes =
    let (in1, in2, outLoc) = fetchInsAndOut codes (position + 1) paramModes
    let res = in1 + in2
    let newState = Array.copy codes
    Array.set newState outLoc res
    
    ((position + 4), newState)
    
let processMultiplication (codes: int array) (position: int) paramModes =
    let (in1, in2, outLoc) = fetchInsAndOut codes (position + 1) paramModes
    let res = in1 * in2
    let newState = Array.copy codes
    Array.set newState outLoc res
    
    ((position + 4), newState)
    
let processInput (codes: int array) (position: int) (firstParamMode, secondParamMode, thirdParamMode) =
    let outLoc = fetchCode codes (position + 1)
    let newState = Array.copy codes
    Array.set newState outLoc 5
    
    ((position + 2), newState)

let outputs = new List<int>() 

let processOutput (codes: int array) (position: int) (firstParamMode, secondParamMode, thirdParamMode) =
    let output = fetchSingular codes (position + 1) firstParamMode
    outputs.Add output
    let newState = Array.copy codes
    
    ((position + 2), newState)
    
let processJumpTrue (codes: int array) (position: int) (modes) =
    let (testVal, jmpPos) = fetchTwoInput codes (position + 1) modes
    if testVal <> 0 then (jmpPos, codes)
    else (position + 3, codes)
        
let processJumpFalse (codes: int array) (position: int) (modes) =
    let (testVal, jmpPos) = fetchTwoInput codes (position + 1) modes
    if testVal = 0 then (jmpPos, codes)
    else (position + 3, codes)
    
let processLessThan (codes: int array) (position: int) (modes) =
    let (in1, in2, outPos) = fetchInsAndOut codes (position + 1) modes
    let res =
        if in1 < in2 then 1
        else 0
    let newState = Array.copy codes
    Array.set newState outPos res
    (position + 4, newState)
    
let processEquals (codes: int array) (position: int) (modes) =
    let (in1, in2, outPos) = fetchInsAndOut codes (position + 1) modes
    let res =
        if in1 = in2 then 1
        else 0
    let newState = Array.copy codes
    Array.set newState outPos res
    (position + 4, newState)
    
let rec processOpcode (codes: int array) (position: int) =
    let (operation, firstParamMode, secondParamMode, thirdParamMode) = fetchOpCode codes position
    let (newPosition, newState) =
        match operation with
        | Addition -> processAddition codes position (firstParamMode, secondParamMode, thirdParamMode)
        | Multiplication -> processMultiplication codes position (firstParamMode, secondParamMode, thirdParamMode)
        | Input -> processInput codes position (firstParamMode, secondParamMode, thirdParamMode)
        | Output -> processOutput codes position (firstParamMode, secondParamMode, thirdParamMode)
        | JumpTrue -> processJumpTrue codes position (firstParamMode, secondParamMode, thirdParamMode)
        | JumpFalse -> processJumpFalse codes position (firstParamMode, secondParamMode, thirdParamMode)
        | LessThan -> processLessThan codes position (firstParamMode, secondParamMode, thirdParamMode)
        | Equals -> processEquals codes position (firstParamMode, secondParamMode, thirdParamMode)
        | Halt -> (Int32.MaxValue, codes)
    if newPosition < codes.Length then
        processOpcode newState newPosition
    else newState
    
let solution () =
    let codes =
        readInputFile 5
        |> Seq.reduce (+)
        |> (fun x -> x.Split ',')
        |> Array.map int
        
    let state = processOpcode codes 0
    
    let allChecks =
        outputs
        |> Seq.filter (fun x -> x <> 0)
        |> Seq.length
        
    let diagnoseCode =
        outputs
        |> Seq.toArray
        |> Array.last
        
        
    sprintf "%d" diagnoseCode