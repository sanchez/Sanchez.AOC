module Sanchez.AOC2019.Runner.Solutions.Day2

open Sanchez.AOC2019.Runner.Core
        
type Statements =
    | Addition of int * int * int
    | Multiplication of int * int * int
    | Halt of unit
    
let getAddition (opcodes: int list) =
    let read1 = opcodes.Head
    let read2 = opcodes.Tail.Head
    let write = opcodes.Tail.Tail.Head
    (read1, read2, write)
    |> Addition
    |> (fun x -> (x, opcodes.Tail.Tail.Tail))
    
let getMultiplication (opcodes: int list) =
    let read1 = opcodes.Head
    let read2 = opcodes.Tail.Head
    let write = opcodes.Tail.Tail.Head
    (read1, read2, write)
    |> Multiplication
    |> (fun x -> (x, opcodes.Tail.Tail.Tail))
    
let getHalt (opcodes: int list) =
    Halt ()
    |> (fun x -> (x, opcodes))
        
let getStatement (operator: int) (opcodes: int list) =
    if operator = 1 then
        getAddition opcodes
    elif operator = 2 then
        getMultiplication opcodes
    else
        getHalt opcodes
        
let rec getStatements (opcodes: int list) =
    let operator = opcodes.Head
    let (statement, remainder) = getStatement operator opcodes.Tail
    if remainder.IsEmpty then
        [statement]
    else
        statement::getStatements remainder
       
let processAddition (in1: int) (in2: int) (out: int) (state: int array) =
    let result = state.[in1] + state.[in2]
    let newState = Array.copy state
    Array.set newState out result
    newState
    
let processMultiplication (in1: int) (in2: int) (out: int) (state: int array) =
    let result = state.[in1] * state.[in2]
    let newState = Array.copy state
    Array.set newState out result
    newState
    
let rec processCommand (commands: Statements list) (state: int array) =
    if commands.IsEmpty then
        state
    else
        match commands.Head with
        | Addition (in1, in2, out) ->
            processAddition in1 in2 out state
            |> processCommand commands.Tail
        | Multiplication (in1, in2, out) ->
            processMultiplication in1 in2 out state
            |> processCommand commands.Tail
        | Halt _ -> state

let solution () =
    let codes =
        readInputFile 2
        |> Seq.reduce (fun acc x -> acc + x)
        |> (fun x -> x.Split ',')
        |> Array.toList
        |> List.map int
        |> getStatements
     
    let initialState = Array.zeroCreate 256
    Array.set initialState 1 12
    Array.set initialState 2 2
    
    let resultState = processCommand codes initialState
    
    sprintf "%d" resultState.[0]