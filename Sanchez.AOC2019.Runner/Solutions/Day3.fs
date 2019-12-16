module Sanchez.AOC2019.Runner.Solutions.Day3

open System
open Sanchez.AOC2019.Runner.Core
open System.Linq

type Direction =
    | Up of int
    | Down of int
    | Left of int
    | Right of int
    
let convertToDirection (input: string) =
    let len = int input.[1..]
    match input.[0] with
    | 'U' -> Some (Up len)
    | 'D' -> Some (Down len)
    | 'L' -> Some (Left len)
    | 'R' -> Some (Right len)
    | _ -> None
    
let rec generateCoords (currentPos: int * int) (directions: Direction list) =
    if directions.IsEmpty then
        []
    else
        let dir = directions.Head
        let (x, y) = currentPos
        let coords =
            match dir with
            | Up len -> [| for i in 1..len -> (x, y + i) |]
            | Down len -> [| for i in 1..len -> (x, y - i) |]
            | Left len -> [| for i in 1..len -> (x - i, y) |]
            | Right len -> [| for i in 1..len -> (x + i, y) |]
        let finalCord =
            if coords.Length = 0 then currentPos
            else Array.last coords
        coords
        |> Array.toList
        |> (fun x -> x @ (generateCoords finalCord directions.Tail))

let solution () =
    let directions =
        readInputFile 3
        |> Seq.map (fun x -> x.Split ',')
        |> Seq.toArray
        
    let wire1 =
        directions.[0]
        |> Array.map convertToDirection
        |> Array.filter (fun x -> x.IsSome)
        |> Array.map (fun x -> x.Value)
        |> Array.toList
    let wire1Coords = generateCoords (0, 0) wire1
    
    let wire2 =
        directions.[1]
        |> Array.map convertToDirection
        |> Array.filter (fun x -> x.IsSome)
        |> Array.map (fun x -> x.Value)
        |> Array.toList
    let wire2Coords = generateCoords (0, 0) wire2
    
    let resultDistance =
        wire1Coords.Intersect wire2Coords
        |> Seq.map (fun (x, y) -> (Math.Abs x) + (Math.Abs y))
        |> Seq.sort
        |> Seq.head
    
//    let resultPoints =
//        wire1Coords
//        |> List.filter (fun x -> wire2Coords |> List.contains x)
    
    sprintf "%d" resultDistance