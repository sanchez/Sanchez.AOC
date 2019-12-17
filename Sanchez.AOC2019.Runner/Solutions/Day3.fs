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
        
let getMinOption (a: int option) (b: int option) =
    if a.IsSome && b.IsSome then
        if a.Value < b.Value then a
        else b
    elif a.IsSome then a
    elif b.IsSome then b
    else None

let rec getWireShortestDistance (wireCoords: (int * int) list) (coord: int * int) (depth: int) =
    if wireCoords.IsEmpty then
        None
    else
        let nextDistance = getWireShortestDistance wireCoords.Tail coord (depth + 1)
        if wireCoords.Head = coord then
            match nextDistance with
            | Some dist -> List.min [depth; dist] |> Some
            | None -> Some depth
        else
            nextDistance

let getWireDistance (wireCoords: (int * int) list) (coord: int*int) =
    wireCoords |> List.tryFindIndex ((=) coord)
    
let getTotalWireDistance (wire1Coords: (int * int) list) (wire2Coords: (int * int) list) (coord: int * int) =
    let wire1Length = wire1Coords |> List.tryFindIndex ((=) coord)
    let wire2Length = wire2Coords |> List.tryFindIndex ((=) coord)
    Option.map2 (+) wire1Length wire2Length

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
        
    let resultWireCoords =
        wire1Coords.Intersect wire2Coords
        
    let distances =
        resultWireCoords
        |> Seq.map (getTotalWireDistance wire1Coords wire2Coords)
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.map (fun x -> x.Value)
        |> Seq.sort
        
    let totalDistance =
        resultWireCoords
        |> Seq.map (getTotalWireDistance wire1Coords wire2Coords)
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.map (fun x -> x.Value)
        |> Seq.sort
        |> Seq.head
    
//    let resultPoints =
//        wire1Coords
//        |> List.filter (fun x -> wire2Coords |> List.contains x)
    
    sprintf "%d, %d" resultDistance totalDistance