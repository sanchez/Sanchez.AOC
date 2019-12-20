module Sanchez.AOC2019.Runner.Solutions.Day6

open System
open Sanchez.AOC2019.Runner.Core

//type Planet = {name: string; root: Planet}

let getLink (line: string) =
    let split = line.Split(")")
    (split.[1], split.[0])

let generateLinks (acc: Map<string, string>) (line: string) =
    let (child, parent) = getLink line
    acc.Add(child, parent)
    
let rec countIndirect (planets: Map<string, string>) (node: string) =
    if node = "COM" then 0
    else
        let newPlanet = planets.[node]
        1 + (countIndirect planets newPlanet)
    
let countDirectAndIndirect (planets: Map<string, string>) (direct, indirect) key value =
    let extraIndirect = countIndirect planets value
    (direct + 1, indirect + extraIndirect)
    
let possibleNodes (planets: Map<string, string>) (currentNode: string) =
    let childrenPlanets =
        query {
            for link in planets do
                where (link.Value = currentNode)
                select link.Key
        }
        |> Seq.toList
    match planets.TryFind currentNode with
    | Some parent -> parent::childrenPlanets
    | None -> childrenPlanets
    
let rec getPath (planets: Map<string, string>) (visited: string list) (currentNode: string) =
    if currentNode = "SAN" then [|"SAN"|]
    else
        let possible =
            possibleNodes planets currentNode
            |> List.distinct
            |> List.filter (fun x -> not (List.contains x visited))
        let paths =
            possible
            |> List.map (getPath planets (currentNode::visited))
            |> List.filter (fun x -> x.Length <> 0)
            |> List.toArray
            |> Array.sortBy (fun x -> x.Length)
            
        Array.tryHead paths
        |> Option.defaultValue [||]
        |> Array.append [|currentNode|]

let solution () =
    let testInput =
        [
            "COM)B"
            "B)C"
            "C)D"
            "D)E"
            "E)F"
            "B)G"
            "G)H"
            "D)I"
            "E)J"
            "J)K"
            "K)L"
            "K)YOU"
            "I)SAN"
        ]
        |> Seq.fold generateLinks Map.empty
    let testResult = testInput |> Map.fold (countDirectAndIndirect testInput) (0, 0)
    
    let planets =
        readInputFile 6
        |> Seq.fold generateLinks Map.empty
        
    let directAndIndirect =
        planets
        |> Map.fold (countDirectAndIndirect planets) (0, 0)
        
    let sumTotal = (fst directAndIndirect) + (snd directAndIndirect)
    
    let testPath = getPath testInput [] "YOU"
    let pathToSanta = getPath planets [] "YOU"
    
    sprintf "%d" sumTotal