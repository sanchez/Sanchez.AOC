module Sanchez.AOC2019.Runner.Solutions.Day4

open System
open Sanchez.AOC2019.Runner.Core
open System.Linq

let rec convertToList (depth: int) (num: int) =
    if num = 0 then
        []
    else
        let currentVal = num % 10
        let remainder = num / 10
        (convertToList (depth + 1) remainder) @ [currentVal]
        
let rec convertToString (items: int list) =
    if items.IsEmpty then ""
    else items.Head.ToString () + convertToString items.Tail
        
let rec hasAdjacent (items: int list) =
    if items.IsEmpty || items.Tail.IsEmpty then
        false
    else
        if items.Head = items.Tail.Head then
            true
        else
            hasAdjacent items.Tail
            
let rec alwaysIncreases (lastItem: int) (items: int list) =
    if items.IsEmpty then true
    else
        if items.Head < lastItem then false
        else alwaysIncreases items.Head items.Tail
        
let rec sameDigitCount (activeItem: int) (count: int) (items: int list) =
    if items.IsEmpty then count = 2
    else
        if items.Head = activeItem then
            sameDigitCount activeItem (count + 1) items.Tail
        else
            if count = 2 then true
            else sameDigitCount items.Head 1 items.Tail
            
let hasEvenAdjacent (items: int list) =
    sameDigitCount items.Head 1 items.Tail

let solution () =
    let lower = 382345
    let upper = 843167
    
    let possible =
        [ lower .. upper ]
        |> List.map (convertToList 0)
        |> List.filter hasAdjacent
        |> List.filter (alwaysIncreases 0)
    
    let extraPossible =
        possible
        |> List.filter hasEvenAdjacent
    
    sprintf "%d, %d" possible.Length extraPossible.Length