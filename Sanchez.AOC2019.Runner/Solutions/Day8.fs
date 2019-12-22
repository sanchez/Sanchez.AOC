module Sanchez.AOC2019.Runner.Solutions.Day8

open System
open Sanchez.AOC2019.Runner.Core

type Pixel =
    | Black
    | White
    | Transparent
    
exception InvalidPixel
    
let convertToPixel input =
    match input with
    | '0' -> Black
    | '1' -> White
    | '2' -> Transparent
    | _ -> raise InvalidPixel
    
let getPixelChar input =
    match input with
    | Black -> ' '
    | White -> '#'
    | Transparent -> ' '

let rec fetchLayer (width: int) (height: int) (layerPosition: int) (pixels: char array) =
    if layerPosition >= pixels.Length then []
    else
        let layer =
            [|
                for i in 0..(height-1) do
                    yield [|
                        for j in 0..(width-1) do
                            yield convertToPixel (pixels.[j + (i * width) + layerPosition])
                    |]
            |]
            
        let nextPosition = layerPosition + width * height
        layer::fetchLayer width height nextPosition pixels
        
let countInLayer (goal: Pixel) (pixels: Pixel array array) =
    pixels
    |> Array.map (Array.filter ((=) goal))
    |> Array.map Array.length
    |> Array.reduce (+)
    
let whichPixel (current: Pixel) (newPixel: Pixel) =
    match newPixel with
    | Transparent -> current
    | _ -> newPixel
    
let rec computeImageLayer (width: int) (height: int) (currentImage: Pixel array array) (layers: Pixel array array list) =
    if layers.IsEmpty then currentImage
    else
        let layer = layers.Head
        let newImage =
            [|
                for i in 0..(height-1) do
                    yield [|
                        for j in 0..(width-1) do
                            yield whichPixel (currentImage.[i].[j]) (layer.[i].[j])
                    |]
            |]
        computeImageLayer width height newImage layers.Tail
    
let computeFinalImage (width: int) (height: int) (pixels: Pixel array array list) =
    let starterImage =
        [|
            for i in 1..height do
                yield [|
                    for j in 1..width do
                        yield Transparent
                |]
        |]
    pixels
    |> List.rev
    |> computeImageLayer width height starterImage
    
let printImage (width: int) (height: int) (image: Pixel array array) =
    printf "\n\nImage: \n"
    image
    |> Array.iter (fun row -> printfn "%s" (String (Array.map getPixelChar row)))
    printf "\n"

let solution () =
    let testImage =
        "0222112222120000"
        |> Seq.toArray
        |> fetchLayer 2 2 0
        |> computeFinalImage 2 2
    
    let pixels =
        readInputFile 8
        |> Seq.reduce (+)
        |> Seq.toArray
        
    let layers =
        pixels
        |> fetchLayer 25 6 0
        
    let mostZeros =
        layers
        |> List.sortBy (countInLayer Black)
        |> List.head
        |> (fun x -> (countInLayer White x) * (countInLayer Transparent x))
        
    let resultImage =
        layers
        |> computeFinalImage 25 6
        
    printImage 25 6 resultImage
        
    sprintf "%d" mostZeros