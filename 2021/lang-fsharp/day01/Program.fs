open System
open System.IO

let second (_, s) =
    s

let solve1 depths =
    depths
        |> List.fold(fun (prevDepth, increases) depth -> (depth, if depth > prevDepth then increases + 1 else increases)) (100000000, 0)
        |> second

let rec slidingWindow previousDepth depths increases =
    match depths with
        | a :: b :: c :: rest ->
            let depth = a + b + c
            let newIncreases = if depth > previousDepth then increases + 1 else increases
            slidingWindow depth (b :: c :: rest) newIncreases
        | _ -> increases

let solve2 depths =
    match depths with
    | a :: b :: c :: rest -> slidingWindow (a + b + c) (b :: c :: rest) 0
    | _ -> -1


[<EntryPoint>]
let main argv =
    match argv with
    | [| filePath |] ->
        let parsedInput = File.ReadAllLines(filePath) |> Seq.map (fun str -> int str) |> Seq.toList
        let solution1 = solve1 parsedInput 
        let solution2 = solve2 parsedInput
        printfn "Part 1: %i" solution1
        printfn "Part 2: %i" solution2
    | _ ->
        printfn "Expected only a single input file path as an argument"
    0 // return an integer exit code