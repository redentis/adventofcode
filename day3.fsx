#!/usr/local/bin/fsharpi

open System

let journey directions =
  let move (x,y) = function
    | '^' -> (x,y+1)
    | '>' -> (x+1, y)
    | 'v' -> (x, y-1)
    | '<' -> (x-1, y)
    | _ -> (x,y)
  directions
  |> Seq.fold(fun (p,ps) c ->
              let np = move p c                         
              (np, np::ps)) ((0,0),[])
  |> snd

let deinterlace directions =
  directions
  |> Seq.fold(fun (i, santa, robo) c -> if (i % 2 = 0) then (i + 1, c::santa, robo) else (i + 1, santa, c::robo)) (0, [],[])
  |> fun (_, a, b) -> [List.rev a; List.rev b]

let directions = Console.ReadLine()
  
directions
|> journey
|> Seq.distinct
|> Seq.length
|> printfn "Houses: %d"

directions
|> deinterlace
|> Seq.collect journey
|> Seq.distinct
|> Seq.length
|> printfn "Houses: %d"
