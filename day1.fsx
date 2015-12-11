#!/usr/loca/bin/fsharpi

open System;

let directions = System.Console.ReadLine()

directions
|> Seq.fold (fun floor c -> match c with | '(' -> floor + 1 | ')' -> floor - 1 | _ -> floor) 0
|> printfn "Floor: %d"

directions
|> Seq.fold (fun (floor, i) c -> if floor < 0 then (floor, i) else match c with | '(' -> (floor + 1, i+1) | ')' -> (floor - 1, i+1) | _ -> (floor,i+1)) (0,0)
|> snd
|> printfn "Basement instruction: %d"
