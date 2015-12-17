#!/usr/local/bin/fsharpi

open System;
open System.IO;
open System.Text.RegularExpressions;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr    
    
let rec permutations (xs:'a list) :'a list list =
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
    match xs with
    | []      -> [[]]
    | x :: xs -> List.concat (List.map (insertions x) (permutations xs))

// --------------------------------------------------

type point = string
type route = string * string
type trip = point * int

let mkRoute a b =
    if (a < b) then (a, b) else (b, a)
    
let parse (s:string) =
    match s.Split(' ') with
        | [|a;_;b;_;c|] -> Some((mkRoute a b), c|>int)
        | _            -> None

let routes =
    Console.In
    |> readLines
    |> Seq.map parse
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> dict

let points =
    routes.Keys
    |> Seq.collect (fun (a,b) -> [a;b])
    |> Seq.distinct
    |> Seq.toList

let distances =
    points
    |> permutations
    |> List.map (fun ps -> ps |> List.pairwise |> List.fold (fun dist (a,b) -> dist + routes.[(mkRoute a b)]) 0)

// part 1
distances    
|> List.min
|> printfn "%A"

// part 2
distances    
|> List.max
|> printfn "%A"



            
