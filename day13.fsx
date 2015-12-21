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

let mkPair a b = if a < b then (a,b) else (b,a)

let mkValue (sign:string) (value:string) =
    let v = value |> int
    match sign with
        | "lose" -> -v
        | _ -> v 
    
let parse (s:string) =
    let mc = Regex.Matches(s, @"^(\w+) would (\w+) ([0-9]+) happiness units by sitting next to (\w+).$")
    if mc.Count = 1 then
        let gs = mc.[0].Groups
        Some ((mkPair gs.[1].Value gs.[4].Value), (mkValue gs.[2].Value gs.[3].Value))
    else None
    
let guests = ["Alice"; "Bob"; "Carol"; "David"; "Eric"; "Frank"; "George"; "Mallory"];

let lookup = new System.Collections.Generic.Dictionary<string * string, int>()

let metric a b =
    let p = mkPair a b
    if lookup.ContainsKey p then lookup.[p] else 0

let tableMetric (gs:string list) =
    gs |> Seq.pairwise
    |> Seq.fold (fun cheer (a,b) -> cheer + (metric a b)) 0
    |> (+) (metric (Seq.head gs) (Seq.last gs))
    
Console.In
|> readLines
|> Seq.map parse
|> Seq.iter (function | Some (p,v) -> if lookup.ContainsKey p then lookup.[p] <- lookup.[p] + v else lookup.[p] <- v
                      | None       -> printfn "Unknown instruction")

guests
|> permutations
|> Seq.map tableMetric
|> Seq.max
|> printfn "%A"

// part 2

guests
|> Seq.iter (fun g -> lookup.[(mkPair "host" g)] <- 0)

"host"::guests
|> permutations
|> Seq.map tableMetric
|> Seq.max
|> printfn "%A"
