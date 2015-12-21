#!/usr/local/bin/fsharpi

open System
open System.IO;
open System.Text.RegularExpressions;

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr

let parse (s:string) =
    let mc = Regex.Matches(s, "^Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)$")
    if mc.Count = 1 then
        let gc = mc.[0].Groups
        Some (gc.[1].Value, dict [(gc.[2].Value, int gc.[3].Value);
                                  (gc.[4].Value, int gc.[5].Value);
                                  (gc.[6].Value, int gc.[7].Value)]);
    else None
        
let clues = [
    ("children", 3, (=));
    ("cats", 7, (>));
    ("samoyeds", 2, (=));
    ("pomeranians", 3, (<));
    ("akitas", 0, (=));
    ("vizslas", 0, (=));
    ("goldfish", 5, (<));
    ("trees", 3, (>));
    ("cars", 2, (=));
    ("perfumes", 1, (=))]

let data =
    Console.In
    |> readLines
    |> Seq.map parse
    |> Seq.cache

// part 1
data
|> Seq.filter (function | Some(_, d) -> clues |> Seq.fold (fun flag (k,v,_) -> match flag with
                                                                               | true   -> not (d.ContainsKey k) || d.[k] = v
                                                                               | false  -> false) true
                        | None -> false)
|> printfn "%A"

// part 2
data
|> Seq.filter (function | Some(_, d) -> clues |> Seq.fold (fun flag (k,v, f) -> match flag with
                                                                                | true   -> not (d.ContainsKey k) || f d.[k] v
                                                                                | false  -> false) true
                        | None -> false)
|> printfn "%A"
