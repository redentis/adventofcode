#!/usr/local/bin/fsharpi
#r "/Users/marcusedwards/.nuget/FSharp.Data.2.2.5/lib/portable-net40+sl5+wp8+win8/FSharp.Data.dll"

open System;
open System.IO;
open System.Text.RegularExpressions;
open FSharp.Data;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr

// --------------------------------------------------

let rec extractNumbers = function
    | JsonValue.Number  n -> seq [n]
    | JsonValue.Array  es -> es |> Seq.collect extractNumbers
    | JsonValue.Record ps -> ps |> Seq.collect (fun (_, v) -> extractNumbers v)
    | _ -> seq []

let rec extractNonRedNumbers = function
    | JsonValue.Number  n -> seq [n]
    | JsonValue.Array  es -> es |> Seq.collect extractNonRedNumbers
    | JsonValue.Record ps -> if (ps |> Seq.exists (fun (_,v) -> match v with | JsonValue.String s -> s="red" | _ -> false))
                             then seq [] else ps |> Seq.collect (fun (_, v) -> extractNonRedNumbers v)
    | _ -> seq []
    
let data = JsonValue.Parse(Console.In.ReadLine())


// part 1
data
|> extractNumbers
|> Seq.sum
|> printfn "%A"

// part2
data
|> extractNonRedNumbers
|> Seq.sum
|> printfn "%A"

