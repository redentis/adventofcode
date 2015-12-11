#!/usr/local/bin/fsharpi

open System;
open System.IO;
open System.Text.RegularExpressions;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr
  
// Part 1 --------------------------------------------------

let presents = readLines Console.In |> Seq.cache

let rule1 (s:string) = s.Replace("\\\"",".")

let rule2 (s:string) = s.Replace("\"","");

let rule3 (s:string) = Regex.Replace(s, "\\\x..", ".");

let rule4 (s:string) = s.Replace("\\\\",".");
    
let ruleset = rule4 >> rule1 >> rule2 >> rule3

let codeSize =
  presents
  |> Seq.map  ruleset
  |> Seq.sumBy String.length

let literalSize =
  presents
  |> Seq.sumBy String.length

printfn "literal: %d; code: %d; diff=%d" literalSize codeSize (literalSize - codeSize)

// Part 2 --------------------------------------------------
  
let rule5 (s:string) = s.Replace("\\","\\\\")

let rule6 (s:string) = s.Replace("\"", "\\\"")

let rule7 (s:string) = "\"" + s + "\""

let encoding = rule5 >> rule6 >> rule7

let encodedSize =
  presents
  |> Seq.map encoding
  |> Seq.sumBy String.length

printfn "Part 2: literalSize=%d; encodedSize=%d; diff=%d" literalSize encodedSize (encodedSize-literalSize)
