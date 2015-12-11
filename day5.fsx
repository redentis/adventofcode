#!/usr/local/bin/fshapi

open System;
open System.IO;
open System.Text.RegularExpressions;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr

// --------------------------------------------------
   
let matches (re:string) (s:string) = Regex.Matches(s, re)
let isMatch (mc:MatchCollection) = mc.Count > 0
let matchCount (mc:MatchCollection) = mc.Count

let rule1 = matches "[aeiou]" >> matchCount >> (<=) 3
let rule2 = matches "(.)\1" >> isMatch
let rule3 = matches "ab|cd|pq|xy" >> isMatch >> not

let test = ["rthkunfaakmwmush";"qxlnvjguikqcyfzt";"sleaoasjspnjctqt";"lactpqehuhmzwfjl"];;

let words = System.Console.In |> readLines |> Seq.cache

words
|> Seq.filter rule1
|> Seq.filter rule2
|> Seq.filter rule3
|> Seq.length
|> printfn "Part one: %A"

// --------------------------------------------------

let rule4 = matches "(..).*\1" >> isMatch
let rule5 = matches "(.).\1" >> isMatch

words
|> Seq.filter rule4
|> Seq.filter rule5
|> Seq.length
|> printfn "Part two: %A"
    
