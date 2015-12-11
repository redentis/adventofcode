#!/usr/local/bin/fshapi

open System;
open System.IO;
open System.Text.RegularExpressions;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr

// --------------------------------------------------

let parser = new Regex("^(toggle|turn on|turn off) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$");

let mkPoint (g:GroupCollection) =
  let lx = g.[2].Value |> int
  let ly = g.[3].Value |> int
  let rx = g.[4].Value |> int
  let ry = g.[5].Value |> int
  seq { for y in ly..ry do for x in lx..rx do yield (x, y) }

let (|Toggle|_|) (mc:MatchCollection) =
  if mc.Count = 1 && mc.[0].Groups.Count = 6 && mc.[0].Groups.[1].Value = "toggle"
  then Some (mc.[0].Groups |> mkPoint)
  else None

let (|On|_|) (mc:MatchCollection) =
  if mc.Count = 1 && mc.[0].Groups.Count = 6 && mc.[0].Groups.[1].Value = "turn on"
  then Some (mc.[0].Groups |> mkPoint)
  else None

let (|Off|_|) (mc:MatchCollection) =
  if mc.Count = 1 && mc.[0].Groups.Count = 6 && mc.[0].Groups.[1].Value = "turn off"
  then Some (mc.[0].Groups |> mkPoint)
  else None

let board = Array2D.create 1000 1000 0

let instructions = Console.In |> readLines |> Seq.cache

// Part 1 --------------------------------------------------
   
instructions
|> Seq.iter (fun i ->
     match parser.Matches(i) with
             | Toggle ps -> ps |> Seq.iter (fun (x,y) -> if board.[x,y] = 1 then board.[x,y] <- 0 else board.[x,y] <- 1)
             | On  ps    -> ps |> Seq.iter (fun (x,y) -> board.[x,y] <- 1) 
             | Off ps    -> ps |> Seq.iter (fun (x,y) -> board.[x,y] <- 0) 
             | _ -> printfn "Nothing")

board
|> Seq.cast<int>
|> Seq.sum
|> printfn ("Lights on = %d")

// Part 2 --------------------------------------------------
   
let board2 = Array2D.create 1000 1000 0             

instructions
|> Seq.iter (fun i ->
     match parser.Matches(i) with
             | Toggle ps -> ps |> Seq.iter (fun (x,y) -> board2.[x,y] <- board2.[x,y] + 2)
             | On  ps    -> ps |> Seq.iter (fun (x,y) -> board2.[x,y] <- board2.[x,y] + 1) 
             | Off ps    -> ps |> Seq.iter (fun (x,y) -> if board2.[x,y] > 0 then board2.[x,y] <- board2.[x,y] - 1) 
             | _ -> printfn "Nothing")

board2
|> Seq.cast<int>
|> Seq.sum
|> printfn ("Brightness = %d")
