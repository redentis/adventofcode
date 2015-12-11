#!/usr/local/bin/fsharpi

open System;
open System.IO;
open System.Text.RegularExpressions;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr
  
// --------------------------------------------------

type register = string

type signal = int

type Operand =
  | Signal of signal
  | Ref of register

type UnaryOp =
  | NOP
  | NOT
  
type BinaryOp =
  | AND
  | OR
  | RSHIFT
  | LSHIFT
  
type Expression =
  | Value of signal   
  | Unary of  (int->int) * Operand
  | Binary of (int->int->int) * Operand * Operand
  
type instruction = Expression * register
  

let parseOperand (s:string) :Operand =
  match Int32.TryParse(s) with
  | (false, _) -> Ref(s)
  | (true, n)  -> Signal(n)
  
let parse (s:string) :instruction option =
  let tokens = s.Split(' ')
  if tokens.Length = 3 then Some(Unary (id, tokens.[0] |> parseOperand), tokens.[2])
  else
  if tokens.[0] = "NOT" then Some(Unary ((~~~), tokens.[1] |> parseOperand), tokens.[3])
  else
  match tokens.[1] with
    | "OR"     -> Some(Binary((|||), tokens.[0] |> parseOperand, tokens.[2] |> parseOperand), tokens.[4])
    | "AND"    -> Some(Binary((&&&), tokens.[0] |> parseOperand, tokens.[2] |> parseOperand), tokens.[4])
    | "RSHIFT" -> Some(Binary((>>>), tokens.[0] |> parseOperand, tokens.[2] |> parseOperand), tokens.[4])
    | "LSHIFT" -> Some(Binary((<<<), tokens.[0] |> parseOperand, tokens.[2] |> parseOperand), tokens.[4])
    | _        -> None

// --------------------------------------------------

// Global state!
let registers = new System.Collections.Generic.Dictionary<string, int>()
let promises = new System.Collections.Generic.Dictionary<string, instruction>()

let read (r:register) = if registers.ContainsKey(r) then Some(registers.[r]) else None
    
let rec reify (i:Expression) =
  match i with
    | Value _             -> i
    | Unary(op, Signal v) -> Value(op v)
    | Unary(op, Ref r)    -> match read r with
                             | Some v -> Unary(op, Signal(v)) |> reify
                             | None   -> i
    | Binary(op, Signal l, Signal r) -> Value(op l r)
    | Binary(op, Ref l, r) -> match read l with
                                     | Some v -> Binary(op, Signal(v), r) |> reify
                                     | None   -> i
    | Binary(op, l, Ref r) -> match read r with
                                     | Some v -> Binary(op, l, Signal(v)) |> reify                           
                                     | None   -> i

let run (instrs: instruction list) =
  let rec iter (instrs: instruction list) =
    instrs
      |> List.fold (fun stack (exp, r) -> match (reify exp) with | Value n -> registers.[r] <- n; stack | e -> (e,r)::stack) []
      |> function | [] -> true | instrs' -> if instrs'.Length = instrs.Length then false else iter (List.rev instrs')
  iter instrs       

// --------------------------------------------------

// Part 1   
readLines Console.In
|> Seq.map parse
|> Seq.filter Option.isSome
|> Seq.map Option.get
|> Seq.toList
|> run
|> printfn "Result: %A"

registers.Keys
|> Seq.iter (fun k -> printfn "%s: %d" k registers.[k])

// Part 2
