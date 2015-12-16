#!/usr/local/bin/fsharpi

open System;
open System.IO;
open System.Text;

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr

// --------------------------------------------------

// rule 1: at least two doubles, that don't overlap and are different characters
// rule 2: at least one run of three characters

let rule1 (cs:byte array) =
    let rec aux ds last i =
        if i >= cs.Length then ds
        else
            if cs.[i] = last then aux (cs.[i]::ds) 255uy (i+1) else aux ds cs.[i] (i+1)
    match (aux [] 255uy 0) with
        | a::b::t when not (a = b) -> true
        | _ -> false
  
let rule2 (cs:byte array) =
    let rec aux run last i =
//        printfn "run=%d; last=%d; i=%d" run last i
        if run > 2 || i >= cs.Length then (run > 2)
        else
            if cs.[i] = last+1uy then aux (run + 1) cs.[i] (i+1) else aux 1 cs.[i] (i+1)
    aux 1 255uy 0

let rule3 (cs:byte array) =
    let rec aux i =
        if i >= cs.Length then true
        else if cs.[i] = 14uy || cs.[i] = 8uy || cs.[i] = 11uy then false else aux (i + 1)
    aux 0
        
let toBytes (s:string) =
    s
    |> Encoding.Default.GetBytes
    |> Array.map (fun c-> c - 97uy);

let fromBytes (bs:byte array) =
    bs
    |> Array.map (fun c-> c + 97uy)
    |> Encoding.Default.GetString
                    
let nextPasswords (seed:string) =
    let rec inc i (bs:byte array) =
        if i < 0 then bs
        else if (bs.[i]=25uy)
             then
                 bs.[i] <- 0uy
                 inc (i-1) bs
             else
                 let v = bs.[i] + 1uy;
                 if (v = 14uy || v = 8uy || v = 11uy) then bs.[i] <- v + 1uy else bs.[i] <- v
                 bs
    Seq.unfold (fun (bs:byte array) -> let newbs = inc (bs.Length-1) bs
                                       Some (newbs, newbs)) (toBytes seed)                        
                             

    
"hxbxxyzz"
|> nextPasswords
|> Seq.filter (fun p -> rule3 p && rule1 p && rule2 p)
|> Seq.head
|> fromBytes
|> printfn "%A"
