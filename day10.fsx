#!/usr/local/bin/fsharpi

open System;

let digits (n:int) =
  let rec aux (ds:int list) = function
    | 0  -> ds
    | n  -> aux ((n % 10)::ds) (n / 10)
  aux [] n

let lookAndSay (ds:int list) =
  let rec aux (r:int list) (n:int) (pd:int) = function
    | []   -> pd::n::r |> List.rev
    | h::t -> if h=pd then aux r (n+1) pd t else aux (pd::n::r) 1 h t
  aux [] 1 (List.head ds) (List.tail ds)

let f (n:int) =
  digits n |> Seq.unfold (fun (ds: int list) -> let ds' = ds |> lookAndSay
                                                Some(ds', ds'))
  
f 1113122113
|> Seq.skip 49
|> Seq.head
|> Seq.length
|> printfn "Length: %d" 
