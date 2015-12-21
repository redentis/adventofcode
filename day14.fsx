#!/usr/local/bin/fsharpi

open System
open System.IO
open System.Text.RegularExpressions

// --------------------------------------------------

let readLines (tr:TextReader) = Seq.unfold (fun (s:TextReader) -> match s.ReadLine() with | null -> None | t -> Some(t, s)) tr

// --------------------------------------------------

type competitor = string * int * int * int

type Event =
    | Fly of int * int
    | Rest of int

let events seconds ((name, speed, sprint, rest):competitor) =
    let rec aux es remaining =
        match Seq.head es with
            | Rest _   -> if remaining <= sprint then Fly(speed * remaining, remaining)::es
                          else aux (Fly(speed * sprint, sprint)::es) (remaining - sprint)
            | Fly(_,_) -> if remaining <= rest then Rest(remaining)::es
                          else aux (Rest(rest)::es) (remaining - rest)
    aux [Rest(0)] seconds

let distance second ((name, speed, sprint, rest):competitor) =
            
let totalDistance es = es |> Seq.sumBy (function | Fly(d,_) -> d | _ -> 0)    
        
let parse (s:string) :competitor option =
    let mc = Regex.Matches(s, "^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$")
    if mc.Count = 1 then
        let gc = mc.[0].Groups
        Some (gc.[1].Value, (int gc.[2].Value), (int gc.[3].Value), (int gc.[4].Value))
    else None

let race = 2503
        
Console.In
|> readLines
|> Seq.map parse
|> Seq.map (function | Some c          -> let (name, _, _, _) = c
                                          name,((events race c) |> totalDistance)
                     | None            -> "", 0)
|> Seq.maxBy (function _, d -> d)
|> printfn "%A"
