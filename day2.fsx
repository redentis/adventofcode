#!/usr/local/bin/fsharpi

open System;

let splitDimensionString (s:string) = s.Split([|'x'|])

let rec readDimensions () =
  Console.In
  |> Seq.unfold (fun sr ->
              match sr.ReadLine() with
              | null -> None
              | line -> let ds = line |> splitDimensionString |> Array.map int
                        if ds.Length = 3 then Some ((ds.[0], ds.[1], ds.[2]), sr) else None)

let areaSides (h, w, l) = [ h*w; w*l; l*h ]

let perimeterSides (h, w, l) = [ 2*(h+w); 2*(w+l); 2*(l+h) ]

let volume (h, w, l) = h * w * l

let packageSurfaceArea ds =
  areaSides ds
  |> Seq.reduce (+)
  |> (*) 2

let packageWrapping ds =
  packageSurfaceArea ds + Seq.min (areaSides ds)

let packageRibbon ds =
  Seq.min (perimeterSides ds) + (volume ds)

let packages = readDimensions() |> Seq.cache

packages
|> Seq.map packageWrapping
|> Seq.reduce (+)
|> printfn "Wrapping: %d"

packages
|> Seq.map packageRibbon
|> Seq.reduce (+)
|> printfn "Ribbon: %d"
