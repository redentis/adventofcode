#!/usr/local/bin/fsharpi

open System;
open System.Security.Cryptography;
open System.Text;

let keySequence key =
  let m = MD5.Create()
  Seq.unfold(fun i -> let source = sprintf "%s%u" key i
                      let md5 = source
                                |> Encoding.ASCII.GetBytes
                                |> m.ComputeHash
                                |> Seq.collect (sprintf "%02x")
                                |> String.Concat
                      Some((i, md5), i+1L)) 0L
  
keySequence "yzbqklnj"
|> Seq.find (fun (_, m) -> m.[0..5] = "000000")
|> printfn "%A"
