﻿namespace L335p

open System

module List =

    let rec takeWhile p l =
        seq {
            match l with
            | x::xs when p x -> yield x; yield! takeWhile p xs
            | _ -> yield! Seq.empty // should be a better way...
        }

module Seq =

    let tail s = Seq.skip 1 s

    let destructure s =
        if Seq.isEmpty s then (None, Seq.empty) else (Some <| Seq.head s, tail s)

module Map =

    /// Creates a new map, which consists of keys and values from `map` appended with
    /// keys and values from second map `withWhat`.
    let update map withWhat = Map.fold (fun acc key value -> Map.add key value acc) map withWhat

module Utils = 

    // TODO there should be a better way...
    let toStr x = sprintf "%A" x
    
    let rec splitWhen pred l =
        match l with
        | [] -> ([], [])
        | x::xs -> 
            if pred x 
                then let left, right = splitWhen pred xs in
                        (x::left, right) 
                else ([], x::xs)

