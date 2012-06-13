namespace L335p

open System

module Utils = 

    // TODO there should be a better way...
    let toStr x = sprintf "%A" x
    
    let rec takewhile p l =
        seq {
            match l with
            | x::xs when p x -> yield x; yield! takewhile p xs
            | _ -> []
        }
    
    let rec splitWhen pred l =
        match l with
        | [] -> ([], [])
        | x::xs -> 
            if pred x 
                then let left, right = splitWhen pred xs in
                        (x::left, right) 
                else ([], x::xs)

