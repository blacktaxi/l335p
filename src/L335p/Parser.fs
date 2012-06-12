namespace L335p

open System
open Reader

module Parser =
    type SyntaxNode =
    | IntegerConstant of string
    | StringConstant of string
    | Reference of string
    | ListForm of SyntaxNode list

//    let rec takewhile p l =
//        seq {
//            match l with
//            | x::xs when p x -> yield x; yield! takewhile p xs
//            | _ -> []
//        }
    
//    let rec splitWhen pred l =
//        match l with
//        | [] -> ([], [])
//        | x::xs -> 
//            if pred x 
//                then let left, right = splitWhen pred xs in
//                        (x::left, right) 
//                else ([], x::xs)

    type ParserException = Exception

    /// Parses some lexemes from input list. Returns a syntax node that
    /// represents parsed lexemes and the rest of lexeme list.
    let rec parseOne (lexemes: Lexeme list) : SyntaxNode * Lexeme list =
        match lexemes with
        | OpenParen::rest -> 
//            let mutable r = rest
//            let mutable stack = []
//
//            while List.nth r 0 <> CloseParen do
//                let x, y = parseOne r
//                stack <- stack @ [x]
//                r <- y
//            ListForm stack, r
            let rec read lexemes stack =
                match lexemes with
                | CloseParen::rest -> stack, rest
                | _ -> 
                    let nodes, restLexemes = parseOne lexemes in
                        read restLexemes (stack @ [nodes])

            let stack, restLexemes = read rest [] in
                ListForm stack, restLexemes
        | CloseParen::rest -> raise <| ParserException(sprintf "Unexpected ')' at %A" lexemes)
        | StringLiteral s::rest -> StringConstant s, rest
        | IntegerLiteral s::rest -> IntegerConstant s, rest
        | (Symbol s)::rest -> Reference s, rest
        | _ -> raise <| ArgumentException(sprintf "Invalid input: %A" lexemes)

    /// Returns a sequence of parsed lexemes.
    let rec parseAll lexemes =
        seq {
            if not (List.isEmpty lexemes) then
                let node, rest = parseOne lexemes in
                    yield node; yield! parseAll rest
        }
     