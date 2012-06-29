namespace Lang

open System
open Lexer
open Runner

module Reader =

    type ReaderException(msg) = inherit Exception(msg)

    /// Parses some lexemes from input list. Returns a syntax node that
    /// represents parsed lexemes and the rest of lexeme list.
    let rec readOne (lexemes: Lexeme seq) : L3Value * Lexeme seq =
        match Seq.destructure lexemes with
        | Some OpenParen, rest -> 
            let rec read lexemes stack =
                match Seq.head lexemes, Seq.tail lexemes with
                | CloseParen, rest -> stack, rest
                | _ -> 
                    let node, restLexemes = readOne lexemes in
                        read restLexemes (node :: stack)

            let stack, restLexemes = read rest [] in
                ListVal (List.rev stack), restLexemes
        | Some CloseParen, rest -> raise <| ReaderException(sprintf "Unexpected ')' at %A" lexemes)
        | Some (StringLiteral s), rest -> StringVal s, rest
        | Some (IntegerLiteral s), rest -> IntegerVal s, rest
        | Some (BooleanLiteral b), rest -> BooleanVal b, rest
        | Some (Name s), rest -> SymbolVal s, rest
        | other -> raise <| ReaderException(sprintf "Don't know how to read %A" other)

    /// Returns a sequence of parsed lexemes.
    let rec readAll (lexemes: Lexeme seq) =
        seq {
            if not (Seq.isEmpty lexemes) then
                let node, rest = readOne lexemes in
                    yield node; yield! readAll rest
        }
     