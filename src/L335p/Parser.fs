namespace L335p

open System
open Reader

module Parser =

    type SyntaxNode =
    | IntegerConstant of int
    | StringConstant of string
    | Reference of string
    | ListForm of SyntaxNode list

    type ParserException(msg) = inherit Exception(msg)

    /// Parses some lexemes from input list. Returns a syntax node that
    /// represents parsed lexemes and the rest of lexeme list.
    let rec parseOne (lexemes: Lexeme seq) : SyntaxNode * Lexeme seq =
        match Seq.destructure lexemes with
        | Some OpenParen, rest -> 
            let rec read lexemes stack =
                match Seq.head lexemes, Seq.tail lexemes with
                | CloseParen, rest -> stack, rest
                | _ -> 
                    let node, restLexemes = parseOne lexemes in
                        read restLexemes (node :: stack)

            let stack, restLexemes = read rest [] in
                ListForm (List.rev stack), restLexemes
        | Some CloseParen, rest -> raise <| ParserException(sprintf "Unexpected ')' at %A" lexemes)
        | Some (StringLiteral s), rest -> StringConstant s, rest
        | Some (IntegerLiteral s), rest -> IntegerConstant s, rest
        | Some (Symbol s), rest -> Reference s, rest
        | _, _ -> raise <| ParserException(sprintf "Don't know how to parse %A" lexemes)

    /// Returns a sequence of parsed lexemes.
    let rec parseAll (lexemes: Lexeme seq) =
        seq {
            if not (Seq.isEmpty lexemes) then
                let node, rest = parseOne lexemes in
                    yield node; yield! parseAll rest
        }
     