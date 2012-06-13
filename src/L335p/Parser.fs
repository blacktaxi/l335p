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
    let rec parseOne (lexemes: Lexeme list) : SyntaxNode * Lexeme list =
        match lexemes with
        | OpenParen::rest -> 
            let rec read lexemes stack =
                match lexemes with
                | CloseParen::rest -> stack, rest
                | _ -> 
                    let node, restLexemes = parseOne lexemes in
                        read restLexemes (node :: stack)

            let stack, restLexemes = read rest [] in
                ListForm (List.rev stack), restLexemes
        | CloseParen::rest -> raise <| ParserException(sprintf "Unexpected ')' at %A" lexemes)
        | StringLiteral s::rest -> StringConstant s, rest
        | IntegerLiteral s::rest -> IntegerConstant s, rest
        | (Symbol s)::rest -> Reference s, rest
        | _ -> raise <| ParserException(sprintf "Don't know how to parse %A" lexemes)

    /// Returns a sequence of parsed lexemes.
    let rec parseAll lexemes =
        seq {
            if not (List.isEmpty lexemes) then
                let node, rest = parseOne lexemes in
                    yield node; yield! parseAll rest
        }
     