namespace L335p

open System
open System.Collections.Generic
open System.Text.RegularExpressions

module Reader =
    type Lexeme =
        | OpenParen
        | CloseParen
        //| Quote
        //| QuasiQuote
        //| Unquote
        //| UnquoteSplicing
        | StringLiteral of string
        | IntegerLiteral of string
        | Symbol of string

    type LexerException = Exception
        
    /// Tokenizes some characters from 'input', returning an Option<Lexeme> and the
    /// rest of the string.
    let readOne input: Option<Lexeme> * string =
        let (|Re|_|) pattern input =
            let m = Regex.Match(input, sprintf "^(?<parsed>%s)(?<rest>.*)$" pattern) in
                if m.Success
                    then Some (m.Groups.Item("parsed").Value, m.Groups.Item("rest").Value)
                    else None

        match input with
            | Re "[\r\n\s]+"              (p, r) -> None, r
            | Re "\("                     (p, r) -> Some OpenParen, r
            | Re "\)"                     (p, r) -> Some CloseParen, r
            | Re "\".*?\""                (p, r) -> Some (StringLiteral p), r
            | Re "\d+"                    (p, r) -> Some (IntegerLiteral p), r
            | Re "[^\(\)\s\d][^\(\)\s]*"  (p, r) -> Some (Symbol p), r
            | _ -> raise <| LexerException(sprintf "Couldn't lex this: %s" input)

    /// Returns a sequence of Lexemes tokenized from string
    let rec readAll input = 
        seq {
            if not (String.IsNullOrEmpty input) then 
                match readOne input with
                | Some lexeme, rest -> 
                    yield lexeme 
                    yield! readAll rest
                | None, rest -> yield! readAll rest
        }
