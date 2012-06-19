namespace L335p

open System
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
        | IntegerLiteral of int
        | BooleanLiteral of bool
        | Name of string

    type ReaderException(msg) = inherit Exception(msg)
        
    /// Tokenizes some characters from 'input', returning an Option<Lexeme> and the
    /// rest of the string.
    let readOne input: Option<Lexeme> * string =
        let (|Re|_|) pattern input =
            let m = Regex.Match(input, pattern) in
                if m.Success
                    then Some (m.Groups.Item("value").Value, m.Groups.Item("rest").Value)
                    else None

        /// Creates a generic term matching pattern.
        let genP s = sprintf "^(?<value>%s)(?<rest>.*)$" s
        
        match input with
        // Ignored stuff
        | Re (genP ";.*[\r\n]?") // comment
            (p, r) -> None, r
        | Re (genP "[\r\n\s]+") // whitespace
            (p, r) -> None, r

        // parens
        | Re (genP "\(") (p, r) -> Some OpenParen, r
        | Re (genP "\)") (p, r) -> Some CloseParen, r

        // literals
        | Re "^\"(?<value>.*?)\"(?<rest>.*)$" // String
            (p, r) -> Some (StringLiteral p), r
        | Re (genP "-?\d+") // Integer
            (p, r) -> Some (IntegerLiteral (Int32.Parse p)), r
        | Re (genP "~t") (p, r) -> Some (BooleanLiteral true), r
        | Re (genP "~f") (p, r) -> Some (BooleanLiteral false), r

        // Symbols
        | Re "^`(?<value>[^\r\n]*)`(?<rest>.*)$" // Quoted (a-la F#)
            (p, r) -> Some (Name p), r
        | Re (genP "[^\(\)\s\d][^\(\)\s]*") // Regular. Can't contain special chars like parens, and can't start with a digit.
            (p, r) -> Some (Name p), r
        
        //
        | _ -> raise <| ReaderException(sprintf "Couldn't read \"%s\"" input)

    /// Returns a sequence of Lexemes tokenized from string
    let rec readAll input: Lexeme seq = 
        seq {
            if not (String.IsNullOrEmpty input) then 
                match readOne input with
                | Some lexeme, rest -> 
                    yield lexeme 
                    yield! readAll rest
                | None, rest -> yield! readAll rest
        }
