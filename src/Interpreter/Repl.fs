namespace R3p1

open System
open Lang

module Repl =

    let evalString scope input =
        input |> Lexer.lexAll |> Reader.readAll |> Seq.map (Runner.force << Runner.eval scope)
    
    let startREPL builtinScope =
        let prompt () =
            Console.Write("l3 - ")
            Console.ReadLine()

        let prompts = 
            Seq.takeWhile 
                (fun x -> (x <> "quit")) 
                (Seq.initInfinite (fun _ -> prompt()))
                //(seq { while true do yield prompt() })

        let evalAndPrint input =
            printfn "-> %A\n" (evalString builtinScope input)

        printfn "Welcome to R3p1. Type 'quit' to quit.\n\n"
        Seq.iter evalAndPrint prompts
    