namespace R3p1

open System
open L335p

module Repl =

    let startREPL builtinScope =
        let prompt () =
            Console.Write("l3 - ")
            Console.ReadLine()

        let prompts = 
            Seq.takeWhile 
                (fun x -> (x <> "quit")) 
                (seq { while true do yield prompt() })

        let evalAndPrint input =
            printfn "-> %A\n" (
                input 
                |> Reader.readAll 
                |> List.ofSeq 
                |> Parser.parseAll 
                |> Seq.map (Runner.maybeForce << Runner.eval builtinScope)
            )

        printfn "Welcome to R3p1. Type 'quit' to quit.\n\n"
        Seq.iter evalAndPrint prompts
    