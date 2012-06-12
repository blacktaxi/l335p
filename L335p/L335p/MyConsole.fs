namespace L335p

open System
open System.Collections.Generic

open Reader
open Parser
open Runner

module MyConsole =
        
    let Run args =

//        let testString2 = "(let ((x 5) (y (+ x 1))) (+ x y))"
//        let testString = "(if (= 5 5) (- 5 6))"
//
//        printfn "%A" (List.ofSeq (lexAll testString))

        printfn "%A" (
            //"(let ((func (lambda (a b) (+ a b)))) (func 2 2))"
            "(let ((fac (lambda (x) 
                            (if (= x 1) 
                                1 
                                (* (fac (- x 1)) x))))) 
                  (fac 30))
            "
//            "(let ((fac (lambda (x) (if (= x 1) 1 (* (fac (- x 1)) x))))) (+ 2 2))"
            |> readAll 
            |> List.ofSeq |> parseAll 
            |> Seq.map (eval builtins)
        )

        //printfn "%A" (testString |> lexString |> parse |> eval testScope)


        Console.ReadLine() |> ignore