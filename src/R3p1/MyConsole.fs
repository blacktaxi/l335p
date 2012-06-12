namespace R3p1

open System
open System.Collections.Generic

open L335p

module MyConsole =
        
    let Run args =

        printfn "%A" (Map.ofList [])
//        let testString2 = "(let ((x 5) (y (+ x 1))) (+ x y))"
//        let testString = "(if (= 5 5) (- 5 6))"
//
//        printfn "%A" (List.ofSeq (lexAll testString))

        printfn "%A" (
            //"(let ((func (lambda (a b) (+ a b)))) (func 2 2))"
            "(if (= 5 5) (- 5 6))"
//            "(let ((fac (lambda (x) 
//                            (if (= x 1) 
//                                1 
//                                (* (fac (- x 1)) x))))) 
//                  (fac 30))
//            "
//            "(let ((fac (lambda (x) (if (= x 1) 1 (* (fac (- x 1)) x))))) (+ 2 2))"
            //"((lambda (x y z) (+ x y z)) 1 2 3)"
            |> Reader.readAll 
            |> List.ofSeq |> Parser.parseAll 
            |> Seq.map (Runner.eval (Builtins.defaultScope))
        )

        //printfn "%A" (testString |> lexString |> parse |> eval testScope)

        Console.ReadLine() |> ignore