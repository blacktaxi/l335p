namespace R3p1

open System
open System.Collections.Generic

open L335p

module MyConsole =
        
    let Run args =

        Repl.startREPL ()

        printfn "%A" (
            //"(= (+ 1 500) 299)"
            "(if (= 2 2) (print 1))"
            //"(print 5)"
            //"(let ((func (lambda (a b) (+ a b)))) (func 2 2))"
            //"(let ((f1 (lambda (x) (+ x 1))) (f2 (lambda (x) (* x 2))) (f3 (lambda (x) (f1 (f2 x))))) (f3 1))"
//            "(if (= 5 5) (- 5 6))"
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
            |> Seq.map (Runner.maybeForce << Runner.eval (Builtins.defaultScope))
        )

        //printfn "%A" (testString |> lexString |> parse |> eval testScope)

        Console.ReadLine() |> ignore