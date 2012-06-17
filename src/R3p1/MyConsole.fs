namespace R3p1

open System
open System.Collections.Generic

open L335p

module MyConsole =
        
    let Run args =

        let evalSingleExpr scope input =
            match input |> Reader.readAll |> Parser.parseAll |> List.ofSeq with
            | [expr] -> Runner.eval scope expr
            | _ -> failwith "Should be a single expression."

        let testStuff =
            (Map.ofList [
                "t1", "(+ 2 2)";                
                "fac", "(let ((fac (lambda fac (x) (if (= x 1) 1 (* (fac (- x 1)) x))))) fac)";
            ]) |> Map.map (fun k v -> evalSingleExpr Builtins.defaultScope v) 
        
        Repl.startREPL (Map.update Builtins.defaultScope testStuff)

//        printfn "%A" (
//            //"(= (+ 1 500) 299)"
//            //"(if (= 2 2) (print 1))"
//            //"(print 5)"
//            //"(let ((func (lambda (a b) (+ a b)))) (func 2 2))"
//            //"(let ((f1 (lambda (x) (+ x 1))) (f2 (lambda (x) (* x 2))) (f3 (lambda (x) (f1 (f2 x))))) (f3 1))"
////            "(if (= 5 5) (- 5 6))"
////            "(let ((fac (lambda (x) 
////                            (if (= x 1) 
////                                1 
////                                (* (fac (- x 1)) x))))) 
////                  (fac 30))
////            "
//            "(let ((fac (lambda (x) (if (= x 1) 1 (* (fac (- x 1)) x))))) (fac 30))"
//            //"((lambda (x y z) (+ x y z)) 1 2 3)"
//            |> Reader.readAll 
//            |> List.ofSeq |> Parser.parseAll 
//            |> Seq.map (Runner.maybeForce << Runner.eval (Builtins.defaultScope))
//        )

        //printfn "%A" (testString |> lexString |> parse |> eval testScope)
