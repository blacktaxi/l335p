namespace L335p

open System
open Reader
open Runner
open Utils

type ArgumentAssertionException(expected, actual) =
    inherit AssertionException(expected, actual)

module CoreFunctions =

    let funcLambda scope nodes =
        let makeLambda closure parametersExpr body =
            // Parameter name strings, from given lambda signature.
            let parameterNames = 
                List.map 
                    (fun e -> 
                        match e with 
                        | SymbolVal n -> n 
                        | other -> raise <| ArgumentAssertionException("a symbol", toStr other)) 
                    parametersExpr

            // Maps provided arguments to respective parameter names in given scope
            let withBoundParameters parameterValues scope =
                List.fold 
                    (fun currentScope (name, value) -> 
                        Map.add name value currentScope) 
                    closure 
                    (List.zip parameterNames parameterValues)

            // A scope where the lambda was defined. Not to confuse with the scope
            // where it is executed.
            let lambdaClosure = scope
            
            // the result function
            fun scope nodes ->
                // Here we evaluate parameter values -- the arguments.
                // Lazy evaluation function is used to delay the actual evaluation
                // until the argument is actually used.
                // TODO it's not yet clearly defined how is this supposed to work...
                let args = List.map (lazyEval scope) nodes

                // Add arguments to the scope.
                let lambdaScope = withBoundParameters args closure

                eval lambdaScope body

        match nodes with
        | [SymbolVal lambdaName; ListVal parametersExpr; lambdaBody] ->
            // create a "ghost" value of this same binding.
            // this is needed so the binding can refer to itself when it is
            // evaluated. there is probably a better way to do this.
            // TODO I think this doesn't really work :(
            let realValue = ref None
            let ghostValue =
                lazy (
                    match !realValue with 
                    | Some v -> v 
                    | None -> raise <| EvaluationException(sprintf "The value of %s can not be evaluated at this time." lambdaName)
                ) |> DelayedVal
                    
            // create the actual lambda value
            let theLambda = 
                makeLambda 
                    (Map.add lambdaName ghostValue scope) 
                    parametersExpr 
                    lambdaBody 
                    |> FunctionVal

            // change the previously defined reference to point to the
            // lambda we've just created
            realValue := Some theLambda

            theLambda
        | [ListVal parametersExpr; lambdaBody] ->
            makeLambda 
                scope 
                parametersExpr 
                lambdaBody 
                |> FunctionVal
        | other -> raise <| ArgumentAssertionException("(lambda (p1 p2 ...) body)", toStr other)

    let funcIf scope (nodes: LeFunctionArg list) =
        match nodes with
        // expect (if (condition) body else-body)
        | [condition; body; elseBody] ->
            match eval scope condition with
            | BooleanVal true -> eval scope body
            | _ -> eval scope elseBody
        // expect (if (condition) body)
        | [condition; body] ->
            match eval scope condition with
            | BooleanVal true -> eval scope body
            | _ -> NothingVal
        | other -> raise <| ArgumentAssertionException("(if (condition) body [else-body])", toStr other)

    /// Takes list of pairs of bindings, sequentially adds them to
    /// current scope, and then executes body in resulting scope.
    /// (let ((name1 value1) (name2 value2) ...) (body))
    let funcLet scope nodes =
        match nodes with
        | [ListVal bindings; body] ->
            let bindingfoldfn currentScope binding =
                match binding with
                | ListVal [nameExpr; valueExpr] ->
                    // evaluate the name. why? because names are not necessarily
                    // plain references (symbols) -- they can be a function calls
                    // that evaluate to references.
                    //let name = maybeForce <| eval currentScope nameExpr

                    // TODO need to evaluate the name! will have to introduce a new
                    // kind of value -- a ReferenceValue or something.
                    let name = nameExpr

                    match name with
                    | SymbolVal n ->
                        // lazy eval because bindings should be lazy.
                        Map.add n (lazyEval currentScope valueExpr) currentScope
                    | other -> raise <| ArgumentAssertionException("a reference", toStr other)
                | other -> raise <| ArgumentAssertionException("(name value)", toStr other)
            
            let scopeWithBindings = List.fold bindingfoldfn scope bindings

            eval scopeWithBindings body
        | other -> raise <| ArgumentAssertionException("(let ((name value) ...) body)", toStr other)

    let all =
        Map.ofList [
            "let", funcLet;
            "if", funcIf;
            "lambda", funcLambda;
        ]

module ArithmeticFunctions =

    /// Wraps a function that takes InterpreterValues as args into InterpreterFunction.
    let nonMacro func = (fun scope nodes -> func (List.map (eval scope) nodes))
    let (|NonMacro|) func = (fun scope nodes -> func (List.map (eval scope) nodes))

    let (NonMacro funcSub) =
        fun args ->
            match List.map expectIntegerValue args with
            // unary
            | [one] -> IntegerVal (- one)
            // binary
            | [first; second] -> IntegerVal (first - second)
            | other -> raise <| ArgumentAssertionException("(- x) or (- x y)", toStr other)

    let (NonMacro funcMul) =
        fun args -> IntegerVal (Seq.reduce (*) <| Seq.map expectIntegerValue args)

    let (NonMacro funcAdd) = 
        fun args -> IntegerVal (Seq.sum <| Seq.map expectIntegerValue args)

    let (NonMacro funcEquals) = 
        fun args ->
            match List.map expectIntegerValue args with
            | [left; right] ->
                BooleanVal (left = right)
            | other -> raise <| ArgumentAssertionException("(= int int)", toStr other)
    
    let all =
        Map.ofList [
            "=", funcEquals;
            "+", funcAdd;
            "*", funcMul;
            "-", funcSub;
        ]
    
module IOFunctions =

    let (|NonMacro|) func = (fun scope nodes -> func (List.map (eval scope) nodes))

    let (NonMacro funcPrint) = 
        fun args ->
            for arg in args do
                printfn ">>> %A" (force arg)

            NothingVal

    let all =
        Map.ofList [
            "print", funcPrint;
        ]

module ListFunctions =

    let funcList scope nodes =
        ListVal <| List.map (lazyEval scope) nodes
    
    let funcCons scope nodes =
        match nodes with
        | [head; tail] ->
            match eval scope tail with
            | ListVal l -> ListVal (eval scope head :: l)
            | other -> raise <| ArgumentAssertionException("a list", toStr other)
        | other -> raise <| ArgumentAssertionException("(cons value a-list)", toStr other)

    let funcFirst scope nodes =
        match nodes with
        | [alist] ->
            match eval scope alist with
            | ListVal l -> List.head l
            | other -> raise <| ArgumentAssertionException("a list", toStr other)
        | other -> raise <| ArgumentAssertionException("(first a-list)", toStr other)
    
    let funcRest scope nodes =
        match nodes with
        | [alist] ->
            match eval scope alist with
            | ListVal l -> ListVal (List.tail l)
            | other -> raise <| ArgumentAssertionException("a list", toStr other)
        | other -> raise <| ArgumentAssertionException("(rest a-list)", toStr other)

    let all = 
        Map.ofList [
            "list", funcList;
            "cons", funcCons;
            "first", funcFirst;
            "rest", funcRest;
        ]
    
module Builtins =
        
    let defaultScope =
        Map.merge
            (Map.map 
                (fun _ v -> FunctionVal v)
                (Map.merge Map.empty [CoreFunctions.all; ArithmeticFunctions.all; ListFunctions.all; IOFunctions.all]))
            []
            //Map(["true", BooleanVal true; "false", BooleanVal false;])
