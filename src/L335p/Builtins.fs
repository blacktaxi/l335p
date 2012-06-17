namespace L335p

open System
open Parser
open Runner
open Utils

module Builtins =

    type ArgumentAssertionException(expected, actual) =
        inherit AssertionException(expected, actual)

    let funcLambda scope nodes =
        match nodes with
        | [ListVal parametersExpr; lambdaBody] ->

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
                    scope 
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
                let lambdaScope = withBoundParameters args lambdaClosure

                eval lambdaScope lambdaBody
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
                        // create a "ghost" value of this same binding.
                        // this is needed so the binding can refer to itself when it is
                        // evaluated. there is probably a better way to do this.
                        // TODO I think this doesn't really work :(
                        let realValue = ref None
                        let ghostValue =
                            lazy (
                                match !realValue with 
                                | Some v -> v 
                                | None -> raise <| EvaluationException(sprintf "The value of %s is not known yet (not evaluated), and therefore can not be referenced." n)
                            ) |> DelayedVal
                    
                        // don't force the binding value -- we don't know if we
                        // need it yet.
                        let value = eval (Map.add n ghostValue currentScope) valueExpr
                        realValue := Some value

                        Map.add n value currentScope
                    | other -> raise <| ArgumentAssertionException("a reference", toStr other)
                | other -> raise <| ArgumentAssertionException("(name value)", toStr other)
            
            let scopeWithBindings = List.fold bindingfoldfn scope bindings

            eval scopeWithBindings body
        | other -> raise <| ArgumentAssertionException("(let ((name value) ...) body)", toStr other)
        
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

    let (NonMacro funcPrint) = 
        fun args ->
            for arg in args do
                printfn ">>> %A" (force arg)

            NothingVal

    let defaultScope = 
        Map.ofList <| [
            // basic values
            "true", BooleanVal true;
            "false", BooleanVal false;
            "=", FunctionVal funcEquals;

            // arithmetic
            "+", FunctionVal funcAdd;
            "*", FunctionVal funcMul;
            "-", FunctionVal funcSub;

            // core functions
            "let", FunctionVal funcLet;
            "if", FunctionVal funcIf;
            "lambda", FunctionVal funcLambda;

            // etc
            "print", FunctionVal funcPrint;
        ]
