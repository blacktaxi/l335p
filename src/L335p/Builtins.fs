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
        | [ListForm parametersExpr; lambdaBody] ->

            let lambdaClosure = scope

            let parameterNames = List.map (fun e -> match e with | Reference n -> n | other -> raise <| ArgumentAssertionException("a reference", toStr other)) parametersExpr

            let withBoundParameters parameterValues scope =
                List.fold (fun currentScope (name, value) -> Map.add name value currentScope) scope (List.zip parameterNames parameterValues)

            fun scope nodes ->
                let args = List.map (eval scope) nodes
                let lambdaScope = withBoundParameters args lambdaClosure

                eval lambdaScope lambdaBody
            |> FunctionValue
        | other -> raise <| ArgumentAssertionException("(lambda (p1 p2 ...) body)", toStr other)

    let funcIf scope (nodes: FunctionArg list) =
        match nodes with
        // expect (if (condition) body else-body)
        | [condition; body; elseBody] ->
            match maybeForce <| eval scope condition with
            | BooleanValue true -> eval scope body
            | _ -> eval scope elseBody
        // expect (if (condition) body)
        | [condition; body] ->
            match maybeForce <| eval scope condition with
            | BooleanValue true -> eval scope body
            | _ -> NothingValue
        | other -> raise <| ArgumentAssertionException("(if (condition) body [else-body])", toStr other)

    /// Takes list of pairs of bindings, sequentially adds them to
    /// current scope, and then executes body in resulting scope.
    /// (let ((name1 value1) (name2 value2) ...) (body))
    let funcLet scope nodes =
        match nodes with
        | [ListForm bindings; body] ->
            let bindingfoldfn currentScope binding =
                match binding with
                | ListForm [nameExpr; valueExpr] ->
                    // evaluate the name. why? because names are not necessarily
                    // plain references (symbols) -- they can be a function calls
                    // that evaluate to references.
                    //let name = maybeForce <| eval currentScope nameExpr

                    // TODO need to evaluate the name! will have to introduce a new
                    // kind of value -- a ReferenceValue or something.
                    let name = nameExpr

                    match name with
                    | Reference n ->
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
                            ) |> DelayedValue
                    
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
            | [one] -> IntegerValue (- one)
            // binary
            | [first; second] -> IntegerValue (first - second)
            | other -> raise <| ArgumentAssertionException("(- x) or (- x y)", toStr other)

    let (NonMacro funcMul) =
        fun args -> IntegerValue (Seq.reduce (*) <| Seq.map expectIntegerValue args)

    let (NonMacro funcAdd) = 
        fun args -> IntegerValue (Seq.sum <| Seq.map expectIntegerValue args)

    let (NonMacro funcEquals) = 
        fun args ->
            match List.map expectIntegerValue args with
            | [left; right] ->
                BooleanValue (left = right)
            | other -> raise <| ArgumentAssertionException("(= int int)", toStr other)

    let (NonMacro funcPrint) = 
        fun args ->
            for arg in args do
                printfn ">>> %A" (maybeForce arg)

            NothingValue

    let defaultScope = 
        Map.ofList <| [
            // basic values
            "true", BooleanValue true;
            "false", BooleanValue false;
            "=", FunctionValue funcEquals;

            // arithmetic
            "+", FunctionValue funcAdd;
            "*", FunctionValue funcMul;
            "-", FunctionValue funcSub;

            // core functions
            "let", FunctionValue funcLet;
            "if", FunctionValue funcIf;
            "lambda", FunctionValue funcLambda;

            // etc
            "print", FunctionValue funcPrint;
        ]
