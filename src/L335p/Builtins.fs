namespace L335p

open Parser
open Runner
open Utils

module Builtins =

    type ArgumentAssertionException(expected, actual) =
        inherit AssertionException(expected, actual)

//    let funcLambda scope nodes =
//        match nodes with
//        | [ListForm parameters; body] ->
//            let closure = scope
//
//            /// Creates a new map, which consists of keys and values from `map` appended with
//            /// keys and values from second map `withWhat`.
//            let update map withWhat = Map.fold (fun acc key value -> Map.add key value acc) map withWhat
//
//            let parameterName p =
//                match p with 
//                | Reference name -> name
//                | other -> raise <| EvaluationException(sprintf "Expected a symbol, instead got %A" other)
//            in
//            /// A list of function parameter names.
//            let paramNames = List.map parameterName parameters
//
//            /// Creates a <name, value> map for positional arguments, "binding" each argument value to
//            /// respective parameter name, positionally.
//            let positionalArgsMap names values = Map.ofList <| List.zip names values
//
//            /// Binds given `values` to their respective positional parameter names in `scope`.
//            /// Returns new scope with all the arguments bound.
//            let bindArgs (scope: InterpreterScope) values = 
//                // lambda takes evaluated code as arguments, so we evaluate it.
//                let argValues = List.map (eval scope) values in
//                update scope (positionalArgsMap paramNames argValues)
//
//            fun (scope: InterpreterScope) (args: FunctionArg list) ->
//                eval (bindArgs closure args) body
//
//            |> FunctionValue
//                                        
//        | other -> raise <| EvaluationException(sprintf "Expected (lambda (args) body), instead got %A" other)

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

//    let funcLet scope nodes =
//        match nodes with
//        // expect (let (bindings) body)
//        | [ListForm bindings; body] ->
//            // fold bindings
//            let finalScope =
//                /// Takes a binding expression, evaluates it's second element (value expression)
//                /// in `currentScope` and returns a new scope where the binding expression's first 
//                /// element (name) is bound to the evaluated value.
//                let bindOne (currentScope: InterpreterScope) (bindingExpr: SyntaxNode) =
//                    match bindingExpr with
//                        // expect (name value)
//                        | ListForm [Reference name; expr] ->
//                            let selfRef = ref None
//                            /// Add a DelayedValue "stub" to "self" to currentScope that raises and 
//                            /// exception upon forcing. The value doesn't exist before this binding is 
//                            /// complete. After that it is OK to force the value out of it.
//                            let withGhostReference =
//                                Map.add 
//                                    name
//                                    (DelayedValue 
//                                        (fun () ->
//                                            match !selfRef with
//                                            | Some x -> x
//                                            | _ -> raise <| EvaluationException(sprintf "Can not evaluate %A before it is fully defined." name)))
//                                    currentScope
//
//                            let evaluated = eval withGhostReference expr
//                            selfRef := Some evaluated
//                            // evaluate binding value and add it to current scope
//                            let evaluated = eval currentScope expr in
//                            Map.add name evaluated currentScope
//                        | other -> raise <| EvaluationException(sprintf "Expected (name value) in a let binding, instead got %A" other)
//                in
//                    List.fold bindOne scope bindings
//            in
//                eval finalScope body
//        | other -> raise <| EvaluationException(sprintf "Expected (let bindings body), instead got %A" other)
        
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
//            "let", FunctionValue funcLet;
            "if", FunctionValue funcIf;
//            "lambda", FunctionValue funcLambda;

            // etc
            "print", FunctionValue funcPrint;
        ]
