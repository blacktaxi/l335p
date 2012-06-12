namespace L335p

open Parser
open Runner

module Builtins =

    let funcLambda scope args =
        match args with
        | [ListForm parameters; body] ->
            let closure = scope

            /// Creates a new map, which consists of keys and values from `map` appended with
            /// keys and values from second map `withWhat`.
            let update map withWhat = Map.fold (fun acc key value -> Map.add key value acc) map withWhat

            let parameterName p =
                match p with 
                | Reference name -> name
                | other -> raise <| EvaluationException(sprintf "Expected a symbol, instead got %A" other)
            in
            /// A list of function parameter names.
            let paramNames = List.map parameterName parameters

            /// Creates a <name, value> map for positional arguments, "binding" each argument value to
            /// respective parameter name, positionally.
            let positionalArgsMap names values = Map.ofList <| List.zip names values

            /// Binds given `values` to their respective positional parameter names in `scope`.
            /// Returns new scope with all the arguments bound.
            let bindArgs (scope: InterpreterScope) values = 
                let argValues = List.map (makeDelayedValue scope) values in
                update scope (positionalArgsMap paramNames argValues)

            fun scope args ->
                eval (bindArgs closure args) body

            |> FunctionValue
                                        
        | other -> raise <| EvaluationException(sprintf "Expected (lambda (args) body), instead got %A" other)

    let funcIf scope args =
        match args with
        // expect (if (condition) body else-body)
        | [condition; body; elseBody] ->
            match eval scope condition with
            | BooleanValue true -> eval scope body
            | _ -> eval scope elseBody
        | [condition; body] ->
            match eval scope condition with
            | BooleanValue true -> eval scope body
            | _ -> NothingValue
        | other -> raise <| EvaluationException(sprintf "Expected (if (condition) body else-body), instead got %A" other)

    let funcLet scope args =
        match args with
        // expect (let (bindings) body)
        | [ListForm bindings; body] ->
            // fold bindings
            let finalScope =
                /// Takes a binding expression, evaluates it's second element (value expression)
                /// in `currentScope` and returns a new scope where the binding expression's first 
                /// element (name) is bound to the evaluated value.
                let bindOne (currentScope: InterpreterScope) (bindingExpr: SyntaxNode) =
                    match bindingExpr with
                        // expect (name value)
                        | ListForm [Reference name; expr] ->
                            let selfRef = ref None
                            /// Add a DelayedValue "stub" to "self" to currentScope that raises and 
                            /// exception upon forcing. The value doesn't exist before this binding is 
                            /// complete. After that it is OK to force the value out of it.
                            let withGhostReference =
                                Map.add 
                                    name
                                    (DelayedValue 
                                        (fun () ->
                                            match !selfRef with
                                            | Some x -> x
                                            | _ -> raise <| EvaluationException(sprintf "Can not evaluate %A before it is fully defined." name)))
                                    currentScope

                            let evaluated = eval withGhostReference expr
                            selfRef := Some evaluated
                            // evaluate binding value and add it to current scope
                            Map.add name evaluated currentScope
                        | other -> raise <| EvaluationException(sprintf "Expected (name value) in a let binding, instead got %A" other)
                in
                    List.fold bindOne scope bindings
            in
                eval finalScope body
        | other -> raise <| EvaluationException(sprintf "Expected (let bindings body), instead got %A" other)
        
    let funcSub scope args =
        match args with
        // unary
        | [one] -> IntegerValue (- (getIntegerValue scope one))
        // binary
        | [first; second] -> IntegerValue ((getIntegerValue scope first) - (getIntegerValue scope second))
        | other -> raise <| EvaluationException(sprintf "Unsupported argument list for -: %A" other)

    let funcMul scope args =
        IntegerValue (Seq.reduce (*) <| Seq.map (getIntegerValue scope) args)

    let funcAdd scope args =
        IntegerValue (Seq.sum <| Seq.map (getIntegerValue scope) args)

    let funcEquals scope args =
        match args with
        | [left; right] ->
            BooleanValue ((getIntegerValue scope left) = (getIntegerValue scope right))
        | other -> raise <| EvaluationException(sprintf "Expected two integer arguments to =, instead got %A" other)

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
        ]
