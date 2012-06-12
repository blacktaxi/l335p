namespace L335p

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Parser

module Runner =

    type EvaluationException = Exception

    type FunctionArg = SyntaxNode

    and InterpreterScope = Map<string, InterpreterValue>

    and InterpreterFunction = InterpreterScope -> FunctionArg list -> InterpreterValue

    and InterpreterValue =
    | IntegerValue of int
    | StringValue of string
    | BooleanValue of bool
    | NothingValue
    | DelayedValue of (unit -> InterpreterValue)
    | FunctionValue of InterpreterFunction

    /// Dereferences `name` in `scope`.
    let dereference name scope =
        match Map.tryFind name scope with
        | Some value -> value
        | None -> raise <| EvaluationException(sprintf "Reference %s not found in scope %A" name scope)

    /// Calls a defined function `func` with arguments `args` in `scope`.
    /// Return type is same as IntepreterFunction's.
    let callFunc (func: InterpreterFunction) scope (args: SyntaxNode list) =
        func scope args

    let maybeForce value =
        match value with
        | DelayedValue v -> v()
        | any -> any
               
    /// Evaluates given node in scope. Returns InterpreterValue.
    let eval (scope: InterpreterScope) (node: SyntaxNode): InterpreterValue =
        match node with
        | StringConstant value -> StringValue(value)
        | IntegerConstant value -> IntegerValue(Int32.Parse value)
        | Reference name -> dereference name scope
        | ListForm (Reference name::args) ->
            match maybeForce (dereference name scope) with
            | FunctionValue func -> callFunc func scope args
            | other -> raise <| EvaluationException(sprintf "%s is not a function at %A -- it is a %A" name node other)
        | _ -> raise <| EvaluationException(sprintf "Don't know how to evaluate %A" node)
                
    /// Creates a lazily evaluated function argument from a syntax node.
    let rec makeDelayedValue scope node =
        let value: ref<Option<InterpreterValue>> = ref None
        let force () =
            match !value with
            | None -> value := Some (eval scope node); (!value).Value
            | Some v -> v
        in
            DelayedValue force

//    /// Evaluates function argument.
//    let getArgValue (scope: InterpreterScope) (arg: FunctionArg): InterpreterValue = eval scope arg
    let (|MaybeForced|) value =
        match value with
        | DelayedValue v -> v()
        | any -> any

    let getIntegerValue scope name =
        match eval scope name with
        | MaybeForced (IntegerValue i) -> i
        | other -> raise <| EvaluationException(sprintf "Expected integer value in %A, instead got %A" name other)

//    let getValue<'valueType> unbox scope name =
//        match eval scope name with
//        | MaybeForced i -> match i with
//                           | 'valueType v -> v
//        | other -> raise <| EvaluationException(sprintf "Expected value of type %A in %A, instead got %A" "hui" name other)

    /// "Built-in" functions and values, special forms, etc.
    let builtins =
        [
            "true", BooleanValue true;

            "false", BooleanValue false;

            "=", FunctionValue <|
                fun scope args ->
                    match args with
                    | [left; right] ->
                        BooleanValue ((getIntegerValue scope left) = (getIntegerValue scope right))
                    | other -> raise <| EvaluationException(sprintf "Expected two integer arguments to =, instead got %A" other)

            "+", FunctionValue <| 
                fun scope args ->
                    IntegerValue (Seq.sum <| Seq.map (getIntegerValue scope) args);

            "*", FunctionValue <| 
                fun scope args ->
                    IntegerValue (Seq.reduce (*) <| Seq.map (getIntegerValue scope) args);

            "-", FunctionValue <|
                fun scope args ->
                    match args with
                    // unary
                    | [one] -> IntegerValue (- (getIntegerValue scope one))
                    // binary
                    | [first; second] -> IntegerValue ((getIntegerValue scope first) - (getIntegerValue scope second))
                    | other -> raise <| EvaluationException(sprintf "Unsupported argument list for -: %A" other)

            "let", FunctionValue <|
                fun scope args ->
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

            "if", FunctionValue <|
                fun scope args ->
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

            "lambda", FunctionValue <|
                fun scope args ->
                    match args with
                    | [ListForm parameters; body] ->
                        FunctionValue <|
                            let closure = scope

                            /// Creates a new map, which consists of keys and values from `map` appended with
                            /// keys and values from second map `withWhat`.
                            let update map withWhat = Map.fold (fun acc key value -> Map.add key value acc) map withWhat

                            /// A list of function parameter names.
                            let paramNames = List.map (fun p -> match p with 
                                                                | Reference name -> name
                                                                | other -> raise <| EvaluationException(sprintf "Expected a symbol, instead got %A" other))
                                                        parameters

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
                                        
                    | other -> raise <| EvaluationException(sprintf "Expected (lambda (args) body), instead got %A" other)

        ] |> Map.ofList

