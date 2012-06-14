namespace L335p

open System
open Parser
open Utils

module Runner =

    // TODO add evaluation context information to errors
    type EvaluationException(msg) = inherit Exception(msg)

    type InvalidReferenceException(name, scope) =
        inherit EvaluationException(sprintf "Reference \"%s\" was not found in scope %s" name scope)

    type AssertionException(expected, actual) =
        inherit EvaluationException(sprintf "Expected %s, instead got %s" expected actual)

    type TypeAssertionException(expectedType, actualValue) =
        inherit AssertionException(sprintf "a value of type %s" expectedType, actualValue)

    /// Code as data!
    type FunctionArg = SyntaxNode

    /// Interpreter scope is a map from name (symbol) to InterpreterValue
    and InterpreterScope = Map<string, InterpreterValue>

    /// A function that can behave like what is called a "macro" in Lisp --
    /// it takes arguments unevaluated -- raw SyntaxNode's.
    and InterpreterFunction = InterpreterScope -> FunctionArg list -> InterpreterValue

    /// A value that can be evaluated or passed as function argument.
    and InterpreterValue =
    /// A lazily-evaluated value.
    | DelayedValue of Lazy<InterpreterValue>
    | FunctionValue of InterpreterFunction
    | NothingValue
    | IntegerValue of int
    | StringValue of string
    | BooleanValue of bool
    //| ReferenceValue of string
    // TODO consider doing the following:
    //| DelayedValue of InterpreterScope * SyntaxNode
    // This will allow to track closures explicitly.
    // TODO do a NumericValue of double (or something better?) instead?

    /// Dereferences `name` in `scope`.
    let dereference name scope =
        match Map.tryFind name scope with
        | Some value -> value
        | None -> raise <| InvalidReferenceException(name, toStr scope)

    /// Forces a DelayedValue evaluation until it evaluates to instant
    /// value. If the argument is not DelayedValue, does nothing.
    /// Never use this to return values. Only force DelayedValue evaluation
    /// at points where you actually need that value.
    let rec maybeForce (value: InterpreterValue): InterpreterValue =
        match value with
        | DelayedValue v -> maybeForce (v.Force())
        | any -> any

    //let maybeForce value = let (MaybeForced v) = value in v

    let expectIntegerValue x = 
        match maybeForce x with 
        | IntegerValue v -> v 
        | other -> raise <| TypeAssertionException("integer", toStr other) 
               
    /// Evaluates given node in scope. Returns InterpreterValue.
    let rec eval (scope: InterpreterScope) (node: SyntaxNode): InterpreterValue = 
        /// Calls a defined function `func` with arguments `args` in `scope`.
        /// Return type is same as IntepreterFunction's.
        let callFunc (func: InterpreterFunction) scope (args: SyntaxNode list) =
            // We don't want to evaluate function arguments here because some
            // functions might want the syntax tree, not the value.
            func scope args

        // TODO probably can make constant and reference evaluation eager without
        // any consequences
        lazy (
            match node with
            | StringConstant value -> StringValue value
            | IntegerConstant value -> IntegerValue value
            | Reference name -> dereference name scope
            | ListForm (expr::args) ->
                match maybeForce <| eval scope expr with
                // Note how func, scope and args are captured within current closure.
                // This should probably be done in a more explicit way, like returning
                // a DelayedValue of InterpreterScope * SyntaxNode.
                | FunctionValue func -> callFunc func scope args
                | other -> raise <| AssertionException("a function", toStr other)
            | _ -> raise <| EvaluationException(sprintf "Don't know how to evaluate %A" node)
        ) |> DelayedValue
                
