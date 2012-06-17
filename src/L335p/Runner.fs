namespace L335p

open System
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

    type LeFunctionArg = L3Value

    /// Interpreter scope is a map from name (symbol) to LeValue
    and LeScope = Map<string, L3Value>

    /// A function that can behave like what is called a "macro" in Lisp --
    /// it takes arguments unevaluated -- raw SyntaxNode's.
    and LeFunction = LeScope -> LeFunctionArg list -> L3Value

    /// A value that can be evaluated or passed as function argument.
    /// L335pValue -> L3Value
    and L3Value =
    /// A lazily-evaluated value.
    | DelayedVal of Lazy<L3Value>
    | FunctionVal of LeFunction
    | NothingVal
    | IntegerVal of int
    | StringVal of string
    | BooleanVal of bool
    | ListVal of L3Value list
    | SymbolVal of string
    // TODO do a NumericValue of double (or something better?) instead?

    /// Forces a DelayedValue evaluation until it evaluates to instant
    /// value. If the argument is not DelayedValue, does nothing.
    /// Never use this to return values. Only force DelayedValue evaluation
    /// at points where you actually need that value.
    let rec force (value: L3Value): L3Value =
        match value with
        | DelayedVal v -> force (v.Force())
        | any -> any

    /// Dereferences `name` in `scope`.
    let dereference name scope =
        match Map.tryFind name scope with
        | Some value -> force value
        | None -> raise <| InvalidReferenceException(name, toStr scope)

    let expectIntegerValue x = 
        match force x with 
        | IntegerVal v -> v 
        | other -> raise <| TypeAssertionException("integer", toStr other) 

    /// Evaluates given node in scope. Returns LeValue.
    let rec eval (scope: LeScope) (node: L3Value): L3Value = 
        /// Calls a defined function `func` with arguments `args` in `scope`.
        /// Return type is same as IntepreterFunction's.
        let callFunc (func: LeFunction) scope (args: LeFunctionArg list) =
            // We don't want to evaluate function arguments here because some
            // functions might want the syntax tree, not the value.
            func scope args

        match node with
        | DelayedVal v -> force node // awkward
        | StringVal v -> node
        | IntegerVal v -> node
        | SymbolVal name -> dereference name scope
        | ListVal (expr::args) ->
            match eval scope expr with
            // Note how func, scope and args are captured within current closure.
            // This should probably be done in a more explicit way, like returning
            // a DelayedValue of LeScope * SyntaxNode.
            | FunctionVal func -> callFunc func scope args
            | other -> raise <| AssertionException("a function", toStr other)
        | _ -> raise <| EvaluationException(sprintf "Don't know how to evaluate %A" node)
                
    let lazyEval scope node = DelayedVal <| lazy (eval scope node)


