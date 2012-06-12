namespace L335p

open System
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
    let rec eval (scope: InterpreterScope) (node: SyntaxNode): InterpreterValue =
        match node with
        | StringConstant value -> StringValue(value)
        | IntegerConstant value -> IntegerValue(Int32.Parse value)
        | Reference name -> dereference name scope
//        | ListForm (Reference name::args) ->
//            match maybeForce (dereference name scope) with
//            | FunctionValue func -> callFunc func scope args
//            | other -> raise <| EvaluationException(sprintf "%s is not a function at %A -- it is a %A" name node other)
        | ListForm (expr::args) ->
            match eval scope expr with
            | FunctionValue func -> callFunc func scope args
            | other -> raise <| EvaluationException(sprintf "%A is not a function at %A -- it is a %A" expr node other)
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

