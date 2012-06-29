l335p
=====

A Lisp-like interpreter.

TODO:
* CLI
* ability to run programs from external files
* builtins are a mess, need to refactor that. probably put all funcs in static types which extend the base "namespace" type.
* TCO
* multi-line strings can't be lexed!
* add integration tests; some unit tests, but low on priority
* floating point arithmetic
* add context information to syntax nodes (like row/col position, etc.)