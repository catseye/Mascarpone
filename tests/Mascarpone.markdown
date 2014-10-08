Test Suite for Mascarpone
=========================

This test suite is written in the format of Falderal 0.9.  It is far from
exhaustive, but provides a basic sanity check on the language.

Mascarpone Tests
----------------

    -> Functionality "Interpret Mascarpone Program" is implemented by
    -> shell command
    -> "bin/mascarpone %(test-body-file)"

    -> Functionality "Interpret Mascarpone Program and Show Final State"
    -> is implemented by shell command
    -> "bin/mascarpone -r %(test-body-file)"

    -> Tests for functionality "Interpret Mascarpone Program"

Test nesting quotes.

    | [o[ll]eh].........
    = ]he]ll[o[

Make a new operation, defined as ",.", and execute it.

    | [,.]v*!
    + Z
    = Z

Redefine "&" as ",." in the current interpreter, and try it.

    | v[,.]v*'&<^&&&
    + Zil
    = Zil

Like the previous, but restore the old interpreter afterwards.

    | vv[,.]v*'&<^&&&^&&
    + Zam
    = Zam

Define an operation that modifies the caller's interpreter.
The operation `&` causes `m` to be redefined as `,.`.

    | v[v{[,.]v*'m<v}^]v*'&<^mmmmm&mm
    + ZK
    = ZK

"Capture" a value in an operation: given a value, push
an operation that pushes that value when executed.

We want to push the string

    ['v]

onto the stack, where v is the value we were given.  So we:

    push [
    swap
    push '
    swap
    push ]

Then we are ready to make the operation.

    | v['[/''/']v*]v*'?<^'p?!.
    = p

Treat an interpreter as a store.  Define S to mean,
pop a symbol, a value, and an interpreter, and push a new
interpreter where the symbol means "push that value."
Then define F to mean, pop a symbol and an interpreter,
then extract the operation so named and run it (pushing
the value stored.)

    | v['[/''/']v*]v*'?<^v[/?/<]v*'S<[>!]v*'F<^[]v*1'p'kS'kF.
    = p

Get whatever definition the interpreter sees fit to give
us for a symbol input from the user, and output it.
We define '?' as above first, and for the most interesting
output (with this particular implementation ;) the user
should enter '?' when the time comes for ',' to execute...

    | v['[/''/']v*]v*'?<^v,>@$............
    + ?
    = ]*v]'/''/['[

Demonstrates how one can use * after @.

    | v['[/''/']v*]v*'?<^vv'?>@$v*'?<^'k?!.
    = k

Demonstrate that we cannot make an interpreter which is
its own parent.  Setting the parent of an interpreter
does not modify that interpreter; it produces a copy.

    | vv}^'k.
    = k
