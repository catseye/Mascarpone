The Mascarpone Programming Language
===================================

_Try it online_ [@ catseye.tc](https://catseye.tc/installation/Mascarpone)
| _Wiki entry_ [@ esolangs.org](https://esolangs.org/wiki/Mascarpone)
| _See also:_ [Emmental](https://github.com/catseye/Emmental)

- - - -

*You are lost in a twisty maze of meta-circular interpreters, all
alike.*

Introduction
------------

Mascarpone is a self-modifying programming language in the style of
[Emmental](http://catseye.tc/node/Emmental/). In fact it is a
rationalization and further exploration of some of the basic ideas
behind Emmental. In Mascarpone, meta-circular interpreters are
"first-class objects": they can be pushed onto the stack, have
operations extracted from and installed into them, and can themselves be
meta-circularly extracted from the language environment ("reified") or
installed into it ("deified.") New operations can be defined as strings
of symbols, and these symbols are given meaning by an interpreter that
is "captured" in the definition, similar to the way that lexical
variables are captured in closures in functional languages. An operation
may also access, and modify, the interpreter that invoked it.

Like Emmental, Mascarpone relies on meta-circular
interpreter-modification to achieve Turing-completeness. Unlike
Emmental, Mascarpone is purely symbolic; there are no arithmetic
instructions.

Stack
-----

Like Emmental, Mascarpone is a stack-based language. Unlike Emmental,
Mascarpone's stack may contain things other than symbols. A stack
element in Mascarpone may be a symbol, an operation, or an interpreter.

Strings are popped off Mascarpone's stack slightly differently than
Emmental's. A string begins with the symbol `]` on the stack; this is
popped and discarded. Symbols are then successively popped and prepended
to a growing string. As further `]`'s are encountered, they too are
prepended to the string, but the nesting level is incremented for each
one as well. Whenever a `[` is encountered, it is prepended to the
string and the nesting level is decremented, unless it is zero, in which
case the `[` is discarded and the string is complete. The net effect of
all this futzing around is that `[]` work as nestable quoting symbols.

Also unlike Emmental, Mascarpone does not have a queue.

Meta-circular Interpreters
--------------------------

The idea of an interpreter in Mascarpone is similar to that in Emmental.
In Mascarpone, an interpreter is a map that takes symbols to operations,
and an operation is a sequence of symbols that is given meaning by some
interpreter.

Of course, this is a circular definition, but that doesn't seem
unreasonable, since we're working with meta-circular interpreters. If
you like, you can think of it as forming an "infinite tower of
meta-circular interpreters," but that's never been a really satisfying
explanation for me. As I explained in the Emmental documentation, I
think you need some source of understanding external to the definition
in order to make complete sense of a meta-circular interpreter. (I also
happen to think that humans have some sort of innate understanding of
interpretation — that is, language — so that this demand for further
understanding doesn't recurse forever.)

There is a special interpreter in Mascarpone called "null". It is an
error to try to interpret anything with this interpreter. Expect that
any program that tries to do this will come crashing to a halt, or will
spin off into space and never be heard from again, or something equally
impressive.

Every interpreter (except for null) is linked to a "parent" interpreter
(which may be null.) No interpreter can be its own ancestor; the
parent-child relationships between interpreters form a directed, acyclic
graph (or DAG.)

There is, at any given time in a Mascarpone, a current interpreter: this
is the interpreter that is in force, that is being used to interpret
symbols. The parent interpreter of the current interpreter is generally
the interpreter that was used to execute the current operation (that is,
the operation currently being interpreted; it consists of a string of
symbols is interpreted by the current interpreter.)

The current interpreter when any top-level Mascarpone program begins is
the initial Mascarpone interpreter, which is described in English in the
next section.

Initial Mascarpone Interpreter
------------------------------

`v` ("reify") pushes the current interpreter onto the stack.

`^` ("deify") pops an interpreter from the stack and installs it as the
current interpreter.

`>` ("extract") pops a symbol from the stack, then pops an interpreter.
It pushes onto the stack the operation associated with that symbol in
that interpreter.

`<` ("install") pops a symbol from the stack, then an operation, then an
interpreter. It pushes onto the stack a new interpreter which is the
same as the given interpreter, except that in it, the given symbol is
associated with the given operation.

`{` ("get parent") pops an interpreter from the stack and pushes it's
parent interpreter onto the stack.

`}` ("set parent") pops an interpreter i from the stack, then pops an
interpreter j. It pushes a new interpreter which is the same as i,
except that it's parent interpreter is j.

`*` ("create") pops an interpreter from the stack, then a string. It
creates a new operation defined by how that interpreter would interpret
that string of symbols, and pushes that operation onto the stack.

`@` ("expand") pops an operation from the stack and pushes a program
string, then pushes an interpreter, such that the semantics of running
the program string with the interpreter is identical to the semantics of
executing the operation. (Note that the program need not be the one that
the operation was defined with, only *equivalent* to it, under the given
interpreter; this allows one to sensibly expand "intrinsic" operations
like those in the initial Mascarpone interpreter.)

`!` ("perform") pops an operation from the stack and executes it.

`0` ("null") pushes the null interpreter onto the stack.

`1` ("uniform") pops an operation from the stack and pushes back an
interpreter where all symbols are associated with that operation.

`[` ("deepquote") pushes a `[` symbol onto the stack and enters "nested
quote mode", which is really another interpreter. In nested quote mode,
each symbol is interpreted as an operation which pushes that symbol onto
the stack. In addition, the symbols `[` and `]` have special additional
meaning: they nest. When a `]` matching the first `[` is encountered,
nested quote mode ends, returning to the interpreter previously in
effect.

`'` ("quotesym") switches to "single-symbol quote mode", which is really
yet another interpreter. In single-symbol quote mode, each symbol is
interpreted as an operation which pushes that symbol onto the stack,
then immediately ends single-symbol quote mode, returning to the
interpreter previously in effect.

`.` pops a symbol off the stack and sends it to the standard output.

`,` waits for a symbol to arrive on standard input, and pushes it onto
the stack.

`:` duplicates the top element of the stack.

`$` pops the top element of the stack and discards it.

`/` swaps to the top two elements of the stack.

Discussion
----------

### Design decisions

As you can see, Mascarpone's semantics and initial operations are a lot
less "fugly" than Emmental's. It's a more expressive language, in that
it's easier to elegantly convey things involving interpreters and
meta-circularity in Mascarpone than it is in Emmental. It explores at
least one idea that I explicitly mentioned in the Emmental documentation
that I'd like to explore, namely, having multiple meta-circular
interpreters and being able to switch between them (and lo and behold,
Mascarpone has very well-developed `[]` and `'` operations.) It's also
"prettier" in that there's more attention paid to providing duals of
operations (both `*` and `@`, for example.)

Mascarpone also appears to be Turing-complete, despite the lack of
explicit conditional, repetition, and arithmetic operators. A cyclic
meaning can be expressed by an operation which examines its own
definition from the parent interpreter of the current interpreter and
re-uses it. A conditional can be formed by creating a new interpreter in
which one symbol, say `S`, maps to an operation which does something,
and in which all other symbols do something else; executing a symbol in
this interpreter is tantamount to testing if that symbol is `S`.

"But", you point out, "Mascarpone only has one stack! You need at least
two stacks in order to simulate a Turing machine's tape." Actually,
Mascarpone *does* have another, less obvious stack: each interpreter has
a parent interpreter. By getting the current interpreter, modifying it,
setting it's parent to be the current interpreter, and setting it as the
current interpreter (in Mascarpone: `v`...`v}^`), we "push" something
onto it; by getting the current interpreter, getting its parent, and
setting that as the current interpreter (`v{^`), we "pop".

Actually, even if there was no explicit parent-child relationship
between interpreters, we'd still be able to store a stack of
interpreters, because each operation in an interpreter has its own
interpreter that gives meaning to the symbols in that operation, and
*that* interpreter can contain operations that can contain interpreters,
etc., etc., ad infinitum. This isn't a very classy way to do it, but
it's very reminiscent of how structures can be built in the lambda
calculus by trapping abstractions in other abstractions.

It's also worth noting that this is how you'd have to accomplish
arithmetic, with something like Church numerals done with interpreters
and operations, since Mascarpone has nothing but symbols. On the plus
side, this means Mascarpone, unlike Emmental, is highly independent of
character set or encoding — it doesn't even have to be ordered. Any set
of symbols that contains the symbols of the initial Mascarpone
interpreter, plus the symbols appearing in the Mascarpone program being
executed, plus the symbols that are desired for input and output, ought
to suffice.

Actually, that's not quite true: it should be a *finite* set. This is
mainly for the sake of the definition of the `'` operator: it switches
to an interpreter where all symbols indicate operations that push that
symbol on the stack. From this we can infer that there should either be
a finite number of such operations (and thus symbols,) or somehow these
operations know what symbol they are to push. They take the symbol that
invoked them as an argument, perhaps. But other operations in Mascarpone
do not have such capabilities: an operation need not even be invoked by
a symbol, as it could be invoked by the `!` operation, for instance.
That would make the operations in the `'` interpreter gratuitously
special. And, practically, most character sets, on which sets of symbols
are based, are finite, so I don't suppose this restriction is much of a
problem.

One further, somewhat related design decision deserves mention. Any
symbol which is not defined in the initial interpreter is interpreted as
a no-op. It probably would have been nicer to treat it as an explicit
error-causing operation. This could be extended to looking, inside each
putative definition, for symbols undefined in the desired interpreter
when executing a `*` operation, and causing a (preferably intelligible)
error early in that case. Semantics like this would have helped me save
time in debugging one or two of the test case programs. However, while
Mascarpone is arguably supposed to be less hostile than Emmental when it
comes to being programmed in, it's certainly still not what you'd call a
mainstream programming language, so while I'm somewhat irked by this
deficiency, I hardly consider it a show-stopper.

### Related Work

There are definitely two related works that are worth mentioning: Brian
Cantwell Smith's Ph.D. thesis "Procedural Reflection in Programming
Languages" (MIT, 1982,) and Friedman and Wand's paper "Reification:
Reflection without Metaphysics" (ACM LISP conference, 1984.) (Forgive me
for not giving proper, perfectly-formatted, Turabianly-correct
references to these two works, but frankly, this is the age of the
Internet: if you're interested in either of these papers, and you can't
find them, there's something wrong with you! If, on the other hand, you
don't have *access* to them, perhaps there's something wrong with the
institutions whose assumed goal is to increase the amount of human
knowledge — but not, it seems, to widen its availability.)

It's hard to say how much influence Smith's 3-LISP language and Friedman
and Wand's Brown language (introduced in the respective papers) have had
on Mascarpone: probably some, since I had read both of them (well, not
*all* of Smith's monster! but enough of it to grasp the main ideas, I
think) and thought about what they were trying to convey. (What Brown
calls "reflection" I've called "deification" to give a sort of
phonological dual to "reification". Also, the term "reflection" seems to
have taken on a more general meaning in computer science since the
'80's, so I wanted to avoid its use here.) But that was a couple of
years previous, and the subject of meta-circular interpreters came up
this time from a different angle; Mascarpone came primarily from trying
to "un-knot" the ideas behind Emmental, which itself came to be, quite
indirectly, from thinking about issues raised by John Reynolds' original
work on meta-circularity.

Certainly a huge difference that sets Mascarpone apart is that 3-LISP
and Brown are caught up in the whole LISP/Scheme thing, so they just use
S-expressions and functions to represent reified interpreter parts,
which include environments and continuations. Mascarpone, on the other
hand, reifies whole interpreters at once, as values which are complete
interpreters. Because interpreters contain operations which contain
interpreters ("ad infinitum", one might think,) this approach seems to
highlight the meta-circularity in a way that is particularly striking.
In addition, Mascarpone's "applicative" organization (like XY or Joy;
that is, like an idealized version of FORTH) lets it avoid some of the
referential issues like names and environments, and gives a nice direct
one-symbol-one-operation correspondence.

Because Mascarpone has interpreters as first-class values, it is never
obliged to make the guts of the running interpreter explicit during
reification — it just needs to make that interpreter available as a
value. The contract of the `@` operation (which, by the way, was a
somewhat late add to the language design, fulfilling the desire for a
dual to `*`) says you get a program and an interpreter with semantics
*equivalent* to the operation you specify, but it doesn't say *how*
they're provided. You could successively perform `@` on an intrinsic
operation (like, say, `@` itself) and get successively more explicit
definitions, written in Mascarpone, of what `@` means. Each one could be
thought of as descending (or ascending? does it matter?) a level in that
infinite tower dealie. Or, you might only get back a single, random
symbol, and an interpreter where all symbols have the semantics of `@`,
with no explanation whatsoever. This inbuilt ambiguity is, I think, the
appropriate level of abstraction for such an operation (in a
meta-circular context, anyway;) saying that you always get back the
program you defined the operation with seems overspecified (and unable
to handle the case of intrinsics,) and saying that you always get back
something opaque, like a function value, seems quite nonplussing in the
context of an interpreter that can supposedly examine its own structure.
It's not clear to me that either 3-LISP or Brown addresses this point to
this degree.

And of course, neither 3-LISP nor Brown tries to use reification and
deification as a means of achieving Turing-completeness in the absence
of conventional conditional and repetition constructs.

Implementation
--------------

`mascarpone.hs` is a reference interpreter for Mascarpone written in
Haskell. Run the function `mascarpone` on a string, or `demo n` to run
one of the included test cases. `mascarpone.hs` also has a much nicer
debugging facility than `emmental.hs`; you can run `debug` on a string
to view the state of the program (the current instruction, the rest of
the program, the stack, and the current interpreter) at each step of
execution. And you can run `test n` to debug the test cases. Lastly,
there is a `main` function that runs `mascarpone` on a string read from
a file named by the first argument, so a Haskell compiler can be used to
build a stand-alone Mascarpone interpreter from this source code.

Even happier interpreter-redefining!  
Chris Pressey  
Chicago, IL  
December 8, 2007
