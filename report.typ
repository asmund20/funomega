#set document(title: [Implementation of the $"Fun"_omega$ language])
#set heading(numbering: "1.1")
#set par(justify: true)
#set page(numbering: "1")
#show "funomega": $"Fun"_omega$
#show "haskell": [Haskell]
#set raw(lang: "haskell")

#align(center)[
  #title()
  #datetime.display(datetime.today())
]

= Introduction
This report documents my solution to the first home exam in IN5630.

The task was to make a parser, type inference and an interpretater for funomega,
a functional programming language with higher order functions, type inference,
user defined data types, algebraic data types and pattern matching. To my
knowledge, the parser is implemented correctly. The interpreter mostly works,
except for $alpha$ conversion.
= Testing
I do testing with unit tests only. There are separate tests for the parser, type
inference and interpreter, though all the tests use the parser. The type
inference and interpreter tests are independent of each other.

== Unit tests
The unit tests all (except possibly negative parser cases) start by parsing a
program, yielding some operators, data definitions and variables to work with.
Then the following general outline is done for some different programs. The
parser tests verifies parsing some terms against hard-coded ASTs, checks that
two terms parses to the same AST or checks that a term will fail parsing. The
type inference tests all start by checking if the type inference for the
definitions succeeds or fails as expected. Then it verifies that terms get the
expected type or fail as expected. The interpreter tests most check that a term
evaluates to some expected value.

=== Parser tests
The parser tests generally test that
- The empty program parses
- That associativity of operators work for right-, left- and non-associative
  operators
- That precedence works as expected
- That a single binary operator parses to the abstract syntax it should do as
  described in @fig:binOp, for application
- That multiple type variables in a data definition parse to the expected
  abstract syntax
- That different associativities at the same precedence does not parse
- That the example in the task is accepted

=== Type inference tests
The type inference tests generally test that
- The empty program type checks
- That the example program from the task type checks
- That some terms made from various definition source files are of the expected
  type for
  - Constructors, including with type variables
  - Functions, both applied and not
  - Variables
- That unbound variables and undefined constructors result in a rejected term
- That unwanted duplicates are rejected:
  - Duplicate type variables in a data definition
  - Duplicate data definition names
  - Duplicate constructor names
  - Duplicate binary operator names
  - Duplicate variable names
- That a definition of the form `x: a = case t of .` is rejected during type
  checking, verifying that at leas one pattern is there

=== Interpreter tests
In my interpreter tests I have something that generates the funomega term for a
number. I have a small test that that works as expected. Other than that I test
that
- Some terms in the example from the task evaluates to the expected value
- That a lot of numerical expressions evaluate to the expected number, and that
  number equality is correct
- That a non-exhaustive pattern will throw an error if none of the patterns
  match, and will work if it matches
- That alpha conversion is done correctly, which it is not, so this test fails.

== Property based testing
In preparation for making property based testing, I have wrapped the interpreter
tests in IO. That way, I can load a program and make a property using the
program, for example testing that addition and multiplication work when
implemented with peano arithmetic. Similar tests can be done for other kinds of
programs, but for the properties, one would need to make a mapping from the
haskell output to either a funomega `Value` or funomega code directly making the
term that will then be trivially converted to the expected value. I have not had
the time to actually implement the property.

== Test Limitations
There are a few limitations to the tests. The most glaring one is that there are
few negative test cases for the parser and interpreter. The parser only has two
negative test cases, one for chaining non-associative operators and different
associativity at the same precedence level does not parse. The interpret only
has one negative case, which is that it results in a runtime error if a case
term does not get a match on any of the patterns. There should not be any other
runtime errors, as the type checker should detect them.

Other than that, I do not check that negative test cases fail for the correct
reason. In general, that is hard to define. I do not check that disallowed
operator names or variable names are rejected in the proper manner.

=== Driver
The repl is not included in the test suite. It can easily be manually verified
that i works by running each of the commands, which all parse and execute
properly.

= Completeness & Correctness <issues>
I have comprehensively tested correct programs for parsing, type inference and
interpreting. I have only found one thing that does not work, which is general
$alpha$-conversion. The problem happens when a term `ty` containing a global
variable `y` is the argument to a term of the form `(fun x -> fun y -> t) ty`,
binding the global variable to the function variable instead. It can also happen
if instead of a `fun y`, there is a pattern binding y. Other than that, there
could be some programs the parser accepts that should not be accepted, for
example ones containing reserved variable or operator names. That is checked for
manually in the parser, but not tested.

Other than the issues mentioned above, the implementation is to my best
knowledge correct and complete, based on my tests.

= Implementation
For my implementation, I mostly filled in the undefined sections and wrote
helper functions as needed. In this section I explain what changes I have made
and briefly explain some of the more involved parts of the code.

== Parser
The parsing is done in two passes, like the task recommends. The first pass only
extracts information on the fixity, precedence and name for the binary
operators, creating an operator table. This is then used to create a term
parser, which is then used for parsing the definitions and the terms in the
repl. I removed the `parseTerm :: String -> Either ParseError Term`, as the
driver never has to only parse a term. I changed the type for
`parseDefinitions :: String -> Either ParseError ([Definition], OpTable)`, so it
returns the operator table for further use. I also made it not accept a file
path but the source string, so the IO can be handled in the driver. I also
changed the type for
`parseCommand :: OpTable -> Prompt -> Either ParseError Command`, so it can make
the term parser based on the operator table returned by `parseDefinitions`.

I do not use `buildExpressionParser`. The initial idea was to use it, but it was
easier to start small and combine parsers with combinators. I ended up with
something that is a less powerful version of `buildExpressionParser`. To change
to use `buildExpressionParser`, all that must be done is therefore to convert
the `OpTable` from the precode to the kind `buildExpressionParser` expects. It
works by iterating through the precedence levels in the `OpTable`, using
`chainl1` or `chainr1` on terms containing only operators that bind tighter.

Like in haskell, I have made it so that after a constructor or function, any
compound term (constructor with arguments, application of terms or binary
operator) must be done within parentheses. At the highest precedence level,
there is function application and constructors. Compound terms here, except the
top-level, must be within parentheses. That is done by either parsing a
constructor followed by literals
#footnote[With literal I mean a function term, variable or case term as
  described in the task, a term in parentheses, a variable or an algebraic term
  without arguments.]
or a `chainl1` of literals applied to each other.

== Type inference
The type inference is done by filling in the precode. I have changed the
environment and substitution types, both to `Data.Map X Type`. That enables some
easier debugging, as a function cannot simply be printed, and increases
performance for doing substitutions, as well as taking the union of two
substitutions. I also changed the type for the exported functions for easier use
from the driver. The exported functions are now
`checkProgram :: [Definition] -> Either TypeError Program` and
`runTermCheck :: Term -> Program -> Either TypeError Type`, which can be used
directly as pure functions or within an either monad. I still have the
`checkTerm :: Term -> Analysis Type`, but that is only used within the type
checker, and is not exported from the module. I also changed the type of the
analysis monad to
```
runCheck ::
  (Program, Environment, Int) ->
  Either TypeError ([Constraint], Substitution, Int, a)
```
I thus added a state, which is an integer used for creating new type variables
and a substitution to the writer part, which is used for swapping back type
variables that are replaced with generated ones to the name they have in the
code, after running the type inference. The `Analysis` monad should therefore
probably be changed to use an imported RWS monad from `Control.Monad`, which I
would have done if I would have realized it was becoming a RWS monad earlier.

Non-exhaustive patterns in case terms are not detected. That is assumed to be
outside of the scope of the type checker, and if a case interpreted with a term
that doesn't match any of the patterns, a runtime error occurs.

== Type variables in the source
The syntax supports having type variables in a program in two different places.
The definition of a data type and as the annotation for the type of a variable.
The type variables in the data type definition are replaced with a generated
type variable during type checking, and that type variable is then changed back
when inference is done. That is done by the substitution in the writer part of
the `Analysis` monad. That enables using the same name for type variables in
different data definitions, for example `List a` and `Maybe a`. The same is not
the case for variables. Their type variables are not replaced with generated
ones, so type variables used in variable definitions must be unique in a
program. The same could be done here, by replacing with generated type variables
and changing back after inference, but I did not have the time to implement
that.

=== Polymorphism
The language has some small polymorphism features, and lacks some that would be
nice to have. By polymorphic, I mean here that one can create at least two
variables that have different types, using the same function, binary operator or
data definition, in a funomega source file. My type checker correctly infers the
type and accepts programs which use data types with different instantiations of
the type variables, for example a program that has a `List a` data definition,
and variables that are for example both `List Nat` and `List Bool` for two other
data definitions `Nat` and `Bool`.

My type checker does not support polymorphic functions or binary operators. It
can and will recognize that a function can be used with many types, for example
the identity function will get a type that contains a type variable. If the
function is used within the source file, that type variable will then be bound
to match the type of what it is applied to, meaning that if it is applied to
something else, it will result in a type mismatch. The same applies to binary
operators, which are syntax sugar for function definitions.

Functions and binary operators are, however, polymorphic in the sense that if
their type contains a type variable after type inference of the file, the type
variable can bind to a different type every time and `Eval Term` command is run
in the repl. That is due to the type check of a command not changing the
definitions/program, so the binding is only local to that single type check.

== Interpreter
In the interpret, I mostly filled out the precode and made helper functions.
Other than that I removed the `emptyProgram`, and made a wrapper for running
`eval`, `interpretTerm :: Program -> Term -> Either RuntimeError Value`. I make
sure that substituting a term in for a variable does not pass an explicit
binding of that variable in either a pattern or a function term. That is not
enough for full alpha-conversion, as mentioned in @issues.

== REPL
The driver supports the commands. Loading files add files to a set, meaning that
multiple files can be loaded at the same time. When loading, the files are
combined into a single source code string. This can result in bad error
messages, as line numbers in parse errors will not align with the number in the
actual files that are loaded, except for the first one. I added the feature that
\\EOT exits the repl, otherwise it is as and only fills in the precode.


== Binary operators
Binary operators are treated as syntax sugar, both for definition and usage.
During the first parser pass, the fixity, precedence level and name are
retrieved for all the binary operators. This information is used for parsing
terms containing the binary operators in the second pass. During the second
parsing pass, the definition parses into a nested function variable. Terms with
binary operators are parsed into application of that nested function to the two
terms surrounding it, as shown in @fig:binOp.

#figure(
  table(
    columns: 2,
    $
      PP & ::= FF space n space x_0 plus.o x_1 = t space . \
      PP & ::= plus.o #h(0em) : plus.o = "fun" x_0 -> "fun" x_1 -> t space .
    $,
    $
      t & ::= t_0 plus.o t_1 \
      t & ::= plus.o t_0 t_1
    $,
  ),
  caption: [
    The top row is the parsed syntax for binary operators. The bottom row is the
    concrete syntax it would be parsed like. A fixity definition is syntax sugar
    for a variable definition for a nested function that also has a precedence
    level and an associativity. A term with a binary operator and two terms is
    syntax sugar for application of that variable to the two terms.
  ],
) <fig:binOp>

= Efficiency
The time complexity of the parser is linear. That is because try is only used
for keywords, which have a finite length and there is a finite number of them
when the first pass extracting the operator is able to finish, leading to a
constant amount of backtracking and linear time.

The time complexity for the type inference is exponential in time.

The time complexity of the interpret is undecidable, as a program that does not
halt can be written in the language. Case terms and function application both
substitute terms in other terms. This is linear with respect to the size of the
term that the substitution is done on. That means that evaluating $n$ nested
case terms results in a complexity of $cal(O)(product_(i=1)^n c_i)$ where $c_i$
is the complexity of case number $i$.


The space complexity of the parser is linear, as it stores the entire program in
memory as a syntax tree.

The space complexity for type inference can possibly be bad, as substituting
types into other types can become arbitrarily large when nested.

Space complexity for interpret is also undecidable, as a program using infinite
memory can be created. During execution, terms are generally made larger and
larger before they are evaluated, so the program definitely grows in space.


= Maintainability
The most important thing is to implement proper $alpha$-conversion. Then the
funomega programs used for testing should be moved or copied to the `test`
folder, so it is clear that they are not only examples but also used for
testing, and should not be altered without careful consideration. Some
property-based testing for the interpret should be made, and also more negative
unit tests for parser, interpret and type inference.

= Conclusion
I have mostly implemented a correct and complete parser, type inference and
interpret, except for $alpha$-conversions and possibly not rejecting some
programs that should be rejected at the earliest possible stage.
