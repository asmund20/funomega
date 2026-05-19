#set document(title: [Implementation of the $"Fun"_omega$ language])
#set heading(numbering: "1.1")
#set par(justify: true)
#set page(numbering: "1")
#show "funomega": $"Fun"_omega$

#align(center)[
  #title()
  #datetime.display(datetime.today())
]

// TODO: Comment in
// #outline(depth: 2)

= Introduction
This report documents my solution to the first home exam in IN5630.

The task was to make a parser, type inference and an interpretater for funomega,
a functional programming language with higher order functions, type inference,
user defined data types, algebraic data types and pattern matching. To my
knowledge, the parser is implemented correctly. //
The type checker accepts some nonsensical programs and rejects some meaningful
ones. These are edge cases and can be fairly easily fixed, and described in
@issues.// TODO: Fixed?
The interpreter mostly works. There are some issues with scoping and variable
binding, described in @issues. // TODO: Fixed?

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
expected type or fail as expected. The interpreter tests all check that a term
evaluates to some expected value. There are no negative cases for the
interpreter because all programs accepted by the type checker should result in a
program that will execute to a value.

== Property based testing
// TODO: Have I made property based testing?
In preparation for making property based testing, I have wrapped the interpreter
tests in IO. That way, I can load a program and make a property using the
program, for example testing that addition and multiplication work when
implemented with peano arithmetic. Similar tests can be done for other kinds of
programs, but for the properties, one would need to make a mapping from the
haskell output to either a funomega `Value` or funomega code directly making the
term that will then be trivially converted to the expected value.

== Test Limitations
// TODO

= Completeness & Correctness
// TODO

== Known issues <issues>
- `data Something a a = Trivial .` should likely fail due to the same type
  variable being used twice.
- `examples/typeCheckerToAccept.fomega` has two variables of different types
  where the type is declared to be the same type variable. This should be
  allowed, as type variables should be local to a single definition.
- `examples/failure.fomega` should result in C, it gets B.
- `examples/numbers.fomega`: The multiplication does not work, though it should
  be the correct funomega program for multiplication.

= Implementation
// TODO
// For the implementation of the interpreter, I simply filled in the missing
// definitions, adding some helper functions. For the implementation of the parser,
// the biggest decision I made was not using `buildExpressionParser` and rather
// combining expressions manually. The main reason was that I did not know about
// the feature from the beginning, and did not want to change once I had
// implemented it. It was a nice learning opportunity to manually specify the
// precedence.

= Efficiency
The time complexity of the parser is linear. That is because try is only used
for keywords, which have a finite length and there is a finite number of them
leading to a constant amount of backtracking and linear time.

// TODO: Time complexity for type inference

// TODO: Time complexity for interpret
// The time complexity if the interpreter is more involved to analyze. Variable
// lookup is linear because the environment is a list, which is kept as it was in
// the precode. The time complexity of assignment is the same as the time
// complexity for the expression. The time complexity of executing all expressions
// except the aforementioned variable lookup, function calls, equality and
// comparison of lists and list comprehensions directly inherit the complexity of
// performing the same operations in Haskell. The time complexity of print calls is
// the sum of the complexity of the arguments. The time complexity of range calls
// is linear with respect to the difference between start and stop. The time
// complexity of list expressions is the sum of the complexity of the
// sub-expressions. The time complexity of list comprehensions is exponential with
// respect to the number of for clauses.

The space complexity of the parser is linear, as it stores the entire program in
memory as a syntax tree.

// TODO: Space complexity for type inference

// TODO: Space complexity for interpret
// The space complexity of the interpreter is constant for all expressions except
// range calls and list comprehensions. Range calls are linear in space with
// respect to the parameters and list comprehensions are exponential in space with
// respect to the number of for clauses.


= Maintainability
// TODO
// The `operate` function is nearly 100 lines long, and contains duplicated code.
// In large part, that is due to manually pattern matching both booleans and
// numbers in both arguments for arithmetic operators. If any changes are to be
// made to the function, a full rewrite is to be preferred.

= Conclusion
// TODO
