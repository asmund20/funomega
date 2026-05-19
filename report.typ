#set document(title: [Implementation of the $"Fun"_omega$ language])
#set heading(numbering: "1.1")
#set par(justify: true)
#set page(numbering: "1")
#show "funomega": $"Fun"_omega$

#align(center)[
  #title()
  #datetime.display(datetime.today())
]

#outline(depth: 2)

= Known issues
- `examples/failure.fomega` should result in C, it gets B.
- `data Something a a = Trivial .` should likely fail due to the same type
  variable being used twice.
- `examples/numbers.fomega`: The multiplication does not work, though it should
  be the correct funomega program for multiplication.
