# Neumann
Library for Simulating a simple Neumann-architecture

```haskell
-- assembly programm for safe division with error value -1
test = [("dld",30),
        ("sto",1),
        ("dld",3),
        ("sto",2),
        ("dld", -1),
        ("sto", 3),
        ("ld", 2),
        ("jeq", 11),
        ("ld", 1),
        ("div", 2),
        ("sto", 3),
        ("ld", 3),
        ("end", 1)]

-- run it:

reg = run test

-- run returns the state of all registers

-- run from file:

-- example 1: safe division
dofile "neumann_test1"

-- example 2: calculate fibonacci numbers
dofile "neumann_test2"
       
```
