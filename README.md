# Neumann
Library for Simulating a simple Neumann-architecture

```haskell
-- assembly programm for safe division with error value -1
test = [("dl",30),
        ("st",1),
        ("dl",3),
        ("st",2),
        ("dl", -1),
        ("st", 3),
        ("ld", 2),
        ("je", 11),
        ("ld", 1),
        ("div", 2),
        ("st", 3),
        ("ld", 3),
        ("e", 1)]

-- run it:

reg = run test

-- run from file:

-- with debug info

dofile "file"

-- without debug info

dofilenores "file"
