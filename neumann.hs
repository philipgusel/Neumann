module Neumann where

test :: [(String,Int)]
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

run t = fst $ (runrecur ((0,0,0,0,0,0,0), t))


runrecur ((i,a,r1,r2,r3,r4,t), l) 
        | t == -1 = ((i,a,r1,r2,r3,r4,t), l)
        | t == -2 = ((i,a,r1,r2,r3,r4,t), l)
        | otherwise = runrecur ((doinstruction (i,a,r1,r2,r3,r4,t) (l !! i)), l)

doinstruction (i,a,r1,r2,r3,r4,t) (s,p)
        | s == "end" = (i,a,r1,r2,r3,r4,-1)

        | s == "ld" && p == 1 = ((i+1),r1,r1,r2,r3,r4,t)
        | s == "ld" && p == 2 = ((i+1),r2,r1,r2,r3,r4,t)
        | s == "ld" && p == 3 = ((i+1),r3,r1,r2,r3,r4,t)
        | s == "ld" && p == 4 = ((i+1),r4,r1,r2,r3,r4,t)

        | s == "dld" = ((i+1),p,r1,r2,r3,r4,t)

        | s == "sto" && p == 1 = ((i+1),a,a,r2,r3,r4,t)
        | s == "sto" && p == 2 = ((i+1),a,r1,a,r3,r4,t)
        | s == "sto" && p == 3 = ((i+1),a,r1,r2,a,r4,t)
        | s == "sto" && p == 4 = ((i+1),a,r1,r2,r3,a,t)

        | s == "add" && p == 1 = ((i+1),(a+r1),r1,r2,r3,r4,t)
        | s == "add" && p == 2 = ((i+1),(a+r2),r1,r2,r3,r4,t)
        | s == "add" && p == 3 = ((i+1),(a+r3),r1,r2,r3,r4,t)
        | s == "add" && p == 4 = ((i+1),(a+r4),r1,r2,r3,r4,t)

        | s == "sub" && p == 1 = ((i+1),(a-r1),r1,r2,r3,r4,t)
        | s == "sub" && p == 2 = ((i+1),(a-r2),r1,r2,r3,r4,t)
        | s == "sub" && p == 3 = ((i+1),(a-r3),r1,r2,r3,r4,t)
        | s == "sub" && p == 4 = ((i+1),(a-r4),r1,r2,r3,r4,t)

        | s == "prd" && p == 1 = ((i+1),(a*r1),r1,r2,r3,r4,t)
        | s == "prd" && p == 2 = ((i+1),(a*r2),r1,r2,r3,r4,t)
        | s == "prd" && p == 3 = ((i+1),(a*r3),r1,r2,r3,r4,t)
        | s == "prd" && p == 4 = ((i+1),(a*r4),r1,r2,r3,r4,t)

        | s == "div" && p == 1 = ((i+1),(a `div` r1),r1,r2,r3,r4,t)
        | s == "div" && p == 2 = ((i+1),(a `div` r2),r1,r2,r3,r4,t)
        | s == "div" && p == 3 = ((i+1),(a `div` r3),r1,r2,r3,r4,t)
        | s == "div" && p == 4 = ((i+1),(a `div` r4),r1,r2,r3,r4,t)
        
        | s == "jmp" = (p,a,r1,r2,r3,r4,t)

        | s == "jge" && a >= 0 = (p,a,r1,r2,r3,r4,t)
        | s == "jge" && a < 0 = ((i+1),a,r1,r2,r3,r4,t)

        | s == "jgt" && a > 0 = (p,a,r1,r2,r3,r4,t)
        | s == "jgt" && a <= 0 = ((i+1),a,r1,r2,r3,r4,t)
        
        | s == "jle" && a < 0 = (p,a,r1,r2,r3,r4,t)
        | s == "jle" && a >= 0 = ((i+1),a,r1,r2,r3,r4,t)

        | s == "jlt" && a < 0 = (p,a,r1,r2,r3,r4,t)
        | s == "jlt" && a >= 0 = ((i+1),a,r1,r2,r3,r4,t)

        | s == "jeq" && a == 0 = (p,a,r1,r2,r3,r4,t)
        | s == "jeq" && a /= 0 = ((i+1),a,r1,r2,r3,r4,t)

        | s == "jne" && a /= 0 = (p,a,r1,r2,r3,r4,t)
        | s == "jne" && a == 0 = ((i+1),a,r1,r2,r3,r4,t)

        | otherwise = (i,a,r1,r2,r3,r4,-2)

