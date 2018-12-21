module Neumann where

import System.Environment
import Data.Char

memsize = 100 :: Integer
getres (i,a,r1,r2,r3,r4,-2,io,mem) = "syntax or addressing error on address " ++ ( show i ) ++ "\n"
getres (i,a,r1,r2,r3,r4,-3,io,mem) = "no end statement\n"
getres (i,a,r1,r2,r3,r4,t,io,mem) = " instruction pointer: " ++ (show i) ++ "\n accumulator: " ++ (show a) ++ "\n register R1: " ++ (show r1) ++ "\n register R2: " ++ (show r2) ++ "\n register R3: " ++ (show r3) ++ "\n register R4: " ++ (show r4) ++ "\n" ++ (show mem) ++ "\n"

runio (i,a,r1,r2,r3,r4,t,io,mem) = do printlist (reverse io)

replace pos newVal list = take (fromIntegral pos) list ++ newVal : drop (fromIntegral (pos+1)) list

getreserr (i,a,r1,r2,r3,r4,-2,io,mem) = "syntax or addressing error on address " ++ ( show i ) ++ "\n"
getreserr (i,a,r1,r2,r3,r4,-3,io,mem) = "no end statement\n"
getreserr _ = ""

printlist [] = do return ()
printlist (l:ls) = do
        putStr (parseio l)
        printlist ls

parseio (i,b)
        | b == True = [toascii (stoi i)]
        | b == False = i

isonly _ [] = True
isonly a (l:ls)
        | a == l = isonly a ls
        | otherwise = False

nmain = do
        a <- getArgs
        if a == [] 
                then do return ()
                else do
                        f <- readFile (a !! 0)
                        let c = run . map (\(a:b:[]) -> (a, stoi b)) . map (\l -> if (length l) /= 2 then [l !! 0, "0"] else l ) . map words . filter (\l -> not (isonly ' ' l))  . filter (\l -> (l !! 0) /= ';') . lines $ f
                        putStr (getreserr c)                        
                        runio c

dofile file = do
        f <- readFile file
        let c = run . map (\(a:b:[]) -> (a, stoi b)) . map (\l -> if (length l) /= 2 then [l !! 0, "0"] else l ) . map words . filter (\l -> not (isonly ' ' l))  . filter (\l -> (l !! 0) /= ';') . filter (/="") . lines $ f
        runio c
        putStr (getres c)                        

dofilenores file = do
        f <- readFile file
        let c = run . map (\(a:b:[]) -> (a, stoi b)) . map (\l -> if (length l) /= 2 then [l !! 0, "0"] else l ) . map words . filter (\l -> not (isonly ' ' l))  . filter (\l -> (l !! 0) /= ';') . filter (/="") . lines $ f
        runio c
        putStr (getreserr c)

dofilearg file s = do
        f <- readFile file
        let c = runargfull s . map (\(a:b:[]) -> (a, stoi b)) . map (\l -> if (length l) /= 2 then [l !! 0, "0"] else l ) . map words . filter (\l -> not (isonly ' ' l))  . filter (\l -> (l !! 0) /= ';') . filter (/="") . lines $ f
        runio c
        putStr (getres c)                        

dofilenoresarg file s = do
        f <- readFile file
        let c = runargfull s . map (\(a:b:[]) -> (a, stoi b)) . map (\l -> if (length l) /= 2 then [l !! 0, "0"] else l ) . map words . filter (\l -> not (isonly ' ' l))  . filter (\l -> (l !! 0) /= ';') . filter (/="") . lines $ f
        runio c
        putStr (getreserr c)


run t = fst $ (runrecur ((0,0,0,0,0,0,0,[],replicate (fromIntegral memsize) 0), t))
runarg (i,a,r1,r2,r3,r4) tape = fst $ (runrecur ((i,a,r1,r2,r3,r4,0,[],replicate (fromIntegral memsize) 0), tape))
runargfull (i,a,r1,r2,r3,r4,t,io,mem) tape = fst $ (runrecur ((i,a,r1,r2,r3,r4,t,io,mem), tape))

stoi :: String -> Integer
stoi s = read s

toascii :: Integer -> Char
toascii i 
        | i < 1000 = toEnum (fromIntegral i)
        | otherwise = '#'

lengthI :: [a] -> Integer
lengthI [] = 0
lengthI (l:ls) = 1 + lengthI ls

at :: [a] -> Integer -> a
at (l:ls) 0 = l
at (l:ls) n = at ls (n-1)

runrecur ((i,a,r1,r2,r3,r4,t,io,mem), l) 
        | t == -1 = ((i,a,r1,r2,r3,r4,t,io,mem), l)
        | t == -2 = ((i,a,r1,r2,r3,r4,t,io,mem), l)
        | i == (lengthI l) = ((i,a,r1,r2,r3,r4,-3,io,mem), l)
        | i >= lengthI l = ((i,a,r1,r2,r3,r4,-2,io,mem), l)
        | otherwise = runrecur ((doinstruction (i,a,r1,r2,r3,r4,t,io,mem) (l `at` i)), l)

doinstruction (i,a,r1,r2,r3,r4,t,io,mem) (s,p)
        | s == "e" = (i,a,r1,r2,r3,r4,-1,io,mem)
        | s == "n" = ((i+1),a,r1,r2,r3,r4,t,io,mem)
        | s == "ld" && p == 1 = ((i+1),r1,r1,r2,r3,r4,t,io,mem)
        | s == "ld" && p == 2 = ((i+1),r2,r1,r2,r3,r4,t,io,mem)
        | s == "ld" && p == 3 = ((i+1),r3,r1,r2,r3,r4,t,io,mem)
        | s == "ld" && p == 4 = ((i+1),r4,r1,r2,r3,r4,t,io,mem)
        | s == "dl" = ((i+1),p,r1,r2,r3,r4,t,io,mem)
        | s == "st" && p == 1 = ((i+1),a,a,r2,r3,r4,t,io,mem)
        | s == "st" && p == 2 = ((i+1),a,r1,a,r3,r4,t,io,mem)
        | s == "st" && p == 3 = ((i+1),a,r1,r2,a,r4,t,io,mem)
        | s == "st" && p == 4 = ((i+1),a,r1,r2,r3,a,t,io,mem)
        | s == "a" && p == 1 = ((i+1),(a+r1),r1,r2,r3,r4,t,io,mem)
        | s == "a" && p == 2 = ((i+1),(a+r2),r1,r2,r3,r4,t,io,mem)
        | s == "a" && p == 3 = ((i+1),(a+r3),r1,r2,r3,r4,t,io,mem)
        | s == "a" && p == 4 = ((i+1),(a+r4),r1,r2,r3,r4,t,io,mem)
        | s == "sub" && p == 1 = ((i+1),(a-r1),r1,r2,r3,r4,t,io,mem)
        | s == "sub" && p == 2 = ((i+1),(a-r2),r1,r2,r3,r4,t,io,mem)
        | s == "sub" && p == 3 = ((i+1),(a-r3),r1,r2,r3,r4,t,io,mem)
        | s == "sub" && p == 4 = ((i+1),(a-r4),r1,r2,r3,r4,t,io,mem)
        | s == "prd" && p == 1 = ((i+1),(a*r1),r1,r2,r3,r4,t,io,mem)
        | s == "prd" && p == 2 = ((i+1),(a*r2),r1,r2,r3,r4,t,io,mem)
        | s == "prd" && p == 3 = ((i+1),(a*r3),r1,r2,r3,r4,t,io,mem)
        | s == "prd" && p == 4 = ((i+1),(a*r4),r1,r2,r3,r4,t,io,mem)
        | s == "div" && p == 1 = ((i+1),(a `div` r1),r1,r2,r3,r4,t,io,mem)
        | s == "div" && p == 2 = ((i+1),(a `div` r2),r1,r2,r3,r4,t,io,mem)
        | s == "div" && p == 3 = ((i+1),(a `div` r3),r1,r2,r3,r4,t,io,mem)
        | s == "div" && p == 4 = ((i+1),(a `div` r4),r1,r2,r3,r4,t,io,mem)
        | s == "j" = (p,a,r1,r2,r3,r4,t,io,mem)
        | s == "jg" && a >= 0 = (p,a,r1,r2,r3,r4,t,io,mem)
        | s == "jg" && a < 0 = ((i+1),a,r1,r2,r3,r4,t,io,mem)
        | s == "jl" && a <= 0 = (p,a,r1,r2,r3,r4,t,io,mem)
        | s == "jl" && a > 0 = ((i+1),a,r1,r2,r3,r4,t,io,mem)
        | s == "je" && a == 0 = (p,a,r1,r2,r3,r4,t,io,mem)
        | s == "je" && a /= 0 = ((i+1),a,r1,r2,r3,r4,t,io,mem)
        | s == "jn" && a /= 0 = (p,a,r1,r2,r3,r4,t,io,mem)
        | s == "jn" && a == 0 = ((i+1),a,r1,r2,r3,r4,t,io,mem)
        
        | s == "st" && p == 0 = (a,a,r1,r2,r3,r4,t,io,mem)
        | s == "ld" && p == 0 = (i,i,r1,r2,r3,r4,t,io,mem)
-- IO
        | s == "io" && p == 1 = ((i+1), a,r1,r2,r3,r4,t,((show a,False):io),mem)
        | s == "io" && p == 0 = ((i+1), a,r1,r2,r3,r4,t,((show a,True):io),mem)
        | s == "io" && p == 2 = ((i+1),a,r1,r2,r3,r4,t,(((show (i,a,r1,r2,r3,r4)),False):io),mem)
        | s == "io" && p == 3 = ((i+1),a,r1,r2,r3,r4,t,(((show mem),False):io),mem)
-- mem
        | s == "mld" = ((i+1), (cycle mem) `at` p,r1,r2,r3,r4,t,io,mem)
        | s == "mst" = ((i+1), a,r1,r2,r3,r4,t,io,replace p a mem)
        | s == "pld" = ((i+1), (cycle mem) `at` a,r1,r2,r3,r4,t,io,mem)  
        | s == "pst"  && p == 1 && a < memsize = ((i+1),a,r1,r2,r3,r4,t,io,replace a r1 mem)
        | s == "pst"  && p == 2 && a < memsize = ((i+1),a,r1,r2,r3,r4,t,io,replace a r2 mem)
        | s == "pst"  && p == 3 && a < memsize = ((i+1),a,r1,r2,r3,r4,t,io,replace a r3 mem)
        | s == "pst"  && p == 4 && a < memsize = ((i+1),a,r1,r2,r3,r4,t,io,replace a r4 mem)
-- default
        | otherwise = (i,a,r1,r2,r3,r4,-2,io,mem)
