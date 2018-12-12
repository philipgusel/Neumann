module Neumann where

import System.Environment
import Data.Char

getres (i,a,r1,r2,r3,r4,-2,io) = "Syntax Error on address " ++ ( show i ) ++ "\n"
getres (i,a,r1,r2,r3,r4,-3,io) = "No end statement\n"
getres (i,a,r1,r2,r3,r4,t,io) = " instruction pointer: " ++ (show i) ++ "\n accumulator: " ++ (show a) ++ "\n register R1: " ++ (show r1) ++ "\n register R2: " ++ (show r2) ++ "\n register R3: " ++ (show r3) ++ "\n register R4: " ++ (show r4) ++ "\n"

runio :: (Integer,Integer,Integer,Integer,Integer,Integer,Integer,[(Integer,Bool)]) -> IO ()
runio (i,a,r1,r2,r3,r4,t,io) = do printlist (reverse io)

getreserr (i,a,r1,r2,r3,r4,-2,io) = "Syntax Error on address " ++ ( show i ) ++ "\n"
getreserr (i,a,r1,r2,r3,r4,-3,io) = "No end statement\n"
getreserr _ = ""

printlist :: [(Integer,Bool)] -> IO ()
printlist [] = do return ()
printlist (l:ls) = do
        putStr (parseio l)
        printlist ls

parseio :: (Integer,Bool) -> String
parseio (i,b)
        | b == True = [toascii i]
        | b == False = show i

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

run :: [(String,Integer)] -> (Integer,Integer,Integer,Integer,Integer,Integer,Integer,[(Integer,Bool)])
run t = fst $ (runrecur ((0,0,0,0,0,0,0,[]), t))

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

runrecur :: ((Integer,Integer,Integer,Integer,Integer,Integer,Integer,[(Integer,Bool)]), [(String,Integer)]) -> ((Integer,Integer,Integer,Integer,Integer,Integer,Integer,[(Integer,Bool)]), [(String,Integer)])
runrecur ((i,a,r1,r2,r3,r4,t,io), l) 
        | t == -1 = ((i,a,r1,r2,r3,r4,t,io), l)
        | t == -2 = ((i,a,r1,r2,r3,r4,t,io), l)
        | i == (lengthI l) = ((i,a,r1,r2,r3,r4,-3,io), l)
        | otherwise = runrecur ((doinstruction (i,a,r1,r2,r3,r4,t,io) (l `at` i)), l)

doinstruction (i,a,r1,r2,r3,r4,t,io) (s,p)
        | s == "end" = (i,a,r1,r2,r3,r4,-1,io)
        | s == "nop" = ((i+1),a,r1,r2,r3,r4,t,io)
        | s == "ld" && p == 1 = ((i+1),r1,r1,r2,r3,r4,t,io)
        | s == "ld" && p == 2 = ((i+1),r2,r1,r2,r3,r4,t,io)
        | s == "ld" && p == 3 = ((i+1),r3,r1,r2,r3,r4,t,io)
        | s == "ld" && p == 4 = ((i+1),r4,r1,r2,r3,r4,t,io)
        | s == "dld" = ((i+1),p,r1,r2,r3,r4,t,io)
        | s == "sto" && p == 1 = ((i+1),a,a,r2,r3,r4,t,io)
        | s == "sto" && p == 2 = ((i+1),a,r1,a,r3,r4,t,io)
        | s == "sto" && p == 3 = ((i+1),a,r1,r2,a,r4,t,io)
        | s == "sto" && p == 4 = ((i+1),a,r1,r2,r3,a,t,io)
        | s == "add" && p == 1 = ((i+1),(a+r1),r1,r2,r3,r4,t,io)
        | s == "add" && p == 2 = ((i+1),(a+r2),r1,r2,r3,r4,t,io)
        | s == "add" && p == 3 = ((i+1),(a+r3),r1,r2,r3,r4,t,io)
        | s == "add" && p == 4 = ((i+1),(a+r4),r1,r2,r3,r4,t,io)
        | s == "sub" && p == 1 = ((i+1),(a-r1),r1,r2,r3,r4,t,io)
        | s == "sub" && p == 2 = ((i+1),(a-r2),r1,r2,r3,r4,t,io)
        | s == "sub" && p == 3 = ((i+1),(a-r3),r1,r2,r3,r4,t,io)
        | s == "sub" && p == 4 = ((i+1),(a-r4),r1,r2,r3,r4,t,io)
        | s == "prd" && p == 1 = ((i+1),(a*r1),r1,r2,r3,r4,t,io)
        | s == "prd" && p == 2 = ((i+1),(a*r2),r1,r2,r3,r4,t,io)
        | s == "prd" && p == 3 = ((i+1),(a*r3),r1,r2,r3,r4,t,io)
        | s == "prd" && p == 4 = ((i+1),(a*r4),r1,r2,r3,r4,t,io)
        | s == "div" && p == 1 = ((i+1),(a `div` r1),r1,r2,r3,r4,t,io)
        | s == "div" && p == 2 = ((i+1),(a `div` r2),r1,r2,r3,r4,t,io)
        | s == "div" && p == 3 = ((i+1),(a `div` r3),r1,r2,r3,r4,t,io)
        | s == "div" && p == 4 = ((i+1),(a `div` r4),r1,r2,r3,r4,t,io)
        | s == "jmp" = (p,a,r1,r2,r3,r4,t,io)
        | s == "jge" && a >= 0 = (p,a,r1,r2,r3,r4,t,io)
        | s == "jge" && a < 0 = ((i+1),a,r1,r2,r3,r4,t,io)
        | s == "jgt" && a > 0 = (p,a,r1,r2,r3,r4,t,io)
        | s == "jgt" && a <= 0 = ((i+1),a,r1,r2,r3,r4,t,io)
        | s == "jle" && a < 0 = (p,a,r1,r2,r3,r4,t,io)
        | s == "jle" && a >= 0 = ((i+1),a,r1,r2,r3,r4,t,io)
        | s == "jlt" && a < 0 = (p,a,r1,r2,r3,r4,t,io)
        | s == "jlt" && a >= 0 = ((i+1),a,r1,r2,r3,r4,t,io)
        | s == "jeq" && a == 0 = (p,a,r1,r2,r3,r4,t,io)
        | s == "jeq" && a /= 0 = ((i+1),a,r1,r2,r3,r4,t,io)
        | s == "jne" && a /= 0 = (p,a,r1,r2,r3,r4,t,io)
        | s == "jne" && a == 0 = ((i+1),a,r1,r2,r3,r4,t,io)
-- extras
        | s == "sto" && p == 0 = (a,a,r1,r2,r3,r4,t,io)
        | s == "ld" && p == 0 = (i,i,r1,r2,r3,r4,t,io)
        | s == "ini" = ((i+1),a,p,p,p,p,t,io)
        | s == "mod" && p == 1 = ((i+1),(mod a r1), r1,r2,r3,r4,t,io)
        | s == "mod" && p == 2 = ((i+1),(mod a r2), r1,r2,r3,r4,t,io)
        | s == "mod" && p == 3 = ((i+1),(mod a r3), r1,r2,r3,r4,t,io)
        | s == "mod" && p == 4 = ((i+1),(mod a r4), r1,r2,r3,r4,t,io)
        | s == "sum" = ((i+1), (r1+r2+r3+r4), r1,r2,r3,r4,t,io)
        | s == "pow" && p == 1 = ((i+1), (a^r1), r1,r2,r3,r4,t,io)
        | s == "pow" && p == 2 = ((i+1), (a^r2), r1,r2,r3,r4,t,io)
        | s == "pow" && p == 3 = ((i+1), (a^r3), r1,r2,r3,r4,t,io)
        | s == "pow" && p == 4 = ((i+1), (a^r4), r1,r2,r3,r4,t,io)
        | s == "io" && p == 1 = ((i+1), a,r1,r2,r3,r4,t,((a,False):io))
        | s == "io" && p == 0 = ((i+1), a,r1,r2,r3,r4,t,((a,True):io))
-- default
        | otherwise = (i,a,r1,r2,r3,r4,-2,io)
