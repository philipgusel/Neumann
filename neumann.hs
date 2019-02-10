module Main where

import System.Environment
import Data.Char
import Text.Read
import Data.String (fromString)

nat = [0,1..]

data Stack a = S a (Stack a) | Empty deriving (Eq, Show)

push a s = (S a s)

pop Empty = Nothing
pop (S a _) = Just a

popr (S a r) = r

peek Empty = Nothing
peek (S a _) = Just a

element _ [] = False
element a (l:ls)
        | a == l = True
        | otherwise = element a ls

isonly _ [] = True
isonly a (l:ls)
        | a == l = isonly a ls
        | otherwise = False



stoimaybe :: String -> Maybe Integer
stoimaybe = readMaybe

isnumber :: String -> Bool
isnumber s
        | (stoimaybe s) == Nothing = False
        | otherwise = True

replace :: Integer -> a -> [a] -> [a]
replace pos newVal list = take (fromIntegral pos) list ++ newVal : drop (fromIntegral (pos+1)) list

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

findsnd i _ [] = i
findsnd i a ((n,l):ls)
        | a == l = n
        | otherwise = findsnd i a ls

main = do
        s <- getArgs
        parseargs s

makepath :: String -> FilePath
makepath = fromString

test s = parseargs [makepath s]


parseprog s = map (\(a:b:[]) -> (a, stoi b)) . map (\l -> if (length l) /= 2 then [l !! 0, "0"] else l ) . map words . filter (\l -> not (isonly ' ' l))  . filter (\l -> (l !! 0) /= ';') . filter (/="") . lines $ s

parseargs [] = do putStrLn "Keine Datei angegeben"
parseargs (f:fa) = do
        file <- readFile f
        run (parseprog file)

-- state layout:
-- ((instruction pointer, accumulator, r1, r2, r3, r4, r5, err, io,  regstack, valstack), tape)

run t = do runrecur ((0,0,0,0,0,0,0,0,((False,'p'),""),Empty,Empty),t)

newinstruction ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t) = (t `at` i)

runrecur ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | e == -1 = do return ()
        | e == -2 = do putStrLn "syntax/addressfehler"
        | i == (lengthI t) = do putStrLn "kein end-statement"
        | i >= (lengthI t) = do putStrLn "syntax/addressfehler"
        | otherwise = do 
                newst <- doio ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
                let n = doinstruction newst (newinstruction newst)
                runrecur n

trd (_,_,s) = s

doio ((i,a,r1,r2,r3,r4,r5,e,(io,s),rs,vs),t) 
        | io == (True,'p') = do 
                                putStr s
                                return ((i,a,r1,r2,r3,r4,r5,e,((False,'p'),""),rs,vs),t)
        | io == (True,'g') = do
                                s <- getChar
                                return ((i,(fromIntegral (ord s)),r1,r2,r3,r4,r5,e,((False,'p'),""),rs,vs),t)
        | io == (True,'i') = do
                                s <- getLine
                                return ((i,(stoi s),r1,r2,r3,r4,r5,e,((False,'p'),""),rs,vs),t)
        | otherwise = do return ((i,a,r1,r2,r3,r4,r5,e,((False,'p'),""),rs,vs),t)

findnum i t p = findsnd i p . map (\(n,a) -> (n,(stoi a)))  . filter (\(n,a) -> isnumber a) . zip nat . map fst $ t

unjust (Just a) = a

restoreregisters ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t) (Just (ar,r1r,r2r,r3r,r4r,r5r)) = ((i,ar,r1r,r2r,r3r,r4r,r5r,e,io,rs,vs),t)  
restoreregisters ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t) Nothing = ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)  

doinstruction ((i,a,r1,r2,r3,r4,r5,e,io,rs,vs),t) (s,p)
-- noop 
        | s == "nop" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- end the programm
        | s == "end" = ((i,a,r1,r2,r3,r4,r5,-1,io,rs,vs),t)
-- noop on lable
        | isnumber s = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- io
        | s == "io" && p == 1 = (((i+1),a,r1,r2,r3,r4,r5,e,((True,'p'),show a),rs,vs),t)
        | s == "io" && p == 2 = (((i+1),a,r1,r2,r3,r4,r5,e,((True,'p'),[toascii a]),rs,vs),t)
        | s == "io" && p == 3 = (((i+1),a,r1,r2,r3,r4,r5,e,((True,'g'),""),rs,vs),t)
        | s == "io" && p == 4 = (((i+1),a,r1,r2,r3,r4,r5,e,((True,'i'),""),rs,vs),t)
-- unconditional lable jump
        | s == "l" = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- conditional lable jumps

        | s == "lge" && a >= 0 = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lge" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lgt" && a > 0 = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lgt" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lle" && a <= 0 = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lle" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "llt" && a < 0 = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "llt" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "leq" && a == 0 = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "leq" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lne" && a /= 0 = (((findnum (i+1) t p),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "lne" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- unconditional address jump
        | s == "jump" = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- conditional address jumps
        | s == "jge" && a >= 0 = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jge" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jgt" && a > 0 = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jgt" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jle" && a <= 0 = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jle" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jlt" && a < 0 = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jlt" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jeq" && a == 0 = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jeq" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jne" && a /= 0 = ((p,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "jne" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- register load
        | s == "load" && p == 0 = (((i+1),i,r1,r2,r3,r4,r5,e,io,rs,vs),t) 
        | s == "load" && p == 1 = (((i+1),r1,r1,r2,r3,r4,r5,e,io,rs,vs),t) 
        | s == "load" && p == 2 = (((i+1),r2,r1,r2,r3,r4,r5,e,io,rs,vs),t) 
        | s == "load" && p == 3 = (((i+1),r3,r1,r2,r3,r4,r5,e,io,rs,vs),t) 
        | s == "load" && p == 4 = (((i+1),r4,r1,r2,r3,r4,r5,e,io,rs,vs),t) 
        | s == "load" && p == 5 = (((i+1),r5,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- direct load
        | s == "dload" = (((i+1),p,r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- store
        | s == "store" && p == 0 = ((a,a,r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "store" && p == 1 = (((i+1),a,a,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "store" && p == 2 = (((i+1),a,r1,a,r3,r4,r5,e,io,rs,vs),t)
        | s == "store" && p == 3 = (((i+1),a,r1,r2,a,r4,r5,e,io,rs,vs),t)
        | s == "store" && p == 4 = (((i+1),a,r1,r2,r3,a,r5,e,io,rs,vs),t)
        | s == "store" && p == 5 = (((i+1),a,r1,r2,r3,r4,a,e,io,rs,vs),t)
-- math
        | s == "inc" = (((i+1),(a+1),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "dec" = (((i+1),(a-1),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "add" && p == 0 = (((i+1),(a+i),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "add" && p == 1 = (((i+1),(a+r1),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "add" && p == 2 = (((i+1),(a+r2),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "add" && p == 3 = (((i+1),(a+r3),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "add" && p == 4 = (((i+1),(a+r4),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "add" && p == 5 = (((i+1),(a+r5),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "sub" && p == 0 = (((i+1),(a-i),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "sub" && p == 1 = (((i+1),(a-r1),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "sub" && p == 2 = (((i+1),(a-r2),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "sub" && p == 3 = (((i+1),(a-r3),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "sub" && p == 4 = (((i+1),(a-r4),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "sub" && p == 5 = (((i+1),(a-r5),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "mult" && p == 0 = (((i+1),(a*i),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "mult" && p == 1 = (((i+1),(a*r1),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "mult" && p == 2 = (((i+1),(a*r2),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "mult" && p == 3 = (((i+1),(a*r3),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "mult" && p == 4 = (((i+1),(a*r4),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "mult" && p == 5 = (((i+1),(a*r5),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "div" && p == 0 = (((i+1),(a `div` i),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "div" && p == 1 = (((i+1),(a `div` r1),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "div" && p == 2 = (((i+1),(a `div` r2),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "div" && p == 3 = (((i+1),(a `div` r3),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "div" && p == 4 = (((i+1),(a `div` r4),r1,r2,r3,r4,r5,e,io,rs,vs),t)
        | s == "div" && p == 5 = (((i+1),(a `div` r5),r1,r2,r3,r4,r5,e,io,rs,vs),t)
-- push 
        | s == "pushv" = (((i+1),a,r1,r2,r3,r4,r5,e,io,rs,(S a vs)),t)
        | s == "pushr" = (((i+1),a,r1,r2,r3,r4,r5,e,io,(S (a,r1,r2,r3,r4,r5) rs),vs),t)
-- pop
        | s == "popv" && vs /= Empty = (((i+1),(unjust (pop vs)),r1,r2,r3,r4,r5,e,io,rs,(popr vs)),t)
        | s == "popr" && rs /= Empty = restoreregisters (((i+1),p,r1,r2,r3,r4,r5,e,io,(popr rs),vs),t) (pop rs)
-- error
        | otherwise = ((i,a,r1,r2,r3,r4,r5,-2,io,rs,vs), t)


