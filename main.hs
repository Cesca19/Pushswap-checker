--
-- EPITECH PROJECT, 2021
-- pushswapchecker
-- File description:
-- main.hs
--

import System.Environment
import System.Exit
import Control.Monad
import Data.Char

myIsNum :: String  -> Bool
myIsNum "" = False
myIsNum (x:xs) | not (null xs) && isDigit x = myIsNum xs
               | not (null xs) && not (isDigit x) = False
               | null xs && isDigit x = True
               | null xs && not (isDigit x) = False 

readInt :: String -> Int
readInt x = read x::Int

checkerror :: [String] -> Bool
checkerror [] = True
checkerror (x:xs) | not (myIsNum x) && not(myIsNum(tail x)) = True
                  | (myIsNum x || myIsNum(tail x)) && not (null xs) =
                    checkerror xs
                  | (myIsNum x || myIsNum(tail x)) && null xs = False

myAppend :: [ a ] -> [ a ] -> [ a ]
myAppend [] l = l
myAppend l [] = l
myAppend (start:[]) (l) = start:l 
myAppend (start:end) (first:second) = start : myAppend end (first:second)

mySwap :: [Int] -> [Int]
mySwap [] = []
mySwap (a : xa) | length xa == 0 = [a]
mySwap (a : b : rest) | length rest >= 0 = b:a:rest

sa :: ([Int], [Int]) -> ([Int], [Int])
sa (a, b) = (mySwap a, b)

sb :: ([Int], [Int]) -> ([Int], [Int])
sb (a, b) = (a, mySwap b)

sc :: ([Int], [Int]) -> ([Int], [Int])
sc (a, b) = (mySwap a, mySwap b)

pb::([Int], [Int]) -> ([Int], [Int])
pb ([], l) = ([], l)
pb ((x:xs), []) = (xs, [x])
pb ((x:[]), l) = ([], x:l)
pb ((x:xs) , l) = (xs, x:l)

pa::([Int], [Int]) -> ([Int], [Int])
pa (l, []) = (l, [])
pa (l, x:[]) = (x:l , [])
pa (l, (x:xs)) = (x:l , xs)

ra::([Int], [Int]) -> ([Int], [Int])
ra ([], l) = ([], l)
ra ((x:xs), l) = ( myAppend xs [x], l)

rb::([Int], [Int]) -> ([Int], [Int])
rb (l, []) = (l, [])
rb (l, x:xs) = ( l, myAppend xs [x])

rr::([Int], [Int]) -> ([Int], [Int])
rr (la, lb) = rb (ra (la, lb))

rra::([Int], [Int]) -> ([Int], [Int])
rra ([], lb) = ([], lb)
rra (la, lb) = ((myAppend [last la] (init la) ), lb )

rrb::([Int], [Int]) -> ([Int], [Int])
rrb (la, []) = (la, [])
rrb (la, lb) = (la ,(myAppend [last lb] (init lb) ))

rrr::([Int], [Int]) -> ([Int], [Int])
rrr (la, lb) = rrb (rra (la, lb))

lauchRaRrr :: String -> ([Int], [Int]) -> ([Int], [Int])
lauchRaRrr "ra" (la, lb) = ra (la, lb)
lauchRaRrr "rb" (la, lb) = rb (la, lb)
lauchRaRrr "rr" (la, lb) = rb (la, lb)
lauchRaRrr "rra" (la, lb) = rra (la, lb)
lauchRaRrr "rrb" (la, lb) = rrb (la, lb)
lauchRaRrr "rrr" (la, lb) = rrr (la, lb)
lauchRaRrr command (la, lb) = (la, lb)

lauchOperations :: String -> ([Int], [Int]) -> ([Int], [Int])
lauchOperations "sa" (la, lb) = sa (la, lb)
lauchOperations "sb" (la, lb) = sb (la, lb)
lauchOperations "sc" (la, lb) = sc (la, lb)
lauchOperations "pa" (la, lb) = pa (la, lb)
lauchOperations "pb" (la, lb) = pb (la, lb)
lauchOperations command (la, lb) = lauchRaRrr command (la, lb)

pushswapchecker :: [String] -> ([Int], [Int]) -> ([Int], [Int])
pushswapchecker [] l = l
pushswapchecker (c:[]) l = lauchOperations c l 
pushswapchecker (c:xc) (la, lb) = let sah = (la, lb) in
    pushswapchecker xc (lauchOperations c sah)

isSorted::[Int] -> Bool
isSorted (x:[]) = True
isSorted (x:xs) | x > (head xs) = False
                | otherwise = isSorted xs

checkList::([Int], [Int]) -> IO ()
checkList (la, []) | isSorted la = putStrLn "OK"
                    | otherwise = putStr  "KO: " >>
                        print (la)
checkList (la, lb) = putStr "KO: " >>
                    print (la, lb)

checkCommand:: [String] -> Bool
checkCommand [] = True
checkCommand (x:[]) | not (elem x l) = False
                    | otherwise = True
                    where l = ["sa", "sb", "sc", "pa", "pb", "ra", "rb", "rra"
                            ,"rr", "rrb", "rrr"] 
checkCommand (x:xs) | not (elem x l) = False 
                | otherwise  = checkCommand xs
                where l = ["sa", "sb", "sc", "pa", "pb", "ra", "rb", "rra"
                            ,"rr", "rrb", "rrr"] 

main :: IO ()
main = do
    args <- getArgs
    pushSwap <- getLine
    let operation = words pushSwap in
        if (checkerror args) || not (checkCommand operation)
            then exitWith (ExitFailure 84)
        else
            checkList (pushswapchecker operation (map readInt args, []))