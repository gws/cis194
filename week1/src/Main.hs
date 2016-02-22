module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi num src tmp dst = hanoi (num - 1) src dst tmp
                        ++ [(src,tmp)]
                        ++ hanoi (num - 1) dst tmp src

main :: IO ()
main = do
    print $ hanoi 2 "a" "b" "c"
