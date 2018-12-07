import Data.Set as S
import Data.Either
import Data.Either.Combinators (rightToMaybe)
import Data.List
import Control.Monad (join)

main = do
    text <- readFile "day1.txt"
    print $ solve text

solve text = 
    let
        jumps = fmap parse . words $ text

        -- Calculate all frequencies by summing their jumps, circularly.
        frequencies = scanl (+) 0 $ cycle jumps
    in 
        (sum jumps, firstDuplicate frequencies)

flatten :: Maybe (Either a b) -> Maybe b
flatten = join . fmap rightToMaybe

parse :: String -> Int
parse s@(x:xs) = read $ (if x == '+' then xs else s)

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = flatten . find isRight . scanl noDupInsert (Left S.empty)

-- If this number was already in the set, return it (wrapped in Right).
-- Otherwise, update and return the set (wrapped in Left).
-- If there was already 
noDupInsert :: Ord a => Either (Set a) a -> a -> Either (Set a) a
noDupInsert outcome num = 
    case outcome of
        Left set -> 
            if member num set 
                then (Right num) 
                else (Left $S.insert num set)
        Right x -> Right x