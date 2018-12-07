import Data.Map.Counter
import Data.Map.Strict (toList, Map)
import Data.List as L (find)
import Data.Maybe (isJust)

main = do
    text <- readFile "day2.txt"
    print $ solve text

solve inputText =
    let
        boxes = words inputText
        q1 = (numRepeatedChars 2 boxes) * (numRepeatedChars 2 boxes)
        pairs = (,) <$> boxes <*> boxes
        q2 = fmap sames . find (\p -> (diffs p) == 1) $ pairs
    in
        (q1, q2)

counts :: (Ord a) => [a] -> Map a Int
-- Count how many times each element appears in the list.
counts = getCounts . mconcat . map mkCounter 

numRepeatedChars :: (Ord a) => Int -> [[a]] -> Int 
-- Given a list of lists, find how many sublists contain n equal values.
numRepeatedChars n = length . filter (has n) . map counts
    where
        has n = isJust . find (== n) . fmap snd . toList

diffs :: Eq a => ([a], [a]) -> Integer
-- How many times are the characters at each index different in each list? 
diffs = _diffs 0
    where
        _diffs n ((x:xs), (y:ys)) = _diffs (if x==y then n else n+1) (xs, ys)
        _diffs n ([], []) = n

sames :: Eq a => ([a], [a]) -> [a]
-- Return the first list, filtering out any chars that don't match the second list. 
sames = reverse . _sames []
    where
        _sames out ([], []) = out
        _sames out ((x:xs), (y:ys)) = _sames (if x==y then x:out else out) (xs, ys) 

in1 = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab"
in2 = "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz"
