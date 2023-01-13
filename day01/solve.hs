import Data.List
import System.Environment

type Calorie = Int
type CalorieList = [Calorie]

main = do
    input <- getContents
    let calorieLists = parseInput input :: [CalorieList]

    putStrLn $ ("Part 1: " ++) $ show $ solvePart1 calorieLists
    putStrLn $ ("Part 2: " ++) $ show $ solvePart2 calorieLists

parseInput :: String -> [CalorieList]
parseInput = convertIntoCalorieList . splitOn "" . lines

solvePart1 :: [CalorieList] -> Calorie
solvePart1 = maximum . sumCaloriesByList

solvePart2 :: [CalorieList] -> Calorie
solvePart2 = sum . take 3 . sortBy (\x y -> compare y x) . sumCaloriesByList

convertIntoCalorieList :: [[String]] -> [CalorieList]
convertIntoCalorieList = map $ map read

sumCaloriesByList :: [CalorieList] -> [Calorie]
sumCaloriesByList = map sum

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x xs = l : case r of
    []     -> []
    _ : r' -> splitOn x r'
    where (l, r) = break (== x) xs
