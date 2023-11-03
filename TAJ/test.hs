import System.Random

-- Generate a random integer in the range [min, max]
randomIntInRange :: Int -> Int -> IO Int
randomIntInRange min max = getStdRandom (randomR (min, max))

main :: IO ()
main = do
    randomNumber <- randomIntInRange 1 100
    putStrLn $ "Random number: " ++ show randomNumber
