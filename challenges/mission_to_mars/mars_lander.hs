import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import System.IO

data Position = Position 
    {
        px :: Int,
        py :: Int
    }

data MarsLander = MarsLander
    {
        x  :: Int,
        y :: Int,
        hs :: Int,
        vs :: Int,
        f :: Int,
        r :: Int,
        p :: Int
    }
    
data LandingSite = LandingSite
    {
        x1 :: Int,
        x2 :: Int,
        h  :: Int
    }
    
data Action = Action
    {
        rotation :: Int,
        power :: Int
    }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Read init information from standard input, if any
    n <- read <$> getLine
    positions <- replicateM n ((\ (x:y:_) -> Position x y) . map read . words <$> getLine)
    
    loop $ findLandingSite positions

findLandingSite :: [Position] -> LandingSite
findLandingSite (p1:p2:ps) 
    | py p1 == py p2  = LandingSite (px p1) (px p2) (py p1)
    | otherwise     = findLandingSite (p2:ps)


process :: LandingSite -> MarsLander -> Action
process ls ml = Action rotation power
    where rotation = 0
          power 
            | abs (vs ml) >= 40 = 4
            | otherwise = 0

loop :: LandingSite -> IO ()
loop ls = do
    -- Read information from standard input
    ml <- (\ (x:y:hs:vs:f:r:p:_) -> MarsLander x y hs vs f r p) . map read .words <$> getLine
    
    -- Compute logic here
    let action = process ls ml
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    putStrLn $ show (rotation action) ++ " " ++ show (power action)
    
    loop ls
    
