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
        x :: Int,
        y :: Int,
        hs :: Int,
        vs :: Int,
        f :: Int,
        r :: Int,
        p :: Int
    }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Read init information from standard input, if any
    n <- read <$> getLine
    positions <- replicateM n ((\ (x:y:_) -> Position x y) . map read . words <$> getLine)
    
    loop positions

rotation :: MarsLander -> Int
rotation _ = 0

power :: MarsLander -> Int
power ml
    | abs (vs ml) >= 40 = 4
    | otherwise = 0
    

loop :: [Position] -> IO ()
loop positions = do
    -- Read information from standard input
    marsLander <- (\ (x:y:hs:vs:f:r:p:_) -> MarsLander x y hs vs f r p) . map read .words <$> getLine
    
    -- Compute logic here
    
    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write action to standard output
    putStrLn $ show (rotation marsLander) ++ " " ++ show (power marsLander)
    
    loop positions
    
