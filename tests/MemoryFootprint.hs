{-# language BangPatterns #-}

import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import GHC.DataSize
import Text.Printf
import Control.DeepSeq

sizes :: [Int]
sizes = [2^n | n <- [1 :: Int .. 11]]

main :: IO ()
main = do
    p "(Int, Int)" (1 :: Int, 2 :: Int)
    putStrLn ""
    p "HM Int Int (0)" (HM.empty :: HM.HashMap Int Int)
    p "HM Int Int (1)" (HM.singleton (1 :: Int, 42 :: Int))
    forM_ sizes $ \n -> pn "HM Int Int" n (HM.fromList (zip [0 :: Int .. (n - 1)] [0 :: Int ..]))
    putStrLn ""
    p "IM Int (0)" (IM.empty :: IM.IntMap Int)
    p "IM Int (1)" (IM.singleton (1 :: Int) (42 :: Int))
    forM_ sizes $ \n -> pn "IM Int" n (IM.fromList (zip [0 :: Int .. (n - 1)] [0 :: Int ..]))

    putStrLn ""
    p "(String, Int)" ("1", 2 :: Int)
    putStrLn ""
    p "HM String Int (0)" (HM.empty :: HM.HashMap String Int)
    p "HM String Int (1)" (HM.singleton ("1", 42 :: Int))
    forM_ sizes $ \n -> pn "HM String Int" n (HM.fromList (zip (map show [0 :: Int .. (n - 1)]) [0 :: Int ..]))
    putStrLn ""
    p "M String Int (0)" (M.empty :: M.Map String Int)
    p "M String Int (1)" (M.singleton ("1", 42 :: Int))
    forM_ sizes $ \n -> pn "M String Int" n (M.fromList (zip (map show [0 :: Int .. (n - 1)]) [0 :: Int ..]))

p :: NFData a => String -> a -> IO ()
p label x = do
    let !x' = force x
    printf (label ++ "\t%d\n") =<< recursiveSize x'

pn :: NFData a => String -> Int -> a -> IO ()
pn label n x = do
    let !x' = force x
    s <- recursiveSize x'
    printf (label ++ "(" ++ show n ++ ")\t%d\t%d\n") s (div (fromIntegral s) n)