-- | find colouring without complete subgraphs
-- example usage: ./dist/build/Ramsey/Ramsey 3 3 3 64
-- last number is size of graph,
-- earlier numbers are sizes of forbidden cliques

{-# language PatternSignatures #-}

import Prelude hiding ( not, and, or )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting

import qualified Satchmo.Binary as B

import Satchmo.Solver.Minisat

import Data.List (sort, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM )
import System.Environment
import Data.Ix ( range)


main :: IO ()
main = do
    argv <- getArgs
    let ns = map read argv
        cs = init ns 
        n = last ns
    Just ( fs :: [ A.Array (Int,Int) Bool ] ) 
           <- solve $ ramsey cs n
    putStrLn $ unlines $ do
         f <- fs
         let ((u,l),(o,r)) = A.bounds f
         x <- [ u .. o ]
         return $ unwords $ do 
             y <- [ l .. r ]
             return $ if f A.! (x,y) 
                      then "* " else ". "

fill k cs = replicate (k - length cs) ' ' ++ cs

ramsey (cs :: [Int]) (n :: Int) = do
    fs <- forM cs $ \ c -> 
         relation ((1 :: Int,1 :: Int),(n,n))
    forM [ 1 .. n ] $ \ x -> 
        forM [ x + 1 .. n ] $ \ y -> 
            assertM $ exactly 1 $ 
                for fs $ \ f -> f ! (x,y) 
    forM ( zip cs fs ) $ \ (c,f) -> 
        forM ( cliques c [1..n] ) $ \ xs ->
            assert $ map not $ do
                x : ys <- tails xs ; y <- ys
                return $ not $ f ! (x,y)
    return $ forM fs decode
    
cliques 0 _ = return []
cliques k [] = []
cliques k (x:xs) = 
      cliques k xs ++ map (x:) ( cliques (k-1) xs)

for = flip map

assertM this = do x <- this ; assert [x]