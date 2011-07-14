-- | compute spaceship for Conway's game of life, 
-- cf. http://www.conwaylife.com/wiki/Category:Oscillators
-- example usage: ./Spaceship 1 1 4 6
-- arguments are: distanceX, distanceY, max. period
-- width [, height, [number of life start cells]]

{-# language PatternSignatures #-}

import Prelude hiding ( not, or, and )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting

import Satchmo.Solver.Minisat

import Data.List (sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, void )
import System.Environment
import Data.Ix ( range, inRange )

main :: IO ()
main = void $ do
    argv <- getArgs
    Just gs <- case map read argv of
        [ dx, dy, p, w       ] -> solve $ glide dx dy p w w Nothing
        [ dx, dy, p, w, h    ] -> solve $ glide dx dy p w h Nothing
        [ dx, dy, p, w, h, c ] -> solve $ glide dx dy p w h $ Just c
    forM ( zip [ 0..  ] gs ) $ \ (t, g) -> do
        putStrLn $ unwords [ "time", show t ]
        printA g

printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

for = flip map

glide :: MonadSAT m
    => Int -> Int
    -> Int -> Int -> Int -> Maybe Int
    -> m ( Decoder [ A.Array (Int,Int) Bool ] )
glide dx dy p w h mc = do
    g0 <- relation ((1,1),(w,h))
    assert $ map snd $ assocs g0
    case mc of
         Just c -> monadic assert [ atmost c $ map snd $ assocs g0 ]
         Nothing -> return ()
    let handle 0 g = return [g]
        handle k g = do g' <- next g ; gs <- handle (k-1) g' ; return $ g : gs
    gs <- handle p g0 
    forM gs bordered

    ms <- forM ( tail gs ) $ \ h -> moved (dx,dy) ( head gs ) h
    assert $ ms

    return $ decode gs

equals r s = monadic and [ implies r s, implies s r ]

moved (dx,dy) g h = do
    f <- constant False
    let bnd @ ((l,o),(r,u)) = bounds g
        get g p = if inRange bnd p then g ! p else f
    monadic and $ for ( range bnd ) $ \ (x,y) -> do
        fun2 (==) ( get g (x,y) ) ( get h (x+dx, y+dy) )


bordered g = do
    let ((u,l),(d,r)) = bounds g
    forM [ u .. d ] $ \ x -> forM [ l  , r ] $ \ y -> assert [ not $ g!(x,y) ]
    forM [ u ,  d ] $ \ x -> forM [ l .. r ] $ \ y -> assert [ not $ g!(x,y) ]


next g = do
    f <- constant False
    let bnd = bounds g
    let neighbours (i,j) = do
            i' <- [ i-1, i, i+1 ]
            j' <- [ j-1, j, j+1 ]
            guard $ i /= i' || j /= j'
            return $ if inRange bnd (i',j') 
               then g ! (i', j')
               else f
    pairs <- forM ( assocs g ) $ \ (p, x) -> do
        y <- step x $ neighbours p
        return (p, y)
    return $ build bnd pairs

step x xs = do
    cs <- counts 3 xs
    keep <- and [ x, cs !! 2 ]
    let birth = cs !! 3
    or [ keep, birth ]
    

-- | output !! k  == True
-- if exactly  k  of the inputs are True
counts :: MonadSAT m
       => Int -> [ Boolean ] 
       -> m [ Boolean ]
counts w xs = do
    t <- constant True ; f <- constant False
    let handle cs x = do
           ds <- forM cs $ \ c -> boolean
           forM ( zip cs ds ) $ \ (c,d) -> do
               assert_fun3 ( \ c d x -> Prelude.not x <= ( c == d ) ) c d x
           forM ( zip ( f : cs) ds ) $ \ (c,d) -> do
               assert_fun3 ( \ c d x -> x <= ( c == d ) ) c d x
           return ds
    foldM handle ( t : replicate w f ) xs

    

