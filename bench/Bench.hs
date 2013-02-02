module Main where

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = defaultMain [
         bench "fib 10" $ nf fib 10
       , bench "fib 30" $ nf fib 30
       , bench "fib 35" $ nf fib 35
       ]
