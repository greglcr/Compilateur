module Main where

import Prelude
import Effect
import Effect.Console

f :: Int -> Int
f x =
  if x > 5 then
    let z = 4 + 2 in
    if z < 5 then
      0
    else
      0 - x
  else
    f (x + 1)

main :: Effect Unit
main = do (let y = f 5 in log (show (y + y)))
          pure unit
