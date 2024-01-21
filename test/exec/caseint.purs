module Main where

import Prelude
import Effect
import Effect.Console

f :: Int -> Effect Unit
f x = case x of 0 -> log "a"
                3 -> log "b"
                5 -> log "c"
                9 -> log "d"
                _ -> log "e"

main :: Effect Unit
main = do f 0
          f 3
          f 5
          f 9
          f 2
          f 10
