module Main where

import Prelude
import Effect
import Effect.Console

f :: String -> Effect Unit
f x = case x of "foo" -> log "cool"
                "bar" -> log "not cool"
                _ -> log "hmm"

main :: Effect Unit
main = do f "foo"
          f "bar"
          f "fo"
          f "fooo"
          f "fo@"
          f "gfdgdf"
