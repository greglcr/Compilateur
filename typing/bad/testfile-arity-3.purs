module Main where
import Prelude
import Effect
import Effect.Console


f:: Int -> String
f x = "a"
main :: Effect Unit
main = log (f 1 2)
