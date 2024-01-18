module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (show (mod (-10) ( 0)))
  log (show (mod (-10) (-1)))
  log (show (mod (-10) ( 3)))
  log (show (mod ( 10) (-3)))
  log (show (mod (-10) (-3)))
  log (show (mod ( 10) ( 3)))
  log "Euclide contre la machine"
