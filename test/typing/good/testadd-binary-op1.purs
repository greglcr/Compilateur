module Main where

import Prelude
import Effect
import Effect.Console

add :: Int
add = 5 + 3

mul :: Int
mul = 2 * 9

div :: Int
div = 9 / 2

minus :: Int
minus = 6 - 3

un_min :: Int
un_min = -6

or :: Boolean
or = true || false

and :: Boolean
and = true && false

is_equal1 :: Boolean
is_equal1 = 5 == 3

is_equal2 :: Boolean
is_equal2 = true == false

is_equal3 :: Boolean
is_equal3 = "a" == "b"

is_diff1 :: Boolean
is_diff1 = 5 /= 3

is_diff2 :: Boolean
is_diff2 = true /= false

is_diff3 :: Boolean
is_diff3 = "a" /= "b"

is_strictly_bigger :: Boolean
is_strictly_bigger = 5 > 3

is_strictly_lower :: Boolean
is_strictly_lower = 5 < 3

is_bigger :: Boolean
is_bigger = 5 >= 3

is_lower :: Boolean
is_lower = 5 <= 3

concat :: String
concat = "akdj" <> "qrkc"

main :: Effect Unit
main = log ""

