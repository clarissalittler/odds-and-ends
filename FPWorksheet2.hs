module Main where

import SOE
import Shape
import Region
import Fal
import Draw
import Animation

skyRegion = Complement Empty


sky = Region Cyan skyRegion
house = Region Red houseRegion
garage = Region Yellow garageRegion
roof = Region Magenta roofRegion

housePict = roof `Over` house `Over` garage `Over` sky 

prob1:: IO()
prob1 = draw "house" housePict

recolor :: Picture -> [(Color,Color)] -> Picture
recolor (Region c r) m = case lookup c m of
                           Nothing -> Region c r
                           Just c' -> Region c' r
recolor EmptyPic m = EmptyPic
recolor (p1 `Over` p2) m = (recolor p1 m) `Over` (recolor p2 m)

prob2:: IO()
prob2 = undefined

prob3:: IO()
prob3 = undefined

prob4:: IO()
prob4 = undefined

main =
  do { putStrLn "Enter an integer to choose a demo."
     ; n <- readLn
     ; [prob1, prob2, prob3, prob4 ] !! n
     }
