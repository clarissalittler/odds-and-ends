module Main where

import Prelude hiding ((*>),(<*))
import SOE
import Shape
import Region
import Picture hiding (main)
import Fal
import Draw
import Animation hiding (Behavior, lift1, lift2, (<*), (>*), yellow, ell, translate, over)

-- just a little helper for making simple isoceles triangles that have the top vertex placed halfway between
-- the bottom side and the sides are the same size
isop c bottomLength height = Region c $ Shape $ Polygon [(-bottomLength / 2,0), 
                                                         (0,height), 
                                                         (bottomLength / 2, 0)]
                                               
roof =  (isop Blue 2 0.5)
        `Over` (isop Green 2 0.75)
        `Over` (isop White 2 1)

transp :: Float -> Float -> Picture -> Picture
transp x y (p1 `Over` p2) = (transp x y p1) `Over` (transp x y p2)
transp x y EmptyPic = EmptyPic
transp x y (Region c r) = Region c (Translate (x,y) r)

scalep :: Float -> Float -> Picture -> Picture
scalep x y (p1 `Over` p2) = (scalep x y p1) `Over` (scalep x y p2)
scalep x y EmptyPic = EmptyPic
scalep x y (Region c r) = Region c (Scale (x,y) r)

rectPict :: Color -> Float -> Float -> Picture
rectPict c x y = Region c (Shape $ Rectangle x y)

background = (Region Yellow $ Translate (1.5,1.5) $ Shape $ Ellipse 0.1 0.1) 
             `Over` (transp 0 (-1.5) $ rectPict Green 6 3) 
             `Over` Region Cyan (Complement Empty)
house = (transp (-0.3) (0.5) $ rectPict Blue 0.2 0.2 )
        `Over` (transp 0.3 0.5 $ rectPict Blue 0.2 0.2)
        `Over` (transp 0 (-0.5) $ rectPict Blue 0.5 1)
        `Over` (rectPict Red 2 2)

garage = (rectPict White 1 1) `Over` (transp 0 0.5 $ scalep 0.5 0.5 roof) 

housePict = (transp 0 1 roof) `Over` (transp (-1.5) (-0.5) garage) `Over` house `Over` background

prob1:: IO()
prob1 = draw "house" housePict

recolor :: Picture -> [(Color,Color)] -> Picture
recolor (Region c r) m = case lookup c m of
                           Nothing -> case lookup c m' of
                                        Nothing -> Region c r
                                        Just c' -> Region c' r
                           Just c' -> Region c' r
    where m' = map flippant m
recolor EmptyPic m = EmptyPic
recolor (p1 `Over` p2) m = (recolor p1 m) `Over` (recolor p2 m)

flippant :: (a, b) -> (b, a)
flippant (x,y) = (y,x) -- did I miss this in the standard library?


prob2:: IO()
prob2 = draw "house" $ recolor housePict [(Red,Green) ,(Blue,Magenta), (Yellow, Black)]

varyColor :: Animation Color
varyColor t = [Red,Green,Blue,Yellow] !! ((floor t) `mod` 4)

regularP :: Int -> [(Float,Float)]
regularP n = let interval = (2*pi) / (fromIntegral n)
             in [(cos r, sin r) | r <- (map (\x -> fromIntegral x * interval) [0..n-1])]

doofyPicture t = Region (varyColor t) $ Scale (cos (t/10), cos (t/10)) 
                 $ Shape $ Polygon $ map (\(x,y) -> (2*x,2*y)) (regularP 5)

prob3:: IO ()
prob3 = animate "Doofy Shape" $  picToGraphic . doofyPicture

picToG = lift1 picToGraphic
overG = lift2 overGraphic

pball' :: Float -> Behavior Graphic
pball' vel = 
    let xvel    = vel `stepAccum` xbounce ->> negate
        xpos    = integral xvel
        xbounce = when (xpos >*  2 ||* xpos <* -2)
        yvel    = vel `stepAccum` ybounce ->> negate
        ypos    = integral yvel
        ybounce = when (ypos >* 1.5) .|. paddlebounce
        paddlebounce =  when (ypos      `between` (-2.0,-1.5) &&*
                              fst mouse `between` (xpos-0.25,xpos+0.25))
        score = lift1 (text (100,200) . show) $ stepAccum 0 (paddlebounce =>> (const (+1)))
    in picToG (paint yellow (translate (xpos, ypos) (ell 0.2 0.2))) `overG`
       score

paddleball' :: Float -> Behavior Graphic 
paddleball' vel = let scenery = picToG (walls `over` paddle)
                  in scenery `overG` (pball' vel) 

prob4:: IO()
prob4 = reactimate "Ball" (paddleball' 3)

varyColorB :: Behavior Color
varyColorB = Behavior $ \(_, ts) -> map (\t -> [Red,Green,Blue,Yellow] !! ((floor t) `mod` 4)) ts

fixedPoints :: Behavior [(Float,Float)]
fixedPoints = Behavior $ \(_, ts) -> map (\t -> map (\(x,y) -> (2*(cos (t/10))*x, 2*(cos (t/10))*y)) (tail $ regularP 5)) ts

doofyPicture' = let totalPoints = lift2 (:) mouse' fixedPoints
                    mouse' = lift2 (,) (fst mouse) (snd mouse)
                in paint varyColorB (lift1 (Shape . Polygon) totalPoints)

prob5 :: IO ()
prob5 = test doofyPicture'

main =
  do { putStrLn "Enter an integer to choose a demo."
     ; n <- readLn
     ; [prob1, prob2, prob3, prob4, prob5] !! n
     }
