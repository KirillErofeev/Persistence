module Visualize where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

vis = display FullScreen white $ rotate 33 (Polygon [(-37,-37), (17,17), (37,-43)])
playVis = playIO FullScreen black 70 0 (\x -> return $ scale 3 3 $ color red $ rotate (1488*x) (Polygon [(-37,-37), (17,17), (37,-43)])) ((return .) . flip const) (\x y -> return $ x+y) 

data Point = Point Int

--0, Dark, Dark2
