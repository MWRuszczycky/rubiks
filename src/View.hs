module View
    ( renderGame
    ) where

import qualified Graphics.Gloss as G
import qualified Model          as M

renderGame :: M.Game -> G.Picture
renderGame = G.pictures . renderFace . M.faceZneg . M.cube

renderFace :: M.Face -> [ G.Picture ]
renderFace xs = map go [ ( (xs !! i) !! j, i, j ) | i <- [0..2], j <- [0..2] ]
    where go (x,r,c) = G.color (renderColor x) $ renderCellFace r c

renderCellFace :: Int -> Int -> G.Picture
renderCellFace r c = G.translate (go r) (go c) ( G.rectangleSolid 20 20 )
    where go u = fromIntegral $ 22 * (u - 1)

renderColor :: M.Color -> G.Color
renderColor M.Red    = G.makeColor 1.0 0.0 0.0 1.0
renderColor M.White  = G.makeColor 1.0 1.0 1.0 1.0
renderColor M.Yellow = G.makeColor 1.0 1.0 0.0 1.0
renderColor M.Green  = G.makeColor 0.0 1.0 0.0 1.0
renderColor M.Blue   = G.makeColor 0.0 0.0 1.0 1.0
renderColor M.Orange = G.makeColor 1.0 0.5 0.0 1.0
renderColor M.Hidden = G.makeColor 0.1 0.1 0.1 1.0
