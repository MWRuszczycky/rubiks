module View
    ( renderGame
    ) where

import qualified Graphics.Gloss as G
import qualified Model          as M

renderGame :: M.Game -> G.Picture
renderGame g = ( renderFace . M.faceZneg . M.cube $ g ) <> renderSelected g

renderFace :: M.Face -> G.Picture
renderFace xs = G.pictures . map go $ indexed
    where indexed    = [ ( (xs !! i) !! j, i, j ) | i <- [0..2], j <- [0..2] ]
          go (x,r,c) = G.color (renderColor x)
                       $ renderCellFace G.rectangleSolid r c

renderSelected :: M.Game -> G.Picture
renderSelected g = maybe G.Blank go . M.selected $ g
    where go (r,c) = G.color G.white $ renderCellFace G.rectangleWire r c

renderCellFace :: (Float -> Float -> G.Picture) -> Int -> Int -> G.Picture
renderCellFace f r c = G.translate (go c) (go r) ( f 20 20 )
    where go u = fromIntegral $ 22 * (u - 1)

renderColor :: M.Color -> G.Color
renderColor M.Red    = G.makeColor 1.0 0.0 0.0 1.0
renderColor M.White  = G.makeColor 1.0 1.0 1.0 1.0
renderColor M.Yellow = G.makeColor 1.0 1.0 0.0 1.0
renderColor M.Green  = G.makeColor 0.0 1.0 0.0 1.0
renderColor M.Blue   = G.makeColor 0.0 0.0 1.0 1.0
renderColor M.Orange = G.makeColor 1.0 0.5 0.0 1.0
renderColor M.Hidden = G.makeColor 0.1 0.1 0.1 1.0
