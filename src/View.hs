module View
    ( renderGame
    ) where

import qualified Graphics.Gloss as G
import qualified Model          as M

renderGame :: M.Game -> G.Picture
renderGame g = G.pictures . map go $ squares
    where go = renderSquare ( rotYMat (M.theta g) )

renderSquare :: Matrix -> Square -> G.Picture
renderSquare r (Square f b ps n) = G.color (renderColor f)
                                   . G.polygon
                                   . project 250
                                   . rotate r $ ps

project :: Float -> Path3D -> G.Path
project d = map go
    where go (x,y,z) | z > d     = (0, 0)
                     | otherwise = ( x * u, y * u )
                     where u = 1 - z / d

squares :: [Square]
squares = [ makeSquare M.Red M.Hidden noRot (0, 0, 60)
          , makeSquare M.Green M.Hidden ( rotYMat (pi / 2) ) (-20, 0, 40)
          ]

makeSquare :: M.Color -> M.Color -> Matrix -> Vec3 -> Square
makeSquare f b r t = Square f b (translate t . rotate r $ ps) (mvProd r dv)
    where dv = (0, 0, -1)
          ps = [ ( -20, -20, 0 )
               , (  20, -20, 0 )
               , (  20,  20, 0 )
               , ( -20,  20, 0 )
               ]

---------------------------------------------------------------------

renderGame' :: M.Game -> G.Picture
renderGame' g = ( renderFace . M.faceZneg . M.cube $ g ) <> renderSelected g

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
renderColor M.Red    = G.makeColor 0.78 0.15 0.10 1.0
renderColor M.White  = G.makeColor 0.75 0.75 0.75 1.0
renderColor M.Yellow = G.makeColor 1.00 0.85 0.34 1.0
renderColor M.Green  = G.makeColor 0.00 0.54 0.36 1.0
renderColor M.Blue   = G.makeColor 0.00 0.64 1.00 1.0
renderColor M.Orange = G.makeColor 1.00 0.38 0.00 1.0
renderColor M.Hidden = G.makeColor 0.10 0.10 0.10 1.0
